#---- Chargements ----
library("data.table") #together forever...
library(udpipe)
library(readr)
library(tidyverse)
#library(stringr)
library(hyphenatr)
library(utf8)
library(quanteda)

load("mega_lexique.rda")

#---- constantes ----
upos.jamais.rares <- c("PUNCT", "DET", "ADP", "PRON", "AUX",
                       "PART", "NUM", "SYM", "PROPN", "X", NA)
udmodel_french <-
  udpipe_load_model(file = "french-gsd-ud-2.3-181115.udpipe") #chargement du modèle linguistique
load("./mega_lexique.rda") #chargement de la méga db lexicale
switch_dict("fr") # dico de syllabes

dossier <- "./txt_parag"

#----fonctions----

#correspond à ce qui était dans le main dans la version console d'ALAIN.
MainStuff <- function() {
  txt <- ConstituerCorpus(dossier)  # lit les fichiers txt qui sont dans le dossier
  parsed.txt <- ParserTexte(txt)  # analyse lexicale avec udpipe, pourrait prendre quelques minutes...
  parsed.edit <- PostTraitementLexique(parsed.txt)
  parsed.freq <- CalculerFreq(parsed.edit)
  mots.rares <- FiltrerMotsRares(parsed.freq) #seuil.livre = 40, seuil.film = 40, 
  #seuil.manulex = 20, seuil.eqol = 5
  mots.rares.severe <-
    FiltrerMotsRares(
      parsed.freq,
      seuil.livre = 20,
      seuil.film = 20,
      seuil.eqol = 2.5,
      seuil.manulex = 10
    )
  mots.rares.laxe <-
    FiltrerMotsRares(
      parsed.freq,
      seuil.livre = 80,
      seuil.film = 80,
      seuil.eqol = 10,
      seuil.manulex = 40
    )
  
  sommaire.corpus <- CalculerFog(parsed.freq)
  
  sommaire.doc <- sommaire.corpus$doc
  new.fog <- textstat_readability(txt$text, measure = c("Strain", "FOG"))
  sommaire.doc <-
    cbind(sommaire.corpus$doc,
          strain.syll = new.fog$Strain,
          fog.syll = new.fog$FOG)
  sommaire.doc %>% mutate_at(2:21, round, 3) -> sommaire.doc
  
  #et là il resterait à afficher ce tableau d'une manière glorieuse
}
  
  
ConstituerCorpus <- function (dossier) {
  #TODO: explorer une solution avec quanteda qui permettrait aussi de prendre un corpus en .zip
  #dossier = "./txt_parag_q"
  if (dir.exists(dossier)) {  # dossier existe
    chemins <- list.files(path=dossier, full.names = TRUE) #liste des fichiers avec le "path"
    n <- length(chemins) #le nombre de textes dans le dossier 
  } else {
    if (file_test("-f", dossier)) {  # si c'est finalement un fichier
      chemins <- paste(getwd(), dossier, sep="/")
      n <- 1
    } else {
      cat("ConstituerCorpus | dossier ou fichier inexistant \n")
      return()
    }
  }
  mon.corpus <- data.table ( #initialisation
    doc_id = basename(chemins),
    text = ""
  )
  
  for (i in (1:n)) {
    contenu <- read_file(chemins[i])
    contenu <- utf8_normalize(contenu, map_quote = TRUE)
    contenu <- str_replace_all(contenu, "’", "'") #correction pour les apostrophes
    #gsub("/’", "/'", contenu, ignore.case = TRUE)
    mon.corpus[i, "text"] <- contenu
  }
  return (mon.corpus)
}

ParserTexte <- function(txt) {
  #txt <- str_replace_all(txt, "’", "'") #correction pour les apostrophes
  #note : on fait maintenant ce remplacement dans une autre fonction, ConstituerCorpus
  parsed <- udpipe(x=txt, object=udmodel_french, trace = TRUE) 
  parsed <- as.data.table(parsed)
  return(parsed)
}

PostTraitementLexique <- function(parsed.post) {
  #parsed.post = copy(parsed)
  
  #ajout d'un identifiant unique
  noms <- 1:nrow(parsed.post)
  parsed.post[, vraiTokenId := noms]
  
  #gestion des noms de fichiers
  parsed.post$doc_id <- str_remove_all(parsed.post$doc_id, ".txt")
  
  #gestion des tokens introduits par le parsing (ex: au parc devient à le parc)
  listeComptePas <- parsed.post[!is.na(token) & is.na(lemma), vraiTokenId]
  parsed.post$compte <- TRUE #par défaut, on compte le token
  #parsed.post[token_id %in% listeComptePas &
  #         upos %in% c("ADP", "DET"), compte := FALSE]
  parsed.post[upos %in% c("PUNCT", "SYM", "PART"), compte := FALSE]
  parsed.post[is.na(upos) & is.na(lemma)]
  
  #mots de contenu vs autres
  parsed.post$contenu <- TRUE
  parsed.post[upos %in% upos.jamais.rares, contenu := FALSE]
  
  #nettoyage des colonnes
  parsed.post$upos <- factor(parsed.post$upos)
  parsed.post <- parsed.post[, -c("start", "end", "xpos", "head_token_id", "dep_rel", "deps")]
  #par défaut les pron relatifs sont classifiés en adverbes, on les remet en PRON
  #pour qu'ils correspondent à la classification dans les db lexicales
  parsed.post[upos == "ADV" & feats == "PronType=Rel",
              upos := "PRON"]
  
  #tout en lowercase sauf PROPN
  parsed.post[upos != "PROPN", token := str_to_lower(token)]
  parsed.post[upos != "PROPN", lemma := str_to_lower(lemma)]
  #parsed.post$token <- sapply(parsed.post$token, tolower)
  
  #correction pour les doubles lettres
  parsed.post[, token := str_replace_all(token, "œ", "oe")]
  parsed.post[, lemma := str_replace_all(lemma, "œ", "oe")]
  
  #retrait des apostrophes et guillemets initiaux
  parsed.post[str_starts(token, "-") | str_starts(token, "'"),
              token := str_sub(token, 2)]
  
  #longueur des mots (char)
  #parsed.post$longueur.mot <- 0 
  #parsed.post[, longueur.mot := nchar(token)]
  
  #longueur des mots (syll)
  #parsed.post <- merge(parsed.post, mega.lexique[, .(token, upos, nb.syll)],
  #                        by=c("token", "upos")) #on va chercher les nb de syll dans mega.lexique
  #parsed.post$nb.syll <- as.integer(parsed.post$nb.syll)
  #parsed.post[1, nb.syll := NA] #pour tester, debug stuff
  #parsed.post$hyph <- as.character(NA)
  #parsed.post[is.na(nb.syll), hyph := hyphenate(token, "=")]
  #parsed.post[is.na(nb.syll), nb.syll := str_count(hyph, "=") + 1]
  
  #on ordonne en terminant...
  parsed.post <- parsed.post[order(doc_id, paragraph_id, sentence_id, term_id)]
  
  return(parsed.post)
}

MatcherLexemes <- function(lexemes, cles) {
  #lexemes = fusion.lexique[is.na(freq.eqol), -c("lemma")]
  #cles = c("token", "upos")
  fusion <- merge( 
    lexemes, mega.lexique,
    by = cles
  )
  
  fusion <- 
    fusion[order(vraiTokenId, freq.lexique.livre, freq.eqol)]
  fusion <- 
    fusion[!duplicated(fusion[, "vraiTokenId"])] 
  
  return(fusion)
}

CalculerFreq <- function(mon.lexique) {
  # retourne le même tableau avec des colonnes indiquant les fréq d'occurrence
  #mon.lexique.tout <- copy(parsed.edit)
  mon.lexique.tout <- copy(mon.lexique)
  mon.lexique <- mon.lexique.tout[compte == TRUE & upos != "NUM"] 
  
  #l'algo de fusion est un peu complexe, on cherche à combiner avec mega.lexique
  #en utilisant des critères de moins en moins sévères
  
  fusion.lexique <- MatcherLexemes(mon.lexique, c("token", "upos", "lemma") )
  no.match <- mon.lexique[!(vraiTokenId %in% fusion.lexique$vraiTokenId)] 
  fusion.token.laxe <- 
    MatcherLexemes(no.match[, -c("lemma")], 
                   c("token", "upos"))
  no.match <- no.match[!(vraiTokenId %in% fusion.token.laxe$vraiTokenId)] 
  fusion.lemma <- MatcherLexemes(no.match[, -c("token")], c("lemma", "upos") )
  no.match <- no.match[!(vraiTokenId %in% fusion.lemma$vraiTokenId)] 
  fusion.lemma.laxe <- 
    MatcherLexemes(no.match[, -c("token", "upos")], c("lemma") ) 
  no.match <- no.match[!(vraiTokenId %in% fusion.lemma.laxe$vraiTokenId)] 
  fusion.token.tres.laxe <- 
    MatcherLexemes(no.match[, -c("lemma", "upos")], c("token") ) 
  no.match <- no.match[!(vraiTokenId %in% fusion.token.tres.laxe$vraiTokenId)]
  
  dt.freq <-
    rbind(fusion.lexique,
          fusion.token.laxe,
          fusion.lemma,
          fusion.lemma.laxe,
          fusion.token.tres.laxe,
          no.match, fill = TRUE)
  rejets <- mon.lexique.tout[!(vraiTokenId %in% dt.freq$vraiTokenId)]
  dt.freq <- rbind(dt.freq, rejets, fill = TRUE)
  
  dt.freq <- dt.freq[order(vraiTokenId)] #on remet dans le bon ordre
  setkeyv(dt.freq, c("doc_id", "token", "upos"))
  
  #dt.freq[is.na(dt.freq) & is.numeric(dt.freq)] <- 0 #remplacement des NAs par des zéros
  dt.freq$quant.eqol <- dt.freq$quant.eqol %>% replace_na(0)
  dt.freq$quant.lexique.film <- dt.freq$quant.lexique.film %>% replace_na(0)
  dt.freq$quant.lexique.livre <- dt.freq$quant.lexique.livre %>% replace_na(0)
  dt.freq$quant.manulex <- dt.freq$quant.manulex %>% replace_na(0)
  
  return(dt.freq)
}

FiltrerMotsRares <- function(mon.lexique, seuil.livre = 40, seuil.film = 40, 
                             seuil.manulex = 20, seuil.eqol = 5) {
  #mon.lexique <- copy(parsed.freq)
  
  #ex: un mot très fréquent va typiquement avoir 90 de quant
  #    signifiant que 90% des mots sont moins populaires que ce mot
  # rares <- mon.lexique[!(upos %in% upos.jamais.rares) &
  #                        compte == TRUE &
  #                        quant.eqol < seuil.eqol &
  #                        (
  #                          quant.lexique.film < seuil.film |
  #                            quant.lexique.livre < seuil.livre |
  #                            quant.manulex < seuil.enfant
  #                        )]
  
  #ici j'ai fait un système à 4 juges.
  # si 3 ou 4 des 4 juges (4 db lexicales) jugent le mot comme rare, alors il est rare
  rares <- mon.lexique[!(upos %in% upos.jamais.rares) &
                         compte == TRUE &
                         ((quant.eqol < seuil.eqol) +
                            (quant.lexique.film < seuil.film) +
                            (quant.lexique.livre < seuil.livre) +
                            (quant.manulex < seuil.manulex)) > 2
                       ]
  
  mini.rares <- unique(rares[, .(
    lemma,
    paragraph_id,
    sentence_id,
    vraiTokenId,
    Eqol = round(max(quant.eqol), 2),
    Lexique_film = round(max(quant.lexique.film), 2),
    Lexique_livre = round(max(quant.lexique.livre), 2),
    Manulex = round(max(quant.manulex), 2)
  ), by = c("doc_id", "token", "upos")])
  
  mini.rares <-
    mini.rares[order(doc_id, paragraph_id, sentence_id, vraiTokenId)]
  return(mini.rares)
}

#---- caclcul de l'indice de Gunning et autres stats de parag ----

CompterMotsRares <- function (id, mode = "default") {
  #reçoit un tableau de tokens et retourne le nb de mots rares
  #id = parsed.gunning[doc_id=="sec1_organes.txt", vraiTokenId]
  if (mode == "severe" ) {
    db.rares <- mots.rares.severe
  } else {
    if (mode == "laxe") {
      db.rares <- mots.rares.laxe
    } else {
      db.rares <- mots.rares
    }
  } 
  rares <- id[id %in% db.rares$vraiTokenId]
  return(length(rares))
}

CompterMotsLongs <- function (token, nb.segments, seuil) {
  #seuil = 3
  #token <- parsed.gunning[sentence_id == 1, token]
  #nb.segments <- parsed.gunning[sentence_id == 1, nb.syll]
  longs <- nb.segments >= seuil
  return(length(token[longs]))
}

CompterVerbesConj <- function (upos, feats) {
  #upos <- parsed.gunning[sentence_id == 1, upos]
  #feats <- parsed.gunning[sentence_id == 1, feats]
  
  lexemes <- data.table(upos, feats)
  lexemes <- lexemes[upos == "VERB" & !(feats == "VerbForm=Inf")]
  return(nrow(lexemes))
}

CompterNDansDb <- function(freq, seuil = 0) {
  n <- freq[freq <= seuil ]
  return(length(n))
}

CalculerFog <- function(parsed) {
  #parsed <- copy(parsed.freq)
  parsed.gunning <- parsed[compte == TRUE & !(upos %in% c("PUNCT"))] 
  parsed.gunning[, quant.multi := quant.eqol + quant.lexique.livre + quant.lexique.film +
                   quant.manulex]
  parsed.gunning[, jugFreq := as.numeric(quant.eqol & quant.lexique.livre & 
                                           quant.lexique.film & quant.manulex)]
  
  # Stats phrase (qui peuvent uniquement être calculées au niveau de la phrase)
  stats.phrase <-
    parsed.gunning[, .(
      nMots = .N,
      nbVerbesConj = CompterVerbesConj(upos, feats),
      propMR = round(CompterMotsRares(vraiTokenId) / .N, 2)
    ), by = c("doc_id", "paragraph_id", "sentence_id")]
  stats.phrase[, fog:= (nMots * (propMR * 100)) * 0.4]
  stats.phrase <- stats.phrase[order(doc_id, paragraph_id, sentence_id)]
  
  #stats par paragraphe
  stats.parag <- parsed.gunning[, .(
    nMots = .N,
    nbPhrase = uniqueN(sentence_id),
    nMR = CompterMotsRares(vraiTokenId),
    nMREqol = .N - CompterNDansDb(quant.eqol, 5),
    nMRLexLiv = .N - CompterNDansDb(quant.lexique.livre, 40),
    nMRLexFil = .N - CompterNDansDb(quant.lexique.film, 40),
    nMRManu = .N - CompterNDansDb(quant.manulex, 30),
    nMLSyll = CompterMotsLongs(token, nb.syll, 3),
    nMLChar = CompterMotsLongs(token, longueur.mot, 8),
    freqMoy = mean(quant.multi, na.rm=T)
    # moyMotsPh = mean(nbMots
  ), by = c("doc_id", "paragraph_id")] #mots total et phrases dans le paragraphe
  
  stats.parag[, ':='( #ajout de proportions
    #pMLSyll = round(nMLSyll / nMots, 2),
    #pMLChar = round(nMLChar / nMots, 2),
    pMR = round(nMR / nMots, 2)
  )]
  stats.parag <- stats.parag[order(doc_id, paragraph_id)]
  
  #sommaire du corpus, par document (doc_id)
  # stats.doc <- stats.parag[, .(
  #   nMots = sum(nMots),
  #   nParag = uniqueN(paragraph_id),
  #   nbPhrase = sum(nbPhrase),
  #   nMR = sum(nMR),
  #   nMREqol = sum(nMREqol),
  #   nMRLexLiv = sum(nMRLexLiv),
  #   nMRLexFil = sum(nMRLexFil),
  #   nMRManu = sum(nMRManu),
  #   nMLSyll = sum(nMLSyll),
  #   nMLChar = sum(nMLChar)
  # ), by = c("doc_id")]
  # stats.doc[, ':='( #ajout de proportions, etc.
  #   pMLSyll = round(nMLSyll / nMots, 2),
  #   pMLChar = round(nMLChar / nMots, 2),
  #   pMR = round(nMR / nMots, 4),
  #   pMREqol = round(nMREqol / nMots, 4),
  #   pMRLexLiv = round(nMRLexLiv / nMots, 4),
  #   pMRLexFil = round(nMRLexFil / nMots, 4),
  #   pMRManu = round(nMRManu / nMots, 4),
  #   longMoyPh = round(nMots / nbPhrase, 2)
  # )]
  # stats.doc[, ':='( #ajout d'indices de lisibilité
  #   fogSyll = round((longMoyPh + (pMLSyll * 100)) * 0.4, 2),
  #   fogChar = round((longMoyPh + (pMLChar * 100)) * 0.4, 2),
  #   fogFreq = round((longMoyPh + (pMR * 100)) * 0.4, 2)
  # )]
  # 
  # #calcul alternatif pour les phrases
  # #ici la médiane de la longueur médiane des ph
  # indices.ph.doc <- stats.phrase[, .(
  #   longMedPh = median(nMots, na.rm = T)
  # ), by = "doc_id"]
  # 
  # stats.doc <- merge(stats.doc, indices.ph.doc, by = "doc_id")
  # 
  # stats.doc.freq <- parsed.gunning[!(upos %in% upos.jamais.rares), .(
  #   freqMedCont = round(median(quant.multi, na.rm = T), 2),
  #   jugFreqCont = round(mean(jugFreq, na.rm = T), 2)
  # ), by = "doc_id"]
  # stats.doc <- merge(stats.doc, stats.doc.freq, by = "doc_id")
  # stats.doc <- stats.doc[order(doc_id)]
  #   
  # stats pour doc, méthode alternative
  
  stats.doc <- parsed.gunning[, .(
    nMots = .N,
    nSyll = sum(nb.syll, na.rm=T),
    nPh = uniqueN(sentence_id),
    longMoyPh = .N / uniqueN(sentence_id),
    freqLexLiv = round(mean(quant.lexique.livre, na.rm=T), 2),
    freqLexFil = round(mean(quant.lexique.film, na.rm=T), 2),
    freqEqol = round(mean(quant.eqol, na.rm=T), 2),
    freqManu = round(mean(quant.manulex, na.rm=T), 2),
    nMR = CompterMotsRares(vraiTokenId),
    nMRSevere = CompterMotsRares(vraiTokenId, mode = "severe"),
    nMRLaxe = CompterMotsRares(vraiTokenId, mode = "laxe"),
    nMRLexLiv = CompterNDansDb(quant.lexique.livre, 35),
    totalFreq = sum(quant.lexique.livre)
  ), by = "doc_id"]
  stats.doc[, dale.chall.lex := 64 - (95 * (nMRLexLiv / nMots)) - (0.69 * longMoyPh)]
  stats.doc[, dale.chall.tout := 64 - (95 * (nMR / nMots)) - (0.69 * longMoyPh)]
  stats.doc[, strain := nSyll / (nPh / 3) / 10]
  stats.doc[, strain.alt := totalFreq / (nPh / 3) / 10]
  stats.doc[, ':='(syllMoyPh = nSyll / nPh,
                   pMRLexLiv = nMRLexLiv / nMots,
                   pMR = nMR / nMots,
                   pMRSevere = nMRSevere / nMots,
                   pMRLaxe = nMRLaxe / nMots)]
  
  # voir https://ogg.osu.edu/media/documents/health_lit/STRAIN%20INDEX.pdf
  
  #head(stats.doc)
  stats.doc.ph <- stats.phrase[, .(
    fog = mean(fog, na.rm=T)
  ), by = "doc_id"]
  stats.doc <- merge(stats.doc, stats.doc.ph, by = "doc_id")
  #formules: fog = (long moyenne phrase + pourc mot diff) * 0.4
  #          Flesch: 206.835 – (1.015 x ASL) – (84.6 x ASW)
  #          Flesch-Kincaid: (.39 x ASL) + (11.8 x ASW) – 15.59
  #          LIX: ASL + pourcentage de longs mots
  #          Coleman-Liau:  = 0.0588L - 0.296S - 15.8 (L est le nombre moy de lettre par 100 char, S est le moyen de phrases par 100 char)
  #          ARI: (4.71 * avg char par mot) + (0.5 * ASL) - 21.43
  #          Linsear Write: pour chaque 100 mots, (nb mots faciles + (3*nb mots diff)) / nbPhrases   ensuite si la rép > 20 on divise par 2, sinon on enlève 2 puis on divise par 2
  # ASW: av syll per word
  # ASL: av sentence length
  # SMOG: sqr(3 + nb mots longs)
  # Dale-Chall: 64 − (0.95 × 100 × (nMots / nMotsDifficiles) − (0.69 × ASL)
  
  # voir https://support.office.com/en-ie/article/test-your-document-s-readability-85b4969e-e80a-4777-8dd3-f7fc3c8b3fd2#__toc342546555
  #      http://www.readabilityformulas.com/the-LIX-readability-formula.php
  
  
  #stats.parag
  #View(stats.parag)
  #return(list(stats.doc = stats.doc, 
  #            stats.phrase = stats.phrase, 
  #            stats.parag = stats.parag))
  result <- list(
    ph = stats.phrase,
    parag = stats.parag,
    doc = stats.doc
  )
  return(result)
}

