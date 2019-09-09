library("data.table") #together forever...
library(udpipe)
library(readr)
library(tidyverse)
library(tokenizers) #pour les n-grams
library(stringr) #pour le str_count
library(hyphenatr) #pour nb de syll

library(DT) #pour tables dans shiny
library(shinycssloaders)


#dl <- udpipe_download_model(language = "french") #en commentaire car déjà téléchargé, rouler ça la 1ère fois
#voir https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-2898 pour les modèles

#pour le modèle ling fr, voir https://github.com/UniversalDependencies/UD_French-GSD/blob/master/README.md
default.input <- read_file("default.txt") #requiert readr

udmodel_french <- udpipe_load_model(file = "french-gsd-ud-2.3-181115.udpipe") #chargement du modèle linguistique

#compareParagSimple retourne le nb de lemmes identiques entre reference et parag ----
comparerParagSimple <- function(reference, parag) {
  #méthode naive, on regarde juste les mots qui sont communs
  ref.content <- reference[isContent == TRUE]
  parag.content <- parag[isContent == TRUE]
  
  nbMotTotal <- length(parag$token)
  u.reference <- unique(ref.content$lemma)
  u.parag <- unique(parag.content$lemma)
  nbOverlap <- length(intersect(u.reference, u.parag)) #retourne le nb d'overlaps
  nbMotUnique <- length(u.parag)
  
  resultat <- list(
    nbOverlap=nbOverlap, 
    nbMotContenu=nbMotUnique,
    nbMotTotal=nbMotTotal)
  return (resultat)
}

#comparerParagNgram comparaison de ngrams ----
comparerParagNgram <- function(ref.txt, compare.txt, n_max=12, n_min=2) {
  ref.ngrams <- tokenize_ngrams(ref.txt, n = n_max, n_min=n_min, simplify=TRUE)
  compare.ngrams <- tokenize_ngrams(compare.txt, n = n_max, n_min = n_min, simplify=TRUE)
  
  intersect.ngrams <- intersect(ref.ngrams, compare.ngrams)
  dt.ngrams <- data.table(intersect.ngrams)
  if (nrow(dt.ngrams) == 0) return (0) #si pas de mots en commun, alors on sort!
  
  dt.ngrams$nbMot <- count_words(dt.ngrams$intersect.ngrams)
  dt.ngrams.tally <- dt.ngrams %>% group_by(nbMot) %>% tally()
  
  #prends un n-gram commun et vérifie s'il est unique
  haystack <- paste(intersect.ngrams, collapse=" ")    #on remet ça en texte
  dt.ngrams$nbOccur <- sapply(dt.ngrams$intersect.ngrams, str_count, string=haystack)
  dt.ngrams.tally <- 
    dt.ngrams[nbOccur == 1] %>% #on s'intéresse seulement aux ngrams uniques
    group_by(nbMot) %>% tally()
  overlap.ngram <- sum(dt.ngrams.tally$nbMot * dt.ngrams.tally$n) #nb de mots en overlap
  #print(dt.ngrams.tally)
  return (overlap.ngram)
}


analOverlap <- function (parsed) {
  
  
  #--- pré-traitement ----
  #tout en minuscules (sinon les lexies seront pas détectés comme étant la même chose)
  #parsed$token <- sapply(parsed$token, tolower)
  #parsed$lemma <- sapply(parsed$lemma, tolower)
  parsed <- parsed[upos != "PUNCT"] #on enlève les ponctuations
  parsed <- parsed[isContent == TRUE & compte == TRUE]
  
 
  #--- comparaison ---
  reference = parsed[paragraph_id==1]
  ref.txt <- paste(reference$token, collapse=" ")    #on remet ça en texte pour les n-gram
  nbParag <- max(parsed$paragraph_id)
  
  #overlaps <- vector(length=nbParag-1)
  dt.overlaps <- data.table( #init
    idParag = 1:nbParag,
    nbOverlap = 0,
    nbMotContenu=0,
    nbMotOverNgram=0
  )
  
  for (i in 1:nbParag ) {
    iParag <- parsed[paragraph_id==i]
    iOverlaps <- comparerParagSimple(reference, iParag)
    dt.overlaps[i, "nbOverlap"] <- iOverlaps$nbOverlap
    dt.overlaps[i, "nbMotContenu"] <- iOverlaps$nbMotContenu
    dt.overlaps[i, "nbMotTotal"] <- nrow(parsed[paragraph_id == i])
    compare.txt <- paste(iParag$token, collapse=" ")
    dt.overlaps[i, "nbMotOverNgram"] <- comparerParagNgram(ref.txt, compare.txt, length(iParag$token), 2)  
  }
  
  dt.overlaps[, propOverCont := round(nbOverlap / nbMotContenu, 2)]
  dt.overlaps[, propOverTotal := round(nbOverlap / nbMotTotal, 2)]
  dt.overlaps[, propMotOverNGram := round(nbMotOverNgram / nbMotTotal, 2)]

  return(dt.overlaps)  
}

leParsing <- function(txt) {
  content_categories <- c("VERB", "NOUN", "PROPN", "ADJ", "ADV") #utilisé quand on tag les content words
  
  parsed <- udpipe_annotate(udmodel_french, x = txt, parser = "none" )
  parsed <- as.data.table(parsed)
  
  #gestion des tokens introduits par le parsing (ex: au parc devient à le parc)
  #on pourrait flusher directement, j'ai créé un bool au cas où on voudrait les utiliser finalement
  listeComptePas <- parsed[!is.na(token) & is.na(lemma), token_id]
  listeComptePas <- str_split(listeComptePas, "-", simplify = TRUE)
  listeComptePas <- as.vector(listeComptePas)
  parsed$compte <- TRUE #par défaut, on compte le token
  parsed[token_id %in% listeComptePas & upos %in% c("ADP", "DET"), compte := FALSE]
  rm(listeComptePas)
  
  parsed$upos <- factor(parsed$upos)
  
  #petit ménage
  parsed$xpos <- NULL
  parsed$doc_id <- NULL
  parsed$deps <- NULL
  parsed$misc <- NULL
  parsed$dep_rel <- NULL
  parsed$head_token_id <- NULL
  
  #listeComptePas <- parsed[!is.na(token) & is.na(lemma), token_id]
  #listeComptePas <- str_split(listeComptePas, "-", simplify = TRUE)
  #listeComptePas <- as.vector(listeComptePas)
  #parsed$compte <- TRUE #par défaut, on compte le token
  #parsed[token_id %in% listeComptePas & upos %in% c("ADP", "DET"), compte := FALSE]
  #rm(listeComptePas)
  
  #on tag les content words
  parsed$isContent <- FALSE
  parsed[upos %in% content_categories, isContent := TRUE]
  
  
  
  return(parsed)
}

#---- calcul de l'indice de Gunning et autres stats de parag ---
calculeIndex <- function (parsed) {
  parsed.gunning <- parsed[!(upos %in% c("PUNCT")) ] #exclut la ponctuation du calcul
  parsed.gunning <- parsed.gunning[compte == TRUE]
  
  if (!curr_dict() == "fr") switch_dict("fr")
  df.syll <- hyphenate(parsed.gunning$token, simplify = FALSE )
  parsed.gunning$nbSyll <- lengths(df.syll)
  
  parsed.gunning$longueur.mot <- "NA"
  parsed.gunning[,longueur.mot := nchar(token)]
  stats.phrase <- parsed.gunning[, .(longueur.phrase=sum(longueur.mot)), 
                                 by=c("paragraph_id", "sentence_id")]
  parsed.gunning <- merge(parsed.gunning, stats.phrase)
  stats.parag <- parsed.gunning[, .(
    nbPhrase = uniqueN(sentence_id),
    nbMotParag=.N
  ), by=c("paragraph_id")] #mots total et phrases dans le paragraphe
  parsed.gunning <- merge(parsed.gunning, stats.parag)
  parsed.gunning[, longueurMoyPhrase := nbMotParag / nbPhrase]
  
  mots.longs <- parsed.gunning[longueur.mot>=9, .(nbMotLongChar=.N), 
                               by=c("paragraph_id")] #mots longs dans le paragraphe
  parsed.gunning <- merge(parsed.gunning, mots.longs)
  mots.longs.syll <- parsed.gunning[nbSyll >=3, .(nbMotLongSyll=.N), 
                                    by=c("paragraph_id")] #mots longs dans le paragraphe
  parsed.gunning <- merge(parsed.gunning, mots.longs.syll)
  parsed.gunning[,indice.gunning.char := round((longueurMoyPhrase + 
                                                  ((nbMotLongChar / nbMotParag)*100)) *0.4, 2)]
  parsed.gunning[,indice.gunning.syll := round((longueurMoyPhrase + 
                                                  ((nbMotLongSyll / nbMotParag)*100)) *0.4, 2)]
  
  verbes.conju <- parsed.gunning[upos == "VERB" & !(feats == "VerbForm=Inf"), .(nbVerbeConju=.N), by="paragraph_id"]
  parsed.gunning <- merge(parsed.gunning, verbes.conju)
  
  tablo.parag <- unique(parsed.gunning[, .(
    gunning.char=indice.gunning.char, 
    gunning.syll=indice.gunning.syll,
    nbPhrase, nbMotParag, nbVerbeConju,
    longueurMoyPhrase=round(longueurMoyPhrase, 2),
    nbMotLongChar, nbMotLongSyll,
    pourMotLongChar=round(100*(nbMotLongChar/nbMotParag)),
    pourMotLongSyll=round(100*(nbMotLongSyll/nbMotParag))
  ), by="paragraph_id"])
  
  #attr(tablo.parag$gunning.char, 'label') <- 'Indice Gunning Fog, calcul par nb de caractères'
  
  return(tablo.parag)
}
