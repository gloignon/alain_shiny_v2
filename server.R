server <- function(input, output) {
  
  values <- reactiveValues(currentParsing = NULL, sommaire.corpus = NULL)
 
  observeEvent(input$but_calculer_lisib,{
    values$currentParsing <- leParsing(input$boite_lisib)
    output$parsed_mini <- DT::renderDT({
      values$currentParsing
    })
    
    parsed.freq <- CalculerFreq(values$currentParsing)
    values$mots.rares <- FiltrerMotsRares(parsed.freq) #seuil.livre = 40, seuil.film = 40, 
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
    
    values$statsPhrase <- ProduireStatsPhrases(parsed.freq, values$mots.rares)
    
    #values$sommaire.corpus <- CalculerFog(parsed.freq, mots.rares)
    values$currentParsingAug <- values$currentParsing
    
    output$tablo_index_lisib <- DT::renderDT({
      values$statsPhrase
    })
    
    output$tablo_mots_rares <- DT::renderDT({
      #CalculerFog(parsed.freq, mots.rares)  
      #mots.rares
      values$mots.rares
    })
    
  })
  
  observeEvent(input$tabs == "parsing",{
    output$parsed = DT::renderDT({
      values$currentParsing
    })
    output$parsedAug = DT::renderDT({
      values$currentParsingAug
    })
    output$tablo_mots_rares2 = DT::renderDT({
      values$mots.rares
    })
  })
  
  #output$overlap <- DT::renderDataTable(
  #  DT::datatable(analOverlap( currentParsedBoite() ), 
  #                options = list(searching = FALSE, paging = FALSE))
  #)
  
}