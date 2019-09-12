library(DT) #pour tables dans shiny
library(shinycssloaders)
library(shinydashboard)

server <- function(input, output) {
 
  observeEvent(input$but_calculer_lisib,{
    currentParsing <- leParsing(input$boite_lisib)
    output$parsed_mini <- DT::renderDT({
      currentParsing
    })
  })
  
  observeEvent(input$tabs == "parsing",{
    output$parsed = DT::renderDT({
      leParsing(input$boite_lisib)
    })
  })
  

  
  #output$overlap <- DT::renderDataTable(
  #  DT::datatable(analOverlap( currentParsedBoite() ), 
  #                options = list(searching = FALSE, paging = FALSE))
  #)
  
}