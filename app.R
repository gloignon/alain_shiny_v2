#interface stuff

library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "ALAIN"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Lisibilité", tabName = "lisibilite", icon = icon("eye-open", lib = "glyphicon")),
      menuItem("Superpositions", tabName = "overlap", icon = icon("file-alt")),
      menuItem("Analyse de corpus", tabName = "corpus", icon = icon("folder-open", lib = "glyphicon")),
      menuItem("Voir le parsing", tabName = "parsing", icon = icon("th-list", lib = "glyphicon"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "lisibilite",
              h1("Production d'indices de lisibilité"),
              fluidRow(
                box (
                  title = "Entrez du texte ici",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 3,
                  "Des truc peuvent s'écrire ici",
                  textAreaInput(inputId="boite_lisib",
                                label = "",
                                placeholder = "entrez texte ici",
                                value = default.input,
                                height = "400"),
                  actionButton("but_calculer_lisib", "Calculer les indices de lisibilité")
                ),
                
                box (
                  title = "Résultats",
                  "Les indices de lisibilité du parag vont apparaitre ici."
                )
              )
      ),
      
      # Second tab content
        tabItem(tabName = "overlap",
              h1("Évaluation des superpositions lexicales."),
              fluidRow(
                box (
                  title = "Entrez du texte ici",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 3,
                  "Des truc peuvent s'écrire ici",
                  textAreaInput(inputId="boite_overlap",
                                label = "",
                                placeholder = "entrez texte ici",
                                value = default.input,
                                height = "400"),
                  actionButton("but_calculer_overlap", "Calculer les superpositions")
                ),
                
                box (
                  title = "Résultats",
                  "Les indices de superposition vont apparaitre ici."
                )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "corpus",
              h1("Analyse de corpus"),
              fileInput("fichier_corpus", "Sélectionnez votre fichier zip",
                        accept = c(
                          "application/zip",
                          ".zip")
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "parsing",
              h1("Parsing")
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)