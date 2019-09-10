#interface stuff

library(shinydashboard)

ui <- dashboardPage(
  skin = "blue",
  
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
<<<<<<< HEAD
                  solidHeader = FALSE,
=======
                  solidHeader = TRUE,
>>>>>>> e8677c58406c6f47564573b2170c8cd64e5a41f5
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
<<<<<<< HEAD
              
                box (
                  title = "Résultats",
                  solidHeader = FALSE,
                  width = 9,
                  tabBox(
                    width = 12,
                    side = "left", height = "250px",
                    selected = "Indices standardisés",
                    tabPanel("Indices standardisés", "Tab content 1"),
                    tabPanel("Corpus éducation aux adultes", "Tab content 2"),
                    tabPanel("Corpus personnalisé ", "Vous devez téléverser un corpus pour utiliser cette fonction. Voir la section Analyse de corpus.")
                  ),
                  actionButton("but_telecharger_lisib", "Télécharger les résultats")
=======
                
                box (
                  title = "Résultats",
                  "Les indices de lisibilité du parag vont apparaitre ici."
>>>>>>> e8677c58406c6f47564573b2170c8cd64e5a41f5
                )
              ) #fin fluidRow
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
<<<<<<< HEAD
              ),
              "ici vont app les carac du corpus"
=======
              )
>>>>>>> e8677c58406c6f47564573b2170c8cd64e5a41f5
      ),
      
      # Fourth tab content
      tabItem(tabName = "parsing",
              h1("Parsing"),
              "le long tableau va app ici"
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