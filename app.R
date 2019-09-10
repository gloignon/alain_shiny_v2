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
                  solidHeader = FALSE,
                  collapsible = FALSE,
                  width = 4,
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
                  solidHeader = FALSE,
                  width = 8,
                  tabBox(
                    width = 12,
                    side = "left", height = "250px",
                    selected = "Indices standardisés",
                    tabPanel("Indices standardisés", 
                             "Le tableau des indices va aller ici", br(), br(),
                             actionButton("but_lisib_stand", "Télécharger") 
                    ),                      
                    tabPanel("Estimation du niveau", 
                             "En se basant sur notre corpus de textes, l'extrait de texte fourni est estimé au niveau suivant:", br()
                    ),
                    tabPanel("Niv. d'après corpus perso", 
                             "Vous devez entrer un corpus en .zip pour utiliser cette fonction."
                    )
                  )
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
                  width = 9,
                  title = "Résultats",
                  "Les indices de superposition vont apparaitre ici.", br(),
                  actionButton("but_telecharger_colab", "Télécharger les résultats")
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
              ),
              "ici vont app les carac du corpus", br(), br(),
              actionButton("but_telecharger_ind_corpus", "Télécharger caractéristiques du corpus")
      ),
      
      # Fourth tab content
      tabItem(tabName = "parsing",
              h1("Parsing"),
              tabBox(
                width = 12,
                side = "left",
                selected = "Parsing complet",
                tabPanel("Parsing complet", 
                         actionButton("but_telecharger_parsing", "Télécharger le tableau d'analyse lexicale"), br(),
                         "Tab content 1"),
                tabPanel("Mots rares", actionButton("but_telecharger_motsrares", "Télécharger le tableau des mots rares"), br(),
                         "Tab content 2")
              )
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