
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
                  width = 6,
                  "Des truc peuvent s'écrire ici",
                  textAreaInput(inputId="boite_lisib",
                                label = "",
                                placeholder = "entrez texte ici",
                                value = "Je pense donc je suis un fripon.",
                                height = "400"),
                  actionButton(inputId = "but_calculer_lisib", "Calculer les indices de lisibilité")
                ),
              ), #fin fluidRow
              fluidRow(tabBox(
                width = 12,
                side = "left",
                selected = "Stats phrases",
                tabPanel (
                  title = "Stats phrases",
                  solidHeader = FALSE,
                  DTOutput("tablo_stats_phrases")
                ),
                tabPanel (
                  title = "Stats parag",
                  solidHeader = FALSE,
                  DTOutput("tablo_stats_parag")
                ) #fin box de droite
              ))
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
                                value = "",
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
                selected = "Parsing de base",
                tabPanel("Parsing de base", 
                         actionButton("but_telecharger_parsing", "Télécharger le tableau d'analyse de base"), br(),
                         DTOutput("parsed", width="100%")  %>% withSpinner(type=4) ),
                tabPanel("Parsing augmenté", 
                         actionButton("but_telecharger_parsing_aug", "Télécharger le tableau d'analyse"), br(),
                         DTOutput("parsedAug", width="100%")  %>% withSpinner(type=4) ),
                tabPanel("Mots rares", 
                         actionButton("but_telecharger_motsrares", "Télécharger le tableau des mots rares"), br(),
                         DTOutput("tablo_mots_rares2", width = "100%"))
              )
      )
    )
  )
)