
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
                                value = "Je pense donc je suis.",
                                height = "400"),
                  actionButton(inputId = "but_calculer_lisib", "Calculer les indices de lisibilité")
                ),
                
                box (
                  title = "Résultats",
                  solidHeader = FALSE,
                  width = 8,
                  DTOutput("tablo_index_lisib", width="100%")
                ) #fin box de droite
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
                selected = "Parsing complet",
                tabPanel("Parsing complet", 
                         actionButton("but_telecharger_parsing", "Télécharger le tableau d'analyse lexicale"), br(),
                         DTOutput("parsed", width="100%")  %>% withSpinner(type=4) ),
                tabPanel("Mots rares", actionButton("but_telecharger_motsrares", "Télécharger le tableau des mots rares"), br(),
                         "Tab content 2")
              )
      )
    )
  )
)