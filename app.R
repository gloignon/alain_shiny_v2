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
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "overlap",
              h1("Analyse des superpositions lexicales")
      ),
      
      # Third tab content
      tabItem(tabName = "corpus",
              h1("Analyse de corpus")
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