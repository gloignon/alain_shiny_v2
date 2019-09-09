library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "ALAIN"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Entrée de données", tabName = "input", icon = icon("file-alt")),
      menuItem("Configuration", tabName = "config", icon = icon("cogs"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "input",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "config",
              h2("Widgets tab content")
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