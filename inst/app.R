library(shiny)
library(pseudoviz)
library(purrr)

ui <- fluidPage(
  verbatimTextOutput("test") 
)

server <- function(session, input, output) {
  
  output$test <- renderPrint({
    viz_recommend(d = iris, package = "hgmagic")
  })
  
}

shinyApp(ui, server)
