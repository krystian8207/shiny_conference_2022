library(shiny)

ui <- fluidPage(
  selectInput("option", label = "Choose letter", choices = letters),
  textOutput("letter")
)

server <- function(input, output, session) {
  output$letter <- renderText({
    paste("Selected letter is:", input$option)
  })
}

shinyApp(ui, server)