# https://shiny.rstudio.com/gallery/widget-gallery.html
library(shiny)

ui <- fluidPage(
  selectInput("option", label = "Choose letter", choices = letters)
)

server <- function(input, output, session) {
  observeEvent(input$option, {
    print(input$option)
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)