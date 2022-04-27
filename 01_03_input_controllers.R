# https://shiny.rstudio.com/gallery/widget-gallery.html

library(shiny)

ui <- fluidPage(
  actionButton(
    inputId = "my_button", label = "Click Me"
  ),
  numericInput(
    inputId = "my_number", label = "Place number here", value = 1, min = 1, max = 10, step = 1
  ),
  textInput(
    "my_text", label = "Place text here", value = "Default text"
  ),
  sliderInput(
    inputId = "my_slider_one", label = "Select number here", value = 1, min = 1, max = 10, step = 1
  ),
  sliderInput(
    inputId = "my_slider_two", label = "Select range here", value = c(5, 6), min = 1, max = 10, step = 1
  ),
  selectInput(
    inputId = "my_dropdown", label = "Choose a letter", choices = letters
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)