library(shiny)
library(shinyGizmo)

ui <- fluidPage(
  modalDialogUI(
    modalId = "my_modal",
    "Modal content"
  ),
  actionButton("open_second", "Open 2nd modal", icon = icon("pen")),
  modalDialogUI(
    modalId = "my_modal_2",
    textInput("my_text_2", "Place text here"),
    textOutput("my_text_2_out"),
    button = NULL
  ),
  modalDialogUI(
    modalId = "my_modal_3",
    textInput("my_text_3", "Place text here"),
    footer = actionButton("close", "Close", icon = icon("times"), `data-dismiss` = "modal")
  ),
  textOutput("my_text_3_out")
)

server <- function(input, output, session) {
  
  observeEvent(input$open_second, {
    showModalUI("my_modal_2")
  })
  
  output$my_text_2_out <- renderText({
    input$my_text_2
  })
  
  output$my_text_3_out <- renderText({
    input$my_text_3
  })
  
  observeEvent(input$close, {
    print("modal closed")
  })
}

shinyApp(ui, server)