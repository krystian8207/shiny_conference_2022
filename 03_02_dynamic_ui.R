library(shiny)

ui <- fluidPage(
  actionButton("add", "Add"),
  numericInput("which", "Which to remove?", value = 1),
  actionButton("remove", "Remove"),
  div(id = "variables")
)

server <- function(input, output, session) {
  observeEvent(input$add, {
    insertUI(
      selector = "#variables",
      where = "beforeEnd",
      ui = wellPanel(id = input$add, input$add),
      immediate = TRUE
    )
  })
  
  observeEvent(input$remove, {
    removeUI(
      selector = glue("#{input$which}"),
    )
  })
}

shinyApp(ui, server)