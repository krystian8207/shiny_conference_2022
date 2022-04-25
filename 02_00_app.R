library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("nrow", "Number of rows", value = 50, min = 1, max = 1000, step = 1),
      div(id = "variables"),
      textInput("name", "Column name"),
      actionButton("new", NULL, icon = icon("plus"), width = "100%"),
      actionButton("run", NULL, icon = icon("play"), width = "100%")
    ),
    mainPanel()
  )
)

server <- function(input, output, session) {

  observeEvent(input$new, {
    print("new clicked")
    print(input$new)
  })
  
  observeEvent(input$run, {
    print("run clicked")
    print(input$run)
  })
}

shinyApp(ui, server)
