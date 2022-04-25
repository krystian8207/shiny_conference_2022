library(shiny)

# Run button as conditionPanel (input.nrow > 0)
# table output -> DT
# Render table only when my_table() is not null

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("nrow", "Number of rows", value = 50, min = 1, max = 1000, step = 1),
      div(id = "variables"),
      textInput("name", "Column name"),
      conditionalPanel(
        "input.name != ''",
        actionButton("new", NULL, icon = icon("plus"), width = "100%")  
      ),
      actionButton("run", NULL, icon = icon("play"), width = "100%")  
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  my_table <- reactiveVal(NULL)
  
  observeEvent(input$new, {
    print("new clicked")
  })
  
  observeEvent(input$run, {
    my_table(iris[1:input$nrow, ])
  })
  
  output$table <- renderTable({
    validate(need(TRUE, message = "No table created."))
    my_table()
  }, options = list(
    paging = TRUE,
    pageLength = 10
  ))
}

shinyApp(ui, server)
