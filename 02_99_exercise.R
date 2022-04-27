# In the below app:
# 1. Wrap "run" button into conditionalPanel.
# Make sure the condition checks if number of rows input (nrow) is positive.
# 2. Replace tableOutput with dataTableOutput, and renderTable with renderDataTable.
# What effect does the change have on the resulted table?
# 3. Make sure the table is rendered only when it's not NULL.
# To do so, please replace 'TRUE' in line 46 with proper condition.

library(shiny)
library(DT)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h3("Table Generator"),
      numericInput("nrow", "Number of rows", value = 50, min = 1, max = 1000, step = 1),
      div(id = "variables"),
      div(
        id = "define-vars",
        textInput("name", "Column name"),
        conditionalPanel(
          "input.name != ''",
          actionButton("new", NULL, icon = icon("plus"), width = "100%")  
        )
      ),
      actionButton("run", "Generate", width = "100%")  
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
    pageLength = 10,
    searching = FALSE
  ))
}

shinyApp(ui, server)
