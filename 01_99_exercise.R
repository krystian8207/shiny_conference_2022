# In the below app:
# 1. Put numericInput
# 2. Put run button
# 3. Write run observer
# 4. Run app and test it out

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # <numeric_input>
      div(id = "variables"),
      textInput("name", "Column name"),
      actionButton("new", NULL, icon = icon("plus"), width = "100%")
      # <run_button>
    ),
    mainPanel()
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$new, {
    print("new clicked")
    print(input$name)
  })
  
  # <run_observer>
  
}

shinyApp(ui, server)
