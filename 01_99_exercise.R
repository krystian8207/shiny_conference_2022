# In the below app:
# 1. Modify numericInput in line 20 so that:
# - its value can be accessible by input$nrow
# - its initial value equals 50
# 2. Create actionButton (in line 27) so that:
# - its value can be accessible by input$run,
# - it displays "Generate" label,
# - it covers full width of its container.
# Use actionButton in line 25 as an example.
# 3. Create an observer (in line 40) that listens to the above button changes.
# Make the observer callback print "run clicked" in the console.
# 4. Run app and test it out.

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h3("Table Generator"),
      numericInput(inputId = "my_number", label = "Number of rows", value = 0, min = 1, max = 150, step = 1),
      div(id = "variables"),
      div(
        id = "define-vars",
        textInput("name", "Column name"),
        actionButton("new", NULL, icon = icon("plus"), width = "100%")
      ),
      # <button>
    ),
    mainPanel()
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$new, {
    print("new clicked")
    print(input$name)
  })
  
  # <run_button_observer>
  
}

shinyApp(ui, server)
