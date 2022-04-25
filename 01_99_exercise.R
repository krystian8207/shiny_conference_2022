# In the below app:
# 1. Modify numericInput in line 20 so that:
# - its value can be accessible by input$nrow
# - its initial value equals 50
# 2. Create actionButton (in line 24) so that:
# - its value can be accessible by input$run,
# - it displays no label,
# - it displays "play" icon,
# - it covers full width of its container.
# Use actionButton in line 23 as an example.
# 3. Create an observer (in line 37) that listens to the above button changes.
# Make the observer callback print "run clicked" in the console.
# 4. Run app and test it out.

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "my_number", label = "Number of rows", value = 0, min = 1, max = 150, step = 1),
      div(id = "variables"),
      textInput("name", "Column name"),
      actionButton("new", NULL, icon = icon("plus"), width = "100%")
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
