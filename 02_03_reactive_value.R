# Rule:
# rval <- reactiveVal(<initial-value>)  - initializing
# rval() - taking the value
# rval(<new-value>) - reassigning

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("x_max", "Maximum x axis value", value = 10),
      textInput("title", "Title")
    ),
    mainPanel(
      plotOutput("sine"),
      tableOutput("plot_table")
    )
  )
)

server <- function(input, output, session) {
  plot_data <- reactiveVal(NULL)
  
  observeEvent(input$x_max, {
    if (identical(input$x_max > 0, TRUE)) {
      x <- seq(0, input$x_max, by = 0.1)
      df <- data.frame(x = x, y = sin(x))
      plot_data(df)
    } else {
      plot_data(NULL)
    }
  })
  
  output$sine <- renderPlot({
    validate(need(!is.null(plot_data()), message = "No valid data"))
    plot(plot_data()$x, plot_data()$y, main = input$title, type = "l")
  })
  
  output$plot_table <- renderTable({
    validate(need(!is.null(plot_data()), message = "No valid data"))
    plot_data()
  })
}

shinyApp(ui, server)