library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("x_max", "Maximum x axis value", value = 10)
    ),
    mainPanel(
      plotOutput("sine"),
      tableOutput("plot_table")
    )
  )
)

server <- function(input, output, session) {
  output$sine <- renderPlot({
    validate(need(input$x_max > 0, message = "No valid data"))
    plot_data <- data.frame(
      x = seq(0, input$x_max, by = 0.1),
      y = sin(seq(0, input$x_max, by = 0.1))
    )
    plot(plot_data$x, plot_data$y, type = "l")
  })
  
  output$plot_table <- renderTable({
    req(input$x_max > 0)
    plot_data <- data.frame(
      x = seq(0, input$x_max, by = 0.1),
      y = sin(seq(0, input$x_max, by = 0.1))
    )
    plot_data
  })
}

shinyApp(ui, server)