library(shiny)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "simple.css")
  ),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        actionButton("run", "Click Me!")
      ),
      wellPanel(
        actionButton("play", "Click Me!")
      )
    ),
    mainPanel()
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)