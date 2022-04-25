# https://shiny.rstudio.com/articles/layout-guide.html
library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel("Sidebar"),
    mainPanel(
      div("I'm in the main panel"),
      tabsetPanel(
        tabPanel("Tab 1", "Tab 1 Content"),
        tabPanel("Tab 2", "Tab 2 Content")
      )
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)