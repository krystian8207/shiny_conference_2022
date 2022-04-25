library(shiny)
library(httr)

# number <- 10
# res <- httr::GET(glue("http://numbersapi.com/{number}"))
# res$status_code
# httr::content(res)

ui <- fluidPage(
  numericInput("number", "Select number", value = 1, min = 0, step = 1),
  textOutput("fact")
)

server <- function(input, output, session) {
  output$fact <- renderText({
    res <- httr::GET(glue("http://numbersapi.com/{input$number}"))
    if (res$status_code == 200) {
      fact <- httr::content(res)
    } else {
      fact <- "No response"
    }
    fact
  })
}

shinyApp(ui, server)