# In the below application:
# 1. Attach 'hidden_mode.js' JS script.
# Then run the application and type 'shiny' having the application open.
# Please try to analyze 'hidden_mode.js' to find out how to revert the above effect.
# 
# 2. Attach 'custom.css' CSS file.
# Run the application and check its new styling.
# Try to play with modifying 
# :root {
#   --bright: #d5ecff;
#   --middle: #699EB5;
#   --dark: #377290;
#   --darker: #3D6B82;
# }
# values in 'custom.css' to apply custom colouring.

library(shiny)
library(shinyGizmo)
library(magrittr)
library(glue)

source("tools.R")

ui <- fluidPage(
  tags$head(
    
  ),
  sidebarLayout(
    sidebarPanel(
      h3("Table Generator"),
      numericInput("nrow", "Number of rows", value = 50, min = 1, max = 1000, step = 1),
      textOutput("number_facts"),
      div(id = "variables"),
      div(
        id = "define-vars",
        textInput("name", "Column name"),
        conditionalPanel(
          "input.name != ''",
          actionButton("new", NULL, icon = icon("plus"), width = "100%")  
        )
      ),
      conditionalPanel(
        "input.nrow > 0 & $('#variables > div').length > 0",
        actionButton("run", "Generate", width = "100%")  
      ),
      width = 2
    ),
    mainPanel(
      DT::dataTableOutput("table"),
      width = 10
    )
  )
)

server <- function(input, output, session) {
  session$userData$vars <- list()
  session$userData$clear <- reactiveVal(1)
  res_table <- reactiveVal(NULL)
  
  observeEvent(input$new, {
    id <- genid()
    insertUI(
      "#variables",
      where = "beforeEnd",
      edit_panel_ui(id, input$name),
      immediate = TRUE
    )
    edit_panel_server(id)
  })
  
  observeEvent(input$run, {
    req(!is.null(session$userData$vars))
    res_table(gen_table(session$userData$vars, input$nrow))
  })
  
  output$table <- DT::renderDataTable({
    validate(need(
      !is.null(res_table()),
      "No table created."
    ))
    res_table()
  }, options = list(
    paging = TRUE,
    pageLength = 10,
    searching = FALSE
  ))
  
  observeEvent(session$userData$clear(), {
    updateTextInput(inputId = "name", value = "")
  }, ignoreInit = TRUE)
  
  observeEvent(input$hidden_mode, {
    print(input$hidden_mode)
  })
  
  output$number_facts <- renderText({
    req(input$hidden_mode)
    res <- httr::GET(glue("http://numbersapi.com/{input$nrow}"))
    if (res$status_code == 200) {
      fact <- httr::content(res)
    } else {
      fact <- "No response"
    }
    fact
  })
}

shinyApp(ui, server)
