library(shiny)
library(shinyGizmo)
library(magrittr)
library(glue)

source("tools.R")

ui <- fluidPage(
  tags$head(
    shiny::tags$script(type = "text/javascript", src = "hidden_mode.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  sidebarLayout(
    sidebarPanel(
      h3("Table Generator"),
      numericInput("nrow", "Number of rows", value = 50, min = 1, max = 1000, step = 1),
      textOutput("number_facts"),
      div(id = "variables"),
      div(
        id = "define-var",
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
      width = 3
    ),
    mainPanel(
      downloadButton("downloadData", NULL),
      DT::dataTableOutput("table"),
      width = 9
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
  
  output$number_facts <- renderText({
    req(input$hidden_mode)
    httr::content(
      httr::GET(glue("http://numbersapi.com/{input$nrow}"))
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(res_table(), file)
    }
  )
}

shinyApp(ui, server)
