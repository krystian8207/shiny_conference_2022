library(shiny)
library(shinyGizmo)
library(magrittr)
library(glue)

source("tools.R")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h3("Table Generator"),
      numericInput("nrow", "Number of rows", value = 50, min = 1, max = 1000, step = 1),
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
    req(length(session$userData$vars) > 0)
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
  })
}

shinyApp(ui, server)