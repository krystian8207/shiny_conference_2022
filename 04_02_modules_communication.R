library(shiny)
library(shinyGizmo)
library(glue)
library(magrittr)

source("tools.R")

column_ui <- function(id, name) {
  ns <- NS(id)
  wellPanel(
    id = id,
    modalDialogUI(
      ns("modal"), 
      textInput(ns("my_text"), "Put text here"),
      numericInput(ns("my_number"), "Put number here", value = 1),
      footer = actionButton(ns("confirm"), "Confirm", `data-dismiss` = "modal")
    ),
    actionButton(ns("delete"), NULL, icon("trash-alt")),
    name
  )
}

column_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    showModalUI("modal")
    
    observeEvent(input[["delete"]], {
      removeUI(glue("#{id}"))
    })
    
    observeEvent(input[["confirm"]], {
      session$userData$modal_closed(
        session$userData$modal_closed() + 1
      )
    })
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("nrow", "Number of rows", value = 50, min = 1, max = 1000, step = 1),
      textOutput("number_facts"),
      div(id = "variables"),
      textInput("name", "Column name"),
      conditionalPanel(
        "input.name != ''",
        actionButton("new", NULL, icon = icon("plus"), width = "100%")  
      ),
      conditionalPanel(
        "input.nrow > 0 & $('#variables > div').length > 0",
        actionButton("run", NULL, icon = icon("play"), width = "100%")  
      )
    ),
    mainPanel(
      DT::dataTableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  my_table <- reactiveVal(NULL)
  session$userData$modal_closed <- reactiveVal(1)
  
  observeEvent(input$new, {
    id <- genid()
    insertUI(
      "#variables",
      where = "beforeEnd",
      column_ui(id, input$name),
      immediate = TRUE
    )
    column_server(id)
  })
  
  observeEvent(input$run, {
    my_table(iris[1:input$nrow, ])
  })
  
  observeEvent(session$userData$modal_closed(), {
    updateTextInput(session, inputId = "name", value = "")
  })
  
  output$table <- DT::renderDataTable({
    validate(need(
      !is.null(my_table()),
      "No table created."
    ))
    my_table()
  }, options = list(
    paging = TRUE,
    pageLength = 10
  ))
}

shinyApp(ui, server)
