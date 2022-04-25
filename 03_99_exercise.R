# ready ui: two buttons: one for delete, the second one for edit
# task is to insert when clicked new, remove when clicked delete (if easy)

# Make sure clicking delete button removes panel.
# Add proper id to confirm - print modal closed.
# Extend conditionalPanel, what effect?

library(shiny)
library(shinyGizmo)
library(glue)
library(magrittr)

source("tools.R")

column_ui <- function(id, name) {
  wellPanel(
    id = id,
    modalDialogUI(
      glue("{id}_modal"), 
      "Edit Panel",
      footer = actionButton(glue("{id}_confirm"), "Confirm", `data-dismiss` = "modal")
    ),
    actionButton(glue("{id}_delete"), NULL, icon("trash-alt")),
    name
  )
}

column_server <- function(id, input, output, session) {
  observeEvent(input[[glue("{id}_delete")]], {
    
  })
  
  observeEvent(input[["proper_id_here"]], {
    print("modal closed")
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
        "input.nrow > 0",
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
  
  observeEvent(input$new, {
    id <- genid()
    insertUI(
      "#variables",
      where = "beforeEnd",
      column_ui(id, input$name),
      immediate = TRUE
    )
    column_server(id, input, output, session, modal_closed)
  })

  observeEvent(input$run, {
    my_table(iris[1:input$nrow, ])
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
