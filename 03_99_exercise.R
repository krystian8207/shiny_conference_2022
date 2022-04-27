# In the below app:
# 1. Make sure clicking delete button removes wellPanel (line 30).
# 2. Add correct id in observer that listens to confirm button changes (line 33).
# 3. Replace "run" button conditionalPanel with condition 
# "input.nrow > 0 & $('#variables > div').length > 0".
# Can you guess what effect does it have in the application?

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
      textInput(glue("{id}_name"), "Name", value = name),
      footer = actionButton(glue("{id}_confirm"), "Confirm", `data-dismiss` = "modal")
    ),
    actionButton(glue("{id}_delete"), NULL, icon("trash-alt")),
    textOutput(glue("{id}_outname"), inline = TRUE)
  )
}

column_server <- function(id, input, output, session) {
  observeEvent(input[[glue("{id}_delete")]], {
    
  })
  
  observeEvent(input[["proper_id_here"]], {
    print("modal closed")
  })
  
  output[[glue("{id}_outname")]] <- renderText({
    input[[glue("{id}_name")]]
  })
}

ui <- fluidPage(
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
        "input.nrow > 0",
        actionButton("run", "Generate", width = "100%")  
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
    column_server(id, input, output, session)
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
    pageLength = 10,
    searching = FALSE
  ))
}

shinyApp(ui, server)
