# Add css.
# Add js.
# Inside renderText display fact using numbers api.

library(shiny)
library(shinyGizmo)
library(magrittr)

source("tools.R")

edit_panel_ui <- function(id, name) {
  ns <- NS(id)
  
  wellPanel(
    id = ns("container"),
    modalDialogUI(
      modalId = ns("modal"),
      tagList(
        textInput(ns("name"), "Column name", name),
        selectInput(
          ns("type"), "Column type", 
          choices = c("numeric", "integer", "character", "custom"),
          selected = "numeric"
        ),
        conditionalPanel(
          "input.type == 'numeric'", 
          ns = ns,
          tabsetPanel(
            id = ns("numtype"),
            tabPanel(
              "Range", 
              value = "numrange",
              sliderInput(ns("numminmax"), "Column range", min = 1, max = 20, value = c(5, 10), step = 1)    
            ),
            tabPanel(
              "Distribution",
              value = "numdistro",
              selectInput(ns("numdistro"), "Distribution", choices = c("normal", "exponential"), selected = "normal"),
              conditionalPanel(
                "input.numdistro == 'normal'", ns = ns, 
                numericInput(ns("normal_mean"), "Mean", value = 0),
                numericInput(ns("normal_sd"), "SD", value = 1, min = 0)
              ),
              conditionalPanel(
                "input.numdistro == 'exponential'", ns = ns, 
                numericInput(ns("exponential_lambda"), "Lambda", value = 0)
              )
            )
          )
        ),
        conditionalPanel(
          "input.type == 'character'", 
          ns = ns,
          sliderInput(ns("charminmax"), "Number of characters", min = 1, max = 20, value = c(5, 10), step = 1),
          selectInput(ns("charpattern"), "Pattern", choices = c("[[a-z][A-Z]]", "[a-z]", "[A-Z]", "[a-zA-Z0-9]"))
        ),
        conditionalPanel(
          "input.type == 'integer'",
          ns = ns,
          tabsetPanel(
            id = ns("inttype"),
            tabPanel(
              "Range", 
              value = "intrange",
              sliderInput(ns("intminmax"), "Column range", min = 1, max = 20, value = c(5, 10), step = 1)    
            ),
            tabPanel(
              "Distribution",
              value = "intdistro",
              selectInput(ns("intdistro"), "Distribution", choices = c("binomial", "poisson"), selected = "binomial"),
              conditionalPanel(
                "input.intdistro == 'binomial'", ns = ns, 
                numericInput(ns("binomial_size"), "Size", value = 10, min = 0, step = 1),
                numericInput(ns("binomial_prob"), "Prob", value = 0.5, min = 0, max = 1)
              ),
              conditionalPanel(
                "input.intdistro == 'poisson'", ns = ns, 
                numericInput(ns("poisson_lambda"), "Lambda", value = 1, min = 0.1)
              )   
            )
          )
        ),
        conditionalPanel(
          "input.type == 'custom'",
          ns = ns,
          textInput("customcode", "Custom code")
        )
      ),
      button = modalButtonUI(ns("modal"), NULL, icon = icon("pen")),
      footer = actionButton(ns("confirm"), "Confirm", `data-dismiss` = "modal")
    ),
    actionButton(ns("delete"), NULL, icon = icon("trash-alt")),
    textOutput(ns("name"), inline = TRUE)
  )
}

edit_panel_server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      state <- reactiveVal(NULL)
      showModalUI(ns("modal"))
      
      observeEvent(input$confirm, {
        state(get_state(input))
        session$userData$vars[[id]] <- state()
        session$userData$clear(session$userData$clear() + 1)
      })
      
      output$name <- renderText({
        state()$name
      })
      
      observeEvent(input$delete, {
        session$userData$vars[[id]] <- NULL
        removeUI(paste0("#", ns("container")))
      }, ignoreInit = TRUE)
    }
  )
}

ui <- fluidPage(
  tags$head(
    shiny::tags$script(type = "text/javascript", src = "hidden_mode.js")
  ),
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
        "input.nrow > 0 & $('#variables > div').length > 0", # exercise for conditionalPanel
        actionButton("run", NULL, icon = icon("play"), width = "100%")  
      )
    ),
    mainPanel(
      DT::dataTableOutput("table")
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
    pageLength = 10
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
