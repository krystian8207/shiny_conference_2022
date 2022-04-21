library(shiny)
library(shinyGizmo)
library(magrittr)

panel_state = function(name = "") {
  list(
    name = name, 
    type = "numeric",
    numtype = "numrange",
    numminmax = c(5, 10),
    numdistro = "normal",
    charminmax = c(5, 10),
    inttype = "intrange",
    intminmax = c(5, 10),
    intdistro = "binomial"
  )
}

gen_column <- function(state, n_rows) {
  if (state$type == "numeric") {
    if (state$numtype == "numrange") {
      res <- runif(n_rows, min = state$numminmax[1], max = state$numminmax[2])
    }
    if (state$numtype == "numdistro") {
      method <- switch(
        state$numdistro, 
        "normal" = rnorm,
        "exponential" = rexp
      )
      res <- method(n = n_rows)
    }
  }
  if (state$type == "character") {
    res <- stringi::stri_rand_strings(
      n_rows, sample(state$charminmax[1]:state$charminmax[2], n_rows, replace = TRUE), "[[a-z][A-Z]]"
    )
  }
  if (state$type == "integer") {
    if (state$inttype == "intrange") {
      res <- sample(state$intminmax[1]:state$intminmax[2], n_rows, replace = TRUE)
    }
    if (state$inttype == "intdistro") {
      if (state$intdistro == "binomial") {
        res <- rbinom(n_rows, size = 20, prob = 0.5)
      }
      if (state$intdistro == "poisson") {
        res <- rpois(n_rows, lambda = 10)
      }
    }
  }
  return(res)
}

get_state <- function(input) {
  input_names <- names(panel_state())
  input_names %>% 
    purrr::map(~ input[[.]]) %>% 
    stats::setNames(input_names)
}

create_modal <- function(state = panel_state(), edit_name = FALSE, ns = NS(NULL)) {
  tagList(
    if (edit_name) {
      textInput(ns("name"), "Column name", state$name)
    },
    selectInput(
      ns("type"), "Column type", 
      choices = c("numeric", "integer", "character", "custom"),
      selected = state$type
    ),
    conditionalPanel(
      "input.type == 'numeric'", 
      ns = ns,
      tabsetPanel(
        id = ns("numtype"),
        selected = state$numtype,
        tabPanel(
          "Range", 
          value = "numrange",
          sliderInput(ns("numminmax"), "Column range", min = 1, max = 20, value = state$numminmax, step = 1)    
        ),
        tabPanel(
          "Distribution",
          value = "numdistro",
          selectInput(ns("numdistro"), "Distribution", choices = c("normal", "exponential"), selected = state$numdistro)   
        )
      )
    ),
    conditionalPanel(
      "input.type == 'character'", 
      ns = ns,
      sliderInput(ns("charminmax"), "Number of characters", min = 1, max = 20, value = state$charminmax, step = 1)
    ),
    conditionalPanel(
      "input.type == 'integer'",
      ns = ns,
      tabsetPanel(
        id = ns("inttype"),
        selected = state$inttype,
        tabPanel(
          "Range", 
          value = "intrange",
          sliderInput(ns("intminmax"), "Column range", min = 1, max = 20, value = state$intminmax, step = 1)    
        ),
        tabPanel(
          "Distribution",
          value = "intdistro",
          selectInput(ns("intdistro"), "Distribution", choices = c("binomial", "poisson"), selected = state$intdistro)   
        )
      )
    )
  )
}

edit_panel_ui <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    id = ns("container"),
    actionButton(ns("edit"), NULL, icon = icon("pen")),
    actionButton(ns("delete"), NULL, icon = icon("trash-alt")),
    textOutput(ns("name"), inline = TRUE)
  )
}

edit_panel_server <- function(id, var_state) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      state <- reactiveVal(var_state)
      
      observeEvent(input$edit, {
        showModal(
          modalDialog(
            create_modal(
              ns = ns,
              edit_name = TRUE,
              state = state()
            ),
            footer = tagList(
              actionButton(ns("confirm"), "Confirm"),
              modalButton("Dismiss")
            )
          )
        )  
      }, ignoreInit = TRUE)
      
      observeEvent(input$confirm, {
        old_name <- state()$name
        state(get_state(input))
        
        session$userData$vars[[old_name]] <- NULL
        session$userData$vars[[state()$name]] <- state()
        removeModal()
      })
    
      output$name <- renderText({
        state()$name
      })
      
      observeEvent(input$delete, {
        session$userData$vars[[state()$name]] <- NULL
        removeUI(paste0("#", ns("container")))
      }, ignoreInit = TRUE)
      
    }
  )
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("nrow", "Number of rows", value = 50, min = 1, max = 1000, step = 1),
      div(id = "managing"),
      textInput("name", "Column name"),
      conditionalPanel(
        "input.name != ''",
        actionButton("new", NULL, icon = icon("plus"), width = "100%")  
      ),
      conditionalPanel(
        "input.nrow > 0", # exercise for conditionalPanel
        actionButton("run", NULL, icon = icon("play"), width = "100%")  
      )
    ),
    mainPanel(
      downloadButton("downloadData", NULL, style = "position: fixed; top: 3px; right: 3px;"), # optional
      tabsetPanel(
        tabPanel("Table content", DT::dataTableOutput("table")),
        tabPanel("Summary", htmlOutput("summary")) # optional
      )
    )
  )
)

genid <- function() {
  paste(sample(letters, 5), collapse = "")
}

server <- function(input, output, session) {
  session$userData$vars <- list()
  res_table <- reactiveVal(NULL)
  current_id <- reactiveVal(NULL)

  observeEvent(input$new, {
    current_id(genid())
    insertUI(
      "#managing",
      where = "beforeEnd",
      edit_panel_ui(current_id())
    )
    
    showModal(
      modalDialog(
        create_modal(),
        title = input$name,
        footer = actionButton("create", "Create")
      )
    )
  })
  
  observeEvent(input$create, {
    create_state <- get_state(input)
    create_state$name <- input$name
    session$userData$vars[[input$name]] <- create_state
    edit_panel_server(current_id(), var_state = create_state)
    removeModal()
    updateTextInput(inputId = "name", value = "")
  })
  
  observeEvent(input$run, {
    res_table(
      session$userData$vars %>% 
        purrr::map_dfc(gen_column, n_rows = input$nrow) 
    )
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
  
  output$summary <- renderUI({
    validate(need(
      !is.null(res_table()),
      "No table created."
    ))
    print(
      summarytools::dfSummary(res_table()),
      footnote = "",
      method = "render",
      headings = FALSE
    ) %>% paste(collapse = " ") %>% 
      HTML()
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