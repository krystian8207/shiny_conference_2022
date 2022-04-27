library(dplyr)

genid <- function() {
  paste(sample(letters, 5), collapse = "")
}

get_state <- function(input) {
  state <- reactiveValuesToList(input)
  return(
    state[setdiff(names(state), c("confirm", "delete"))] 
  )
}

gen_col_character <- function(state, n_rows) {
  if (state$charminmax[1] == state$charminmax[2]) {
    nchars <- state$charminmax[1]
  } else {
    nchars <- sample(state$charminmax[1]:state$charminmax[2], n_rows, replace = TRUE)
  }
  return(
    stringi::stri_rand_strings(n_rows, nchars, state$charpattern) 
  )
}

gen_col_numeric <- function(state, n_rows) {
  if (state$numtype == "numrange") {
    res <- runif(n_rows, min = state$numminmax[1], max = state$numminmax[2])
  }
  if (state$numtype == "numdistro") {
    res <- switch(
      state$numdistro,
      "normal" = rnorm(n_rows, mean = state$normal_mean, sd = state$normal_sd),
      "exponential" = rexp(n_rows, rate = state$exponential_rate)
    )
  }
  return(res)
}

gen_col_integer <- function(state, n_rows) {
  if (state$inttype == "intrange") {
    res <- sample(state$intminmax[1]:state$intminmax[2], n_rows, replace = TRUE)
  }
  if (state$inttype == "intdistro") {
    if (state$intdistro == "binomial") {
      res <- rbinom(n_rows, size = state$binomial_size, prob = state$binomial_prob)
    }
    if (state$intdistro == "poisson") {
      res <- rpois(n_rows, lambda = state$poisson_lambda)
    }
  }
  return(res)
}
 
gen_col_custom <- function(state, n_rows) {
  eval(parse(text = state$customcode))
}

gen_column <- function(state, n_rows) {
  method <- switch(
    state$type,
    "numeric" = gen_col_numeric,
    "integer" = gen_col_integer,
    "character" = gen_col_character,
    "custom" = gen_col_custom
  )
  res <- list(method(state, n_rows))
  names(res) <- state$name
  return(res)
}

gen_table <- function(states, n_rows) {
  purrr::map_dfc(unname(states), gen_column, n_rows = n_rows)
}

edit_panel_ui <- function(id, name) {
  ns <- NS(id)
  
  wellPanel(
    id = id,
    modalDialogUI(
      modalId = ns("modal"),
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
        textInput(ns("customcode"), "Custom code")
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
      state <- reactiveVal(NULL)
      showModalUI("modal")
      
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
        removeUI(glue("#{id}"))
      })
    }
  )
}
