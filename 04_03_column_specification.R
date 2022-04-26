library(shiny)
library(shinyGizmo)
library(magrittr)
library(glue)

source("tools.R")

name <- "age"

ui <- fluidPage(
  wellPanel(
    id = "container",
    modalDialogUI(
      modalId = "modal",
      textInput("name", "Column name", name),
      selectInput(
        "type", "Column type", 
        choices = c("numeric", "integer", "character", "custom"),
        selected = "numeric"
      ),
      conditionalPanel(
        "input.type == 'numeric'", 
        tabsetPanel(
          id = "numtype",
          tabPanel(
            "Range", 
            value = "numrange",
            sliderInput("numminmax", "Column range", min = 1, max = 20, value = c(5, 10), step = 1)    
          ),
          tabPanel(
            "Distribution",
            value = "numdistro",
            selectInput("numdistro", "Distribution", choices = c("normal", "exponential"), selected = "normal"),
            conditionalPanel(
              "input.numdistro == 'normal'",  
              numericInput("normal_mean", "Mean", value = 0),
              numericInput("normal_sd", "SD", value = 1, min = 0)
            ),
            conditionalPanel(
              "input.numdistro == 'exponential'",  
              numericInput("exponential_lambda", "Lambda", value = 0)
            )
          )
        )
      ),
      conditionalPanel(
        "input.type == 'character'", 
        sliderInput("charminmax", "Number of characters", min = 1, max = 20, value = c(5, 10), step = 1),
        selectInput("charpattern", "Pattern", choices = c("[[a-z][A-Z]]", "[a-z]", "[A-Z]", "[a-zA-Z0-9]"))
      ),
      conditionalPanel(
        "input.type == 'integer'",
        tabsetPanel(
          id = "inttype",
          tabPanel(
            "Range", 
            value = "intrange",
            sliderInput("intminmax", "Column range", min = 1, max = 20, value = c(5, 10), step = 1)    
          ),
          tabPanel(
            "Distribution",
            value = "intdistro",
            selectInput("intdistro", "Distribution", choices = c("binomial", "poisson"), selected = "binomial"),
            conditionalPanel(
              "input.intdistro == 'binomial'",  
              numericInput("binomial_size", "Size", value = 10, min = 0, step = 1),
              numericInput("binomial_prob", "Prob", value = 0.5, min = 0, max = 1)
            ),
            conditionalPanel(
              "input.intdistro == 'poisson'",  
              numericInput("poisson_lambda", "Lambda", value = 1, min = 0.1)
            )   
          )
        )
      ),
      conditionalPanel(
        "input.type == 'custom'",
        textInput("customcode", "Custom code")
      ),
      button = modalButtonUI("modal", NULL, icon = icon("pen")),
      footer = actionButton("confirm", "Confirm", `data-dismiss` = "modal")
    ),
    actionButton("delete", NULL, icon = icon("trash-alt")),
    textOutput("name", inline = TRUE)
  )
)

server <- function(input, output, session) {
  session$userData$clear <- reactiveVal(1)
  
  state <- reactiveVal(NULL)
  
  observeEvent(input$confirm, {
    state(get_state(input))
    session$userData$vars[[name]] <- state()
    session$userData$clear(session$userData$clear() + 1)
  })
  
  output$name <- renderText({
    state()$name
  })
  
  observeEvent(input$delete, {
    session$userData$vars[[name]] <- NULL
    removeUI(paste0("#", "container"))
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)