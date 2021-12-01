library(tidyverse)
# library(shinyFeedback)
library(shiny)


# Mastering Shiny: 8.1.4 Validate output
# Demo of shinyFeedback
feedbackApp <- function() {
ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  textInput("dataset", "Dataset name"), 
  tableOutput("data")
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$dataset)
    
    exists <- exists(input$dataset, "package:datasets")
    shinyFeedback::feedbackDanger("dataset", !exists, "Unknown dataset")
    req(exists, cancelOutput = TRUE)
    
    get(input$dataset, "package:datasets")
  })
  
  output$data <- renderTable({
    head(data())
  })
}

shinyApp(ui, server)  
}

# feedbackApp()


# Test feedback with textInput --------------------------------------------

textFeedbackApp <- function() {
  ui <- fluidPage(
    shinyFeedback::useShinyFeedback(),
    textInput('text_in', 'Enter "a" or "b":'),
    textOutput('text_out')
  )
  
  server <- function(input, output, session) {
    
    text_out <- reactive({
      req(input$text_in)

      pass <- input$text_in %in% c('a', 'b')
      
      if (pass) {
        shinyFeedback::hideFeedback('text_in')
        shinyFeedback::showFeedbackSuccess('text_in', "Entry matched")
        # input$text_in
        `$`(input, 'text_in')
      } else {
        shinyFeedback::hideFeedback('text_in')
        shinyFeedback::showFeedbackWarning('text_in', "Entry not found")
      }
      
      # switch_feedback('text_in',
      #                 pass,
      #                 "Entry matched",
      #                 "Entry not found")

    })

    output$text_out <- renderText({text_out()})
  }
  shinyApp(ui, server)  
}

textFeedbackApp()


switch_feedback <- function(.input, .show, .pass_message, .fail_message){
  # Switch beteen FeedbackSuccess and FeedbackDanger depending on
  # the input.
  
  if (.show) {
    shinyFeedback::hideFeedback(.inputID)
    shinyFeedback::showFeedbackSuccess(.inputID, .pass_message)
    # `$`(get('input', envir = rlang::caller_env(-2)), .inputID)
    # `$`(.inputID, 'text_in')
    .input
  } else {
    shinyFeedback::hideFeedback(.inputID)
    shinyFeedback::showFeedbackDanger(.inputID, .fail_message)
  }
}
