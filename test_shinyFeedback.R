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
    
    # valid_text_input <- c('a', 'fu')
    
    text_out <- reactive({
      req(input$text_in)
      
      pass <- input$text_in %in% c('a', 'b')
      
      shinyFeedback::feedbackDanger("text_in", !pass, "Entry not found")
      req(pass, cancelOutput = TRUE)
      
      input$text_in
    })
    
    output$text_out <- renderText({text_out()})
  }
  shinyApp(ui, server)  
}

textFeedbackApp()