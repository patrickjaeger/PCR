App <- function() {
  ui <- fluidPage(
    textInput('t1', 'Text'),
    actionButton('b1', 'Push it'),
    textOutput('t2')
  )
  server <- function(input, output, session) {
    tt <- reactive({
      validate(need(input$t1, 'fuuuuuuu'))
      paste(input$t1, 'fufufu')
    })
    
    message <- eventReactive(input$b1, {
      
      paste('FU', tt())
    })
    
    output$t2 <- renderText(message())
  }
  shinyApp(ui, server)  
}
App()