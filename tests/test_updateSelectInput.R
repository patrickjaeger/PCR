# Demo of updateSelectInput inside a module

updateInputUI <- function(id) {
  tagList(
    textInput(NS(id, 'text1'), 'Text'),
    selectInput(NS(id, 'list1'), 'List', choices = c()),
    textOutput(NS(id, 'text_out'))
    
  )
}

# WTF is this?!
# testServer() 

updateInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$text1, {
      req(input$text1)
      updateSelectInput(inputId = 'list1', 
                        choices = str_split(input$text1, ' ')[[1]])  
    })
    
    output$text_out <- renderText({input$list1})
    
  })
}


App <- function() {
  ui <- fluidPage(
    updateInputUI('test1')
  )
  server <- function(input, output, session) {
    updateInputServer('test1')
  }
  shinyApp(ui, server)  
}

App()