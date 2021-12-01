# Disabled ----------------------------------------------------------------

if (interactive()) {
  library(shiny)
  shinyApp(
    ui = fluidPage(
      useShinyjs(),  # Set up shinyjs
      actionButton("btn", "Click me"),
      disabled(
        textInput("element", NULL, "I was born disabled")
      )
    ),
    server = function(input, output) {
      observeEvent(input$btn, {
        enable("element")
      })
    }
  )
}

# Inside module -----------------------------------------------------------

buttonUI<- function(id) {
  tagList(
    shinyjs::useShinyjs(),
    actionButton(NS(id, 'b1'), 'push it'),
    actionButton(NS(id, 'b2'), 'push it 2'),
    downloadButton(NS(id, 'b3'), 'push it 3')
  )
}

buttonServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$b1, shinyjs::toggleState('b3'))
  })
}

disableApp <- function() {
  ui <- fluidPage(
    buttonUI('butt')
  )
  server <- function(input, output, session) {
    buttonServer('butt')
  }
  shinyApp(ui, server)  
}

disableApp()

# Example from doc --------------------------------------------------------

if (interactive()) {
  library(shiny)
  
  shinyApp(
    ui = fluidPage(
      useShinyjs(),  # Set up shinyjs
      actionButton("btn", "Click me"),
      # textInput("element", "Watch what happens to me"),
      actionButton("element", "Watch what happens to me")
    ),
    server = function(input, output) {
      observeEvent(input$btn, {
        # Change the following line for more examples
        toggleState("element")
      })
    }
  )
}
## Not run: 
# The shinyjs function call in the above app can be replaced by
# any of the following examples to produce similar Shiny apps
toggleState(id = "element")
enable("element")
disable("element")

# Similarly, the "element" text input can be changed to many other
# input tags, such as the following examples
actionButton("element", "I'm a button")
fileInput("element", "Choose a file")
selectInput("element", "I'm a select box", 1:10)

## End(Not run)

## toggleState can be given an optional `condition` argument, which
## determines if to enable or disable the input
if (interactive()) {
  shinyApp(
    ui = fluidPage(
      useShinyjs(),
      textInput("text", "Please type at least 3 characters"),
      actionButton("element", "Submit")
    ),
    server = function(input, output) {
      observe({
        toggleState(id = "element", condition = nchar(input$text) >= 3)
      })
    }
  )
}