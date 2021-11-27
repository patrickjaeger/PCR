library(tidyverse)
library(shiny)
source('utils.R')

ui <- navbarPage(
  'PCRmate',
  tabPanel(
    mainPanel(
      fileInput('file')
    )
  )
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)