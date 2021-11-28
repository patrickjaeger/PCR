library(tidyverse)
library(shinyjqui)
library(shiny)
source('utils.R')
source('mod_10_import.R')
source('mod_20_remove-blanks.R')

ui <- navbarPage(
  'PCRmate',
  
  tabPanel('Upload',
           mainPanel(
             importUI('file1'),
             
             blanksUI('blank1'),
             
             h3('Calculate Stuff'),
             selectInput('housek', 
                         'Select housekeeper:',
                         choices = c()),
             selectInput('ctrl',
                         'Select control group:',
                         choices = c(),),
             actionButton('calculate', 'Calculate Relative Gene Expression'),
             downloadButton('d_res_long', 'Download Results (long format)'),
             downloadButton('d_res_wide', 'Download Results (wide format)'),
             tableOutput('head_res_long'),
             
             h3('Extract Tags'),
             textInput('tags', 
                       'Enter names of tags to extract:', 
                       placeholder = 'donor genotype sample'),
             orderInput('arrange_tags', 
                        'Arrange by:', 
                        items = c('B', 'A', 'N', 'A', 'N', 'A')),
             tableOutput('head_res2_long'),
             downloadButton('d_res2_long', 'Download Results (long format)'),
             downloadButton('d_res2_wide', 'Download Results (wide format)')
           )
  ),
  
  tabPanel('Plot'),
  tabPanel('Info')
  
)

server <- function(input, output, session) {
  dat_raw <- importServer('file1')
  dat <- blanksServer('blank1', dat_raw)
  
  # observeEvent(dat_raw(), print(dat_raw()))
  
  
}

shinyApp(ui, server)