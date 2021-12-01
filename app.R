library(tidyverse)
library(shinyjqui)
library(shinyjs)
library(shiny)


source('utils.R')
source('mod_10_import.R')
source('mod_20_remove-blanks.R')
source('mod_30_check-tech-replicats.R')
source('mod_40_gene-expr.R')

ui <- navbarPage(
  'PCRmate',
  
  tabPanel('Upload',
           mainPanel(
             
             importUI('file1'),
             
             blanksUI('blank1'),
             replicatesUI('rep1'),
             calculationUI('calc1'),
             
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
  dat1 <- importServer('file1')
  dat2 <- blanksServer('blank1', dat1)
  dat3 <- replicatesServer('rep1', dat2)
  # dat4 <- calculationServer('calc1', dat3)
  # observeEvent(dat3(), print('fu'))
  
  
}

shinyApp(ui, server)