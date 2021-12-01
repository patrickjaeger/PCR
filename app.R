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
             calculationUI('calc1')
             
           )
  ),
  
  tabPanel('Plot'),
  tabPanel('Info')
  
)

server <- function(input, output, session) {
  dat1 <- importServer('file1')
  dat2 <- blanksServer('blank1', dat1)
  dat3 <- replicatesServer('rep1', dat2)
  dat4 <- calculationServer('calc1', dat3)
  # observeEvent(dat3(), print('fu'))
  
  
}

shinyApp(ui, server)