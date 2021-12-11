# Source ------------------------------------------------------------------
library(tidyverse)
library(shinyjqui)
library(shinyjs)
library(shinyFeedback)
library(shiny)

source('mod_10_import.R')
source('mod_20_remove-blanks.R')
source('mod_21_check-empty-samples.R')
source('mod_30_check-tech-replicats.R')
source('mod_40_gene-expr.R')


# TODO --------------------------------------------------------------------

# TODO App chrashes if input method is switched after some calculation 
# have been made
# TODO Reset app when another dataset is loaded
# TODO Average control group for calculations, e.g. average across WT in 
# Greta's data
# TODO Implement plotting
# TODO Add multiple text inputs to &-filter for the control group
# TODO Handle multiple entries in biological
# TODO Check how/if the control group is averaged


# UI ----------------------------------------------------------------------

ui <- navbarPage(
  'PCRmate',
  
  tabPanel('Upload',
           mainPanel(
             
             importUI('file1'),
             blanksUI('blank1'),
             checkSamplesUI('samples1'),
             replicatesUI('rep1'),
             calculationUI('calc1')
             
           )
  ),
  
  tabPanel('Plot'),
  tabPanel('Info')
  
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  dat1 <- importServer('file1')
  dat2 <- blanksServer('blank1', dat1)
  dat2.1 <- checkSamplesServer('samples1', dat2)
  dat3 <- replicatesServer('rep1', dat2.1)
  dat4 <- calculationServer('calc1', dat3)
  # observeEvent(dat3(), print('fu'))
  
}

shinyApp(ui, server)