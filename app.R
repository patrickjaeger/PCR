library(tidyverse)
library(shinyjqui)
library(shiny)
source('utils.R')

ui <- navbarPage(
  'PCRmate',
  
  tabPanel('Upload',
    mainPanel(
      h3('Upload Data'),
      selectInput('pcr_machine', 
                  'Select your PCR machine:',
                  choices = c('384-well beast',
                              '96-well old-school')),
      fileInput('file', 'Please upload file:'),
      
      h3('Check Blanks'),
      textInput('blank_tag', 'Enter name of blanks:', value = 'Neg Ctrl'),
      actionButton('check_blanks', 'Check blanks'),
      tableOutput('blanks_table'),
      
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
  dat_raw <- reactive({
    req(input$file)
    input$file$datapath %>% 
      read_csv() %>% 
      select(Well, Target, Content, Sample, Cq) %>%
      rename_with(tolower)
  })
  
  
  
  
  
}

shinyApp(ui, server)