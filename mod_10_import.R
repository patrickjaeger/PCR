# Different PCR machine machines deliver different file formats and
# formatting. Here the user selects his machine to ensure consistent
# data import.
# TODO Finish read_96()
# TODO Add error message if file format is incompatible


# Modules -----------------------------------------------------------------

importUI <- function(id) {
  tagList(
    
    h3('Upload Data'),
    selectInput(NS(id, 'machine'), 
                'Select PCR machine:',
                choices = c('384-well beast',
                            '96-well old-school')),
    fileInput(NS(id, 'file'), 
              'Please upload file:')
    
  )
}

importServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Return tibble with Cq values, etc.
    
    reactive({
      req(input$file)
      input$file$datapath %>% 
        if_else(input$machine == '384-well beast',
                read_384(.),
                read_96(.))
    })
    
  })
}


# Utils -------------------------------------------------------------------

read_384 <- function(.fpath) {
  # Read CSV containing Cq values from 384-well PCR machine
  .fpath %>% 
    read_csv() %>% 
    select(Well, Target, Content, Sample, Cq) %>%
    rename_with(tolower)
}

read_96 <- function(.fpath) {
  # Read CSV containing Cq values from 96-well PCR machine
  # TODO Write function :O
  # .fpath %>% 
  #   read_csv() %>% 
  #   select(Well, Target, Content, Sample, Cq) %>%
  #   rename_with(tolower)
}