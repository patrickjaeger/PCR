# Info --------------------------------------------------------------------

# Different PCR machines deliver different file formats and
# formatting. Here the user selects his machine to ensure consistent
# data import.

# TODO Fix feedback for blank selection line ~50
# TODO Add biological column(s)
# TODO Check NA values in cq - then warn and remove them
# TODO Validate correct readouts with someone more experienced with PCR


# I/O ---------------------------------------------------------------------
## Input
# - CSV.files only; main table only (= without supple. info at the top/bottom)
# - machines: StepOnePlus, xxx
# - 

## Output

# dat_raw (tibble [n x 6])
# - well (chr)
# - target (chr)
# - content (chr)
# - biological (chr)
# - sample (chr)
# - cq (dbl)


# Modules -----------------------------------------------------------------

importUI <- function(id) {
  tagList(
    shinyFeedback::useShinyFeedback(),
    h3('Upload Data'),
    selectInput(NS(id, 'machine'), 
                'Select PCR machine:',
                choices = c('384-well beast',
                            '96-well old-school')),
    uiOutput(NS(id, 'blankUI')),
    fileInput(NS(id, 'file'), 
              'Please upload file:')
    
  )
}

importServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Render text input for blank designation if option 96-well is chosen
    output$blankUI <- renderUI({
      validate(need(input$machine == '96-well old-school', ''))
      tagList(
        textInput(NS(id, 'blank'), 'Enter name of blank (case sensitive):')
      )
    })
    
    # Return tibble with Cq values, etc.
    dat_raw <- reactive({
      req(input$file)
      
      switch (input$machine,
              `384-well beast` = read_384(input$file$datapath),
              `96-well old-school` = read_96(input$file$datapath, input$blank)
      )
    })
    
    observeEvent(input$blank, {
      req(dat_raw())
      
      if (str_detect(input$blank, dat_raw()$content)) {
        shinyFeedback::hideFeedback('blank')
      } else {
        shinyFeedback::feedbackDanger('blank', 'Blanks not found.')
      }
      
      # shinyFeedback::feedbackDanger('blank', 
      #                               !str_detect(input$blank, dat_raw()$content),
      #                               'Blanks not found!')
      
    })
    
    return(dat_raw)
  })
}


# Utils -------------------------------------------------------------------

read_384 <- function(.fpath) {
  # Read CSV containing Cq values from 384-well PCR machine
  
  dat <- read_csv(.fpath)
  
  exist_biological <- 'Biological Set Name' %in% names(dat)
  
  if (exist_biological) {
    dat %>% 
      select(Well, Target, Content, Sample, Cq, 
             biological = `Biological Set Name`) %>%
      rename_with(tolower)
  } else {
    dat %>% 
      select(Well, Target, Content, Sample, Cq) %>%
      rename_with(tolower) %>% 
      mutate(biological = NA)
  }
  
}

read_96 <- function(.fpath, .blank) {
  # Read the .csv file from the steponeplus. The machine saves an .xls, which
  # must first be saved as .csv and the garbage must be deleted from the 
  # beginning and end.
  # .blank (chr): name given to blank wells
  
  read_csv(.fpath) %>% 
    select(Well, `Sample Name`, `Target Name`, `C<U+0442>`) %>% 
    rename(well = Well, sample = `Sample Name`, target = `Target Name`, 
           cq = `C<U+0442>`) %>% 
    mutate(
      # Replace <Undetermined> with <NA> in cq
      cq = ifelse(cq == 'Undetermined', NA, cq) %>% as.numeric(),
      # Tag row containing samples and blanks in content
      content = if_else(sample == .blank, .blank, 'Unknown'),
      # Create dummy biologica column
      biological = NA
    )
}


# read_96 test ------------------------------------------------------------

# .fpath <- 'data/210816_patrick_2d_stretch_n4.csv'
# t1 <- read_csv(.fpath) %>%
#   # slice(1:(nrow(.)-5)) %>%
#   select(Well, `Sample Name`, `Target Name`, `C<U+0442>`) %>%
#   rename(well = Well, sample = `Sample Name`, target = `Target Name`,
#          cq = `C<U+0442>`) %>%
#   mutate(cq = ifelse(cq == 'Undetermined', NA, cq) %>% as.numeric())
# tail(t1)
# blank <- 'BLANK'
# t1 %>% mutate(content = if_else(sample == blank, blank, 'Unknown'))


# TestApp -----------------------------------------------------------------

importApp <- function() {
  ui <- fluidPage(
    importUI('file1'),
    tableOutput('table1')
  )
  server <- function(input, output, session) {
    dat_raw <- importServer('file1')
    output$table1 <- renderTable(dat_raw())
  }
  shinyApp(ui, server)
}

importApp()

