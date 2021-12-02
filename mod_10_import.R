# Different PCR machines deliver different file formats and
# formatting. Here the user selects his machine to ensure consistent
# data import.
# TODO Finish read_96() - cq is still character
# TODO Add error message if file format is incompatible
# TODO Validate correct readouts with someone more experienced with PCR

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
      if (input$machine == '384-well beast')
        read_384(input$file$datapath)
      # if (input$machine == '96-well old-school')
      #   read_96(input$file$datapath)
    })
    
  })
}


# Utils -------------------------------------------------------------------

read_384 <- function(.fpath) {
  # Read CSV containing Cq values from 384-well PCR machine
  .fpath %>% 
    read_csv() %>% 
    select(Well, Target, Content, Sample, Cq) %>%
    rename_with(tolower) %>% 
    mutate(target = toupper(target))
}

read_96 <- function(.fpath) {
  # Read the .xls file and cut the garbage from
  # the beginning and end
  readxl::read_xls(.fpath, skip = 7) %>% 
    slice(1:(nrow(.)-5)) %>% 
    select(Well, `Sample Name`, `Target Name`, `C<U+0442>`) %>% 
    rename(well = Well, sample = `Sample Name`, target = `Target Name`, 
           cq = `C<U+0442>`) %>% 
    mutate(cq = as.character(cq))
}

# read_96 test ------------------------------------------------------------

# .fpath <- 'data/210816_patrick_2d_stretch_n4.xls'
# t1 <- readxl::read_xls(.fpath, skip = 7) %>% 
#   slice(1:(nrow(.)-5)) %>% 
#   select(Well, `Sample Name`, `Target Name`, `C<U+0442>`) %>% 
#   rename(well = Well, sample = `Sample Name`, target = `Target Name`, 
#          cq = `C<U+0442>`) %>%  
#   mutate(cq = as.character(cq)) 
# 
# blank <- 'BLANK'
# t1 %>% mutate(content = if)

# TestApp -----------------------------------------------------------------

# importApp <- function() {
#   ui <- fluidPage(
#     importUI('file1')
#   )
#   server <- function(input, output, session) {
#     dat_raw <- importServer('file1')
#   }
#   shinyApp(ui, server)  
# }
# 
# importApp()


# xlsx test ---------------------------------------------------------------

# file.exists('data/Greta_150921_Mousecells_VHLKO -  Quantification Summary.xlsx')
# readxl::read_xlsx('data/Greta_150921_Mousecells_VHLKO -  Quantification Summary.xlsx')
