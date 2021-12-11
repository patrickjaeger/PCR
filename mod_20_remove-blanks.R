# PCR is done with wells that contain no mRNA as blank controls to 
# check for mRNA contamination in the reagent mix. These blanks should 
# be blank. Here the blanks are checked and removed from the main data.


# Modules -----------------------------------------------------------------

blanksUI <- function(id) {
  tagList(
    
    h3('Check Blanks'),
    textInput(NS(id, 'blank_tag'), 
              'Enter name of blanks:', 
              value = 'Neg Ctrl'),
    actionButton(NS(id, 'check_blanks'), 'Check blanks'),
    textOutput(NS(id, 'blanks_message')),
    tableOutput(NS(id, 'blanks_table'))
    
  )
}


blanksServer <- function(id, .dat_raw) {
  moduleServer(id, function(input, output, session) {
    
    # Filter rows with blanks
    blanks <- eventReactive(input$check_blanks, {
      req(.dat_raw())
      filter(.dat_raw(), str_detect(content, input$blank_tag))
    })
    
    # Check and create message
    blanks_message <- reactive({
      validate(need(nrow(blanks()) != 0, 
                    'Blanks not found. Correct the name!'))
      
      if (sum(blanks()$cq, na.rm = TRUE) == 0) {
        'Yay! All blanks are blank.'
      } else {
        'Warning! One or more blanks are not blank.'
      }
    })
    
    # Create table of corrupt blanks, if detected
    blanks_table <- reactive({
      validate(need(nrow(filter(blanks(), !is.na(cq))) > 0, ''))
      filter(blanks(), !is.na(cq))
    })
    
    # Render message and table of blanks
    output$blanks_message <- renderText({blanks_message()})
    output$blanks_table <- renderTable(blanks_table())
    
    # Return dat_raw without the blanks
    observeEvent(input$check_blanks, 
                 filter(.dat_raw(), 
                        !str_detect(content, input$blank_tag)))
    
  })
}
