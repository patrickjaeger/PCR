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
    blanks <- reactive({
      req(.dat_raw())
      filter(.dat_raw(), str_detect(content, input$blank_tag))
    })
    
    # Check and save message
    blanks_message <- eventReactive(input$check_blanks, {
      validate(need(nrow(blanks()) != 0, 
                    'Blanks not found. Correct the name!'))
      
      if (sum(blanks()$cq, na.rm = TRUE) == 0) 
        'Blanks are blank, continue.'
      if (sum(blanks()$cq, na.rm = TRUE) > 0) 
        'Warning! One or more blanks are not blank.'
    })
    
    # Save table of corrupt blanks, if detected
    blanks_table <- eventReactive(input$check_blanks, {
      validate(need(nrow(blanks()) != 0, ''))
      filter(blanks(), !is.na(cq))
      })
    
    # Return message and table of blanks
    output$blanks_message <- renderText({blanks_message()})
    output$blanks_table <- renderTable(blanks_table())
    
    # Return dat_raw without the blanks
    reactive(filter(.dat_raw(), !str_detect(content, input$blank_tag)))
    
  })
}
