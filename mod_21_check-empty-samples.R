# Sometimes wells with samples fail to deliver a signal. Here, we check
# whether this happened to some samples.


# Modules -----------------------------------------------------------------

checkSamplesUI <- function(id) {
  tagList(
    
    h3('Check samples'),
    actionButton(NS(id, 'check_samples'), 'Check samples'),
    textOutput(NS(id, 'empty_message')),
    tableOutput(NS(id, 'empty_table'))
    
  )
}

checkSamplesServer <- function(id, .dat) {
  moduleServer(id, function(input, output, session) {
    
    # Get samples that don't have a signal
    empty_samples <- eventReactive(input$check_samples, {
      req(.dat())
      empty_samples <- filter(.dat(), is.na(cq))
      
      list(
        table = empty_samples,
        message = ifelse(nrow(empty_samples) > 0,
                         'Warning! Samples without signal detected.',
                         'Yay! All samples have a signal.')
      )
    })
    
    # Render message and table of empty samples
    output$empty_message <- renderText({
      req(empty_samples()$message)
      empty_samples()$message
    })
    
    output$empty_table <- renderTable({
      req(nrow(empty_samples()$table) > 0)
      empty_samples()$table
    })
    
    # Return data without empty samples
    eventReactive(input$check_samples, na.omit(.dat()))
    
  })
}

