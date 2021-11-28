# PCR data comes with multiple technical replicates that need to be averaged
# before calculating the dCq, ddCq, etc. If their values are to far apart,
# indicates that the pipetting wasn't very professional.

# TODO Fix replicatesServer() output - it keeps crashing

# Modules -----------------------------------------------------------------

replicatesUI <- function(id) {
  tagList(
    
    h3('Check technical replicates'),
    actionButton(NS(id, 'check_replicates'), 'Check'),
    textOutput(NS(id, 'replicates_message')),
    tableOutput(NS(id, 'replicates_table'))
    
    )
}


replicatesServer <- function(id, .dat, .thresh = 0.3) {
  moduleServer(id, function(input, output, session) {
    
    # Average Cq values
    dat_avg <- reactive({
      req(.dat())
      .dat() %>%
        select(-content) %>% 
        group_by(target, sample) %>% 
        summarise(mean_cq = mean(cq),
                  sd_cq = sd(cq) %>% round(2), 
                  .groups = 'drop')
    })
    
    ## Check SD of technical replicates and save message
    high_sd_samples <- 
      eventReactive(input$check_replicates, {
        dat_avg() %>% filter(sd_cq > .thresh)
      })
    
    replicates_message <- eventReactive(input$check_replicates, {
      if (nrow(high_sd_samples()) > 0)
        'Attention! High variation between technical replicates detected.'
      if (nrow(high_sd_samples()) == 0)
        'Variation among technical replicates is acceptable, continue.'
    })
    observeEvent(replicates_message(), print(replicates_message()))
    # Outputs
    # output$replicates_message <- renderText({replicates_message()})
    # output$replicates_table <- renderTable(high_sd_samples())

    # return(dat_avg())

  })
}
