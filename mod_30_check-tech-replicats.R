# PCR data comes with multiple technical replicates that need to be averaged
# before calculating the dCq, ddCq, etc. If their values are to far apart,
# indicates that the pipetting wasn't very professional.


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
    dat_avg <- eventReactive(input$check_replicates, {
      req(.dat())
      .dat() %>% 
        select(-content) %>%
        group_by(target, sample) %>%
        summarise(mean_cq = mean(cq),
                  sd_cq = sd(cq) %>% round(2),
                  .groups = 'drop')
    })
    
    ## Check SD of technical replicates and save message
    high_sd_samples <- reactive(filter(dat_avg(), sd_cq > .thresh))
    
    replicates_message <- reactive({
      if(nrow(high_sd_samples()) > 0) {
        'Attention! High variation between technical replicates detected.'
      } else {
        'Variation among technical replicates is acceptable, continue.'
      }
    })
    
    # Outputs
    output$replicates_message <- renderText({replicates_message()})
    output$replicates_table <- renderTable({high_sd_samples()})
    
    # Return dat_avg
    reactive(dat_avg())

  })
}
