# 

calculationUI <- function(id) {
  tagList(
    
    h3('Calculate Stuff'),
    selectInput(NS(id, 'housek'), 
                'Select housekeeper:',
                choices = c()),
    textInput(NS(id, 'ctrl_grp'),
                'Enter name of control group (case sensitive):'),
    actionButton(NS(id, 'calculate'), 'Calculate Relative Gene Expression'),
    downloadButton(NS(id, 'd_res_long'), 'Download Results (long format)'),
    downloadButton(NS(id, 'd_res_wide'), 'Download Results (wide format)'),
    tableOutput(NS(id, 'head_res_long'))
    
  )
}


calculationServer <- function(id, .dat) {
  moduleServer(id, function(input, output, session) {
    
    # Update housekeeper selection
    observeEvent(.dat(), {
      updateSelectInput(inputId = 'housek', choices = unique(.dat()$target))
    })
    
    # Get tags for control group
    # 1. get all tag
    all_tags <- reactive()
    
    
    housek <- reactive(input$housek)
    ctrl_grp <- reactive({
      # TODO Insert shinyFeedback if ctrl_grp cannot be found
      input$ctrl_grp
      })
    
    
    
    
  })
}



# Utils -------------------------------------------------------------------

calc_dcq <- function(.df, .housek) {
  # Calculate dct values in a long format tibble
  # .df (dataframe): contains averaged Cq values
  # .housek (chr): name of housekeeper gene
  
  housek_cqs <- 
    .df %>% 
    filter(target == .housek) %>% 
    select(sample, mean_housek_cq = mean_cq)
  
  .df %>% 
    group_by(target) %>% 
    full_join(., housek_cqs) %>% 
    ungroup() %>% 
    mutate(dcq = mean_cq - mean_housek_cq) %>% 
    select(-mean_housek_cq) %>% 
    filter(target != .housek)
}


calc_ddcq_and_fold_change <- function(.df, .ctrl_grp, .gmean = FALSE) {
  # Calculate the mean of the control group first and then the ddcq
  # ∆∆Ct = ∆Ct (Sample) – ∆Ct (Control average)
  # .df (dataframe): contains dCq values
  # .ctrl_grp (chr): name of control group
  # .gmean (lgl): use arithmetic or geometric mean to calculate ctrl_grp_avg?
  ctr_grp_avgs <- 
    .df %>% 
    filter(str_detect(sample, .ctrl_grp)) %>% 
    group_by(target) %>% 
    summarise(mean_ctrl_dcq = if_else(T, 
                                      prod(dcq)^(1/length(dcq)),
                                      mean(dcq)),
              sd_ctrl_dcq = sd(dcq))
  
  .df %>% 
    group_nest(target) %>% 
    full_join(., ctr_grp_avgs) %>% 
    select(-sd_ctrl_dcq) %>% 
    unnest(data) %>% 
    mutate(ddcq = dcq - mean_ctrl_dcq) %>% 
    mutate(fold_change = 2^(-1*ddcq))
}
