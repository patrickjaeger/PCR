# 

calculationUI <- function(id) {
  tagList(
    shinyFeedback::useShinyFeedback(),
    h3('Calculate Stuff'),
    selectInput(NS(id, 'housek'), 
                'Select housekeeper:',
                choices = c()),
    strong('Enter name of control group (case sensitive):'),
    textInput(NS(id, 'ctrl_grp'), NULL),
    # actionButton(NS(id, 'check_ctrl_grp'), 'Validate control group'), 
    actionButton(NS(id, 'calculate'), 
                 'Calculate Relative Gene Expression'), br(),
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
    
    # Check and save control group
    ctrl_grp <- reactive({
      shinyFeedback::feedbackWarning('ctrl_grp', 
                                     str_length(input$ctrl_grp) == 0, '-_-')
      req(!str_length(input$ctrl_grp) == 0, cancelOutput = TRUE)
      
      pass <- sum(str_detect(.dat()$sample, input$ctrl_grp)) > 0
      
      if (pass) {
        shinyFeedback::hideFeedback('ctrl_grp')
        shinyFeedback::showFeedbackSuccess('ctrl_grp', "Entry matched")
        input$ctrl_grp
      } else {
        shinyFeedback::hideFeedback('ctrl_grp')
        shinyFeedback::showFeedbackWarning('ctrl_grp', "Entry not found")
      }
    })
    
    # Calculate gene expression
    rel_gene_expr <- eventReactive(input$calculate, {
      req(ctrl_grp())

      .dat() %>% 
        calc_dcq(input$housek) %>% 
        calc_ddcq_and_fold_change(ctrl_grp())
    })
    
    # Render head of 
    output$head_res_long <- renderTable({
      head(rel_gene_expr())
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
