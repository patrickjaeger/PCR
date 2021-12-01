# Calculate stuff


# Modules -----------------------------------------------------------------

calculationUI <- function(id) {
  tagList(
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    
    h3('Calculate Stuff'),
    selectInput(NS(id, 'housek'), 
                'Select housekeeper:',
                choices = c()),
    strong('Enter name of control group (case sensitive):'),
    textInput(NS(id, 'ctrl_grp'), NULL),
    actionButton(NS(id, 'calculate'), 
                 'Calculate Relative Gene Expression', 
                 width = '300px'), br(),
    disabled(
      downloadButton(NS(id, 'd_res_long'), 
                     'Download Results (long format)', 
                     style = 'width:300px'), br(),
      downloadButton(NS(id, 'd_res_wide'), 
                     'Download Results (wide format*)', 
                     style = 'width:300px')
    ), br(),
    p('*Contains only fold change.'),
    tableOutput(NS(id, 'head_res_long')), br()
    
  )
}


calculationServer <- function(id, .dat) {
  moduleServer(id, function(input, output, session) {
    
    # Update housekeeper selection
    observeEvent(.dat(), {
      updateSelectInput(inputId = 'housek', 
                        choices = unique(.dat()$target))
    })
    
    # Check whether ctrl_grp is in sample
    ctrl_grp <- reactive(input$ctrl_grp)
    pass <- reactive(sum(str_detect(.dat()$sample, input$ctrl_grp)) > 0)
    
    # Give feedback on ctrl_grp and toggle download
    observeEvent(input$calculate, {
      shinyFeedback::feedbackWarning('ctrl_grp',
                                     str_length(input$ctrl_grp) == 0, '-_-')
      req(!str_length(input$ctrl_grp) == 0, cancelOutput = TRUE)
      
      if (pass()) {
        shinyFeedback::hideFeedback('ctrl_grp')
        shinyFeedback::showFeedbackSuccess('ctrl_grp', 
                                           "Entry matched")
        shinyjs::enable('d_res_long')
        shinyjs::enable('d_res_wide')
      } else {
        shinyFeedback::hideFeedback('ctrl_grp')
        shinyFeedback::showFeedbackWarning('ctrl_grp', 
                                           "Entry not found")
        shinyjs::disable('d_res_long')
        shinyjs::disable('d_res_wide')
      }
      
    })
    
    # Calculate gene expression
    rel_gene_expr <- eventReactive(input$calculate, {
      req(pass(), ctrl_grp())
      
      .dat() %>% 
        calc_dcq(input$housek) %>% 
        calc_ddcq_and_fold_change(ctrl_grp())
    })
    
    # Render head of results
    output$head_res_long <- renderTable({
      head(rel_gene_expr())
    })
    
    # Wide format results
    rel_gene_expr_wide <- reactive({
      req(rel_gene_expr())
      rel_gene_expr() %>% 
        select(target, sample, fold_change) %>% 
        pivot_wider(names_from = target, 
                    values_from = fold_change)
    })
    
    # Download long results
    output$d_res_long <- downloadHandler(
      filename = function() {
        paste0('rel_gene_expression', ".csv")
      },
      content = function(file) {
        write_csv(rel_gene_expr(), file)
      }
    )
    
    # Download wide results
    output$d_res_wide <- downloadHandler(
      filename = function() {
        paste0('rel_gene_expression_wide', ".csv")
      },
      content = function(file) {
        write_csv(rel_gene_expr_wide(), file)
      }
    )
    
    return(rel_gene_expr)
    
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
