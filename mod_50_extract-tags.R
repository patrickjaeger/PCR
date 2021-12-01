# People don't know how to name things. Aaaargh!

# Modules -----------------------------------------------------------------

extractTagsUI <- function(id) {
  tagList(
    h3('Extract Tags'),
    textInput('tags', 
              'Enter names of tags to extract:', 
              placeholder = 'donor genotype sample'),
    orderInput('arrange_tags', 
               'Arrange by:', 
               items = c('B', 'A', 'N', 'A', 'N', 'A')),
    tableOutput('head_res2_long'),
    downloadButton('d_res2_long', 'Download Results (long format)'),
    downloadButton('d_res2_wide', 'Download Results (wide format)')
  )
}

extractTagsServer <- function(id, .dat) {
  moduleServer(id, function(input, output, session) {
    dat_final <- split_label(.dat, 'sample')
  })
}


# Utils -------------------------------------------------------------------

split_label <- function(.df, .col) {
  # Split character column containing n substrings separated by '_'
  # into n column named tag_1...tag_n
  # .df (dataframe): dataframe
  # .col (chr): name of column in .df to be split, supplied in 'quotes'
  
  tags <- 
    .df %>% 
    pluck(.col) %>% 
    str_split('_') %>% 
    tibble(tag = .) %>% 
    unnest_wider(tag, '_')
  
  bind_cols(.df, tags) %>% select(-!!.col)
}

