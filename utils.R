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
