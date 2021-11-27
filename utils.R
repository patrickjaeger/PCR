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


calc_dcq <- function(.df, .housek) {
  # Calculate dct values in a long format tibble
  # .df (dataframe): dataframe that already contains averaged Cq values
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