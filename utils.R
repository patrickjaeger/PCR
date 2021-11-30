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


# calc_dcq <- function(.df, .housek) {
#   # Calculate dct values in a long format tibble
#   # .df (dataframe): contains averaged Cq values
#   # .housek (chr): name of housekeeper gene
#   
#   housek_cqs <- 
#     .df %>% 
#     filter(target == .housek) %>% 
#     select(sample, mean_housek_cq = mean_cq)
#   
#   .df %>% 
#     group_by(target) %>% 
#     full_join(., housek_cqs) %>% 
#     ungroup() %>% 
#     mutate(dcq = mean_cq - mean_housek_cq) %>% 
#     select(-mean_housek_cq) %>% 
#     filter(target != .housek)
# }
# 
# 
# calc_ddcq <- function(.df, .ctrl_grp, .gmean = FALSE) {
#   # Calculate the mean of the control group first and then the ddcq
#   # ∆∆Ct = ∆Ct (Sample) – ∆Ct (Control average)
#   # .df (dataframe): contains dCq values
#   # .ctrl_grp (chr): name of control group
#   # .gmean (lgl): use arithmetic or geometric mean to calculate ctrl_grp_avg?
#   ctr_grp_avgs <- 
#     .df %>% 
#     filter(str_detect(sample, .ctrl_grp)) %>% 
#     group_by(target) %>% 
#     summarise(mean_ctrl_dcq = if_else(T, 
#                                       prod(dcq)^(1/length(dcq)),
#                                       mean(dcq)),
#               sd_ctrl_dcq = sd(dcq))
#   
#   .df %>% 
#     group_nest(target) %>% 
#     full_join(., ctr_grp_avgs) %>% 
#     select(-sd_ctrl_dcq) %>% 
#     unnest(data) %>% 
#     mutate(ddcq = dcq - mean_ctrl_dcq)
# }