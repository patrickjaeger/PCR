library(tidyverse)

# Load data and tidy up ---------------------------------------------------
tags <- c('animal', 'genotype', 'sample')

dat <- read_csv('Greta_150921_Mousecells_VHLKO -  Quantification Summary_CLEARNAMES.csv') %>% 
  select(Well, Target, Content, Sample, Cq) %>%
  rename_with(tolower) %>% 
  mutate(target = toupper(target)) %>% 
  separate(sample, tags, '_')


# Show blanks -------------------------------------------------------------

filter(dat, str_detect(content, 'Neg Ctrl'))


# Average Cq of wells -----------------------------------------------------

all_avg_cqs <- dat %>% 
  filter(!str_detect(content, 'Neg Ctrl')) %>% 
  select(-content) %>% 
  group_by(target, !!!syms(tags)) %>% 
  summarise(mean_cq = mean(cq), .groups = 'drop')


# Exctract housekeeper Cq values ------------------------------------------

housek <- 'ANXA5'

housek_avg_cqs <- all_avg_cqs %>% 
  filter(target == housek) %>% 
  select(!!!syms(tags), mean_cq) %>%
  rename(housek_mean_cq = mean_cq)


# Get mean control group Cq -----------------------------------------------

ctrl_group <- 'WT'

ctrl_avg_cqs <- all_avg_cqs %>% 
  filter(genotype == ctrl_group) %>% 
  group_by(target) %>% 
  summarise(ctrl_avg_cq = mean(mean_cq))

get_ctrl_avg_cq <- function(.avg_cqs, .ctrl_group, .gmean = FALSE) {
  # Calculate mean Cq values of control groups. To be
  # used with a nested tibble.
  # .avg_cqs (dataframe): all Cqs of target gene
  # .ctrl_group (string): name of contol group
  # .gmean (logic): calculate geometric or normal mean
  .avg_cqs %>% filter(genotype == .ctrl_group) %>% 
    pluck('mean_cq') %>% 
    if_else(.gmean, 
            prod(.)^(1/length(.)),
            mean(.))
}


# Calculate dCq -----------------------------------------------------------
# dCq = target Cq - housekeeper Cq

get_dct <- function(.target_avg_cqs, .housek_avg_cqs) {
  # Calculate dCq. To use with map over a nested tibble.
  # .target_avg_cqs (dataframe): ct values of target gene
  # .housek_avg_cqs (dataframe): ct values of housekeeper
  full_join(.target_avg_cqs, .housek_avg_cqs) %>% 
    mutate(dct = mean_cq - housek_mean_cq)
}

# test code for dct calculation 
{
# target_gene <- 'VEGFA'

# target_avg_cqs <- all_avg_cqs %>% 
#   filter(target == target_gene) %>% 
#   unnest(cols = data)
# 
# target_dct <- full_join(target_avg_cqs, housek_avg_cqs) %>% 
#   mutate(dct = mean_cq - housek_mean_cq)

# get_dct(target_avg_cqs, housek_avg_cqs)
}

ctrl_group <- 'WT'
# all_dcts <- 
  all_avg_cqs %>% 
  group_nest(target) %>% 
  mutate(dct = map(data, ~get_dct(., housek_avg_cqs)),
         ctrl_avg_cq = map(dct, ~get_ctrl_avg_cq(., ctrl_group))) #%>% 
  
  # full_join(., ctrl_avg_cqs)
# TODO Fix get_ctrl_avg_cq

# Calculate ddCq ----------------------------------------------------------

get_ddct <- function(.target_dct_dat, .ctrl_cq_mean) {
  # Calculate ddCq. To be used with map over a nested tibble.
  # .target_dct_dat (dataframe): 
  # .ctrl_cq_mean (dataframe):
}

  


  