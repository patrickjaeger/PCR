library(tidyverse)

dat <- read_csv('Greta_150921_Mousecells_VHLKO -  Quantification Summary.csv') %>% 
  select(Well, Fluor, Target, Content, Sample, Cq) %>%
  # rename_with(tolower) %>% 
  separate(Sample, c('part0', 'part3'), remove = FALSE) %>% 
  mutate(part3 = if_else(is.na(part3), 1, 2)) %>% 
  mutate(part1 = parse_number(part0),
         part2 = str_extract(part0, '[A-Z]+')) %>% 
  mutate(Sample = paste(part1, part2, part3, sep = '_')) %>% 
  select(-part0, -part1, -part2, -part3)

write_csv(dat, 'Greta_150921_Mousecells_VHLKO -  Quantification Summary_CLEARNAMES.csv')
