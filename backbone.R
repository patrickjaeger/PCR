# Source ------------------------------------------------------------------

# Author: Patrick Jaeger
# Contact: patrick.jaeger@hest.ethz.ch
# R version 4.1.2 (2021-11-01)

library(tidyverse)  # Version 1.3.1
source('utils.R')

# Import data -------------------------------------------------------------
fpath <- 'data/Greta_150921_Mousecells_VHLKO -  Quantification Summary_CLEARNAMES.csv'

dat_raw <- 
  read_csv(fpath) %>% 
  select(Well, Target, Content, Sample, Cq) %>%
  rename_with(tolower)


# Check and remove blanks -------------------------------------------------

blank_tag <- 'Neg Ctrl'

blanks <- filter(dat_raw, str_detect(content, blank_tag))

blanks_sum <- sum(blanks$cq, na.rm = TRUE)

blanks_message <- 
  if_else(blanks_sum == 0,
          'Congrats! Blanks are blank.',
          'Attention! One or more blanks are not blank.')
warning(blanks_message)

corrupt_blanks <- filter(blanks, !is.na(cq))

dat <- filter(dat_raw, !str_detect(content, blank_tag))


# Clean up and average Cqs ------------------------------------------------

## Average technical replicates
dat <-
  dat %>%
  select(-content) %>% 
  mutate(target = toupper(target)) %>% 
  group_by(target, sample) %>% 
  summarise(mean_cq = mean(cq),
            sd_cq = sd(cq) %>% round(2), 
            .groups = 'drop') 

## Check SD of technical replicates
thresh_sd <- 0.3

high_sd_samples <- dat %>% filter(sd_cq > thresh_sd)

if (nrow(high_sd_samples) > 0) {
  warning('Attention! High variation between technical replicates detected.')
  high_sd_samples
}


# Calculate rel. gene exp. ------------------------------------------------

housek <- 'ANXA5'
ctrl_grp <- 'WT'

rel_gene_expr <-
  dat %>% 
  calc_dcq(housek) %>% 
  calc_ddcq(ctrl_grp) %>% 
  mutate(fold_change = 2^(-1*ddcq)) %>% print(n = nrow(.))

# Split label -------------------------------------------------------------

dat_final <- split_label(rel_gene_expr, 'sample')

