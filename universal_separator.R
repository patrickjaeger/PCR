# How to split camelCase
# https://stackoverflow.com/questions/8406974/splitting-camelcase-in-r

s1 <- c("322_WT_1", "322_WT_2", "323_WT_1", "323_WT_2", "324_KO_1")
s1 <- c("322WT_1", "322WT_2", "323WT_1", "323WT_2", "324KO_1")

strsplit(gsub("([[:upper:]])", " \\1", s1), " ")

str_extract(s1, '')

s1 <- '336_KO_1'
str_detect(s1, 'KO')
sum(str_detect(s1, 'WT')) > 0

t1 <- tibble(sample = s1)

t1 <- read_csv('data/Greta_150921_Mousecells_VHLKO -  Quantification Summary.csv')
t1 <- t1 %>% select(-...1, -SQ) %>% na.omit()

t1 <- t1$Sample[1:20]
t1 <- tibble(sample = t1)
t1 %>% 
  mutate(donor = str_extract(sample, '[:digit:]{3}'),
         sample = str_remove(sample, donor)) %>% 
  mutate(genotype = str_extract(sample, '[:upper:]{2}'),
         sample = str_remove(sample, genotype)) %>% 
  mutate(sample_number = str_extract(sample, '[:digit:]{1}'),
         sample = str_remove(sample, sample_number)) %>% 
  mutate(sample_number = replace_na(sample_number, 1))

