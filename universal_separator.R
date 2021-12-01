# How to split camelCase
# https://stackoverflow.com/questions/8406974/splitting-camelcase-in-r

s1 <- c("322_WT_1", "322_WT_2", "323_WT_1", "323_WT_2", "324_KO_1")

strsplit(gsub("([[:upper:]])", " \\1", s1), " ")


s1 <- '336_KO_1'
str_detect(s1, 'KO')
sum(str_detect(s1, 'WT')) > 0
