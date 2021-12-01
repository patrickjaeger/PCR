t1 <- read_csv('rel_gene_expression.csv')

t1 %>% 
  select(target, sample, fold_change) %>% 
  pivot_wider(names_from = target, values_from = fold_change)


us_rent_income
us_rent_income %>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe))
