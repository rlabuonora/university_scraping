# Merge data from both sources
library(dplyr)
# topuniversities
programs_final <- readRDS('./data/programs_clean.rds')

# THE
# Scores has no names
scores <- readRDS('./data/scores.rds')
ranks <- readRDS('./data/ranks.rds') %>% 
  select(name, rank)

the_data <- scores %>% 
  left_join(ranks) %>% 
  rename(university=name)

merge <- program_names %>% 
  left_join(the_data)

not_found <- filter(merge, is.na(rank))

saveRDS(merge, './data_output/universities.rds')


