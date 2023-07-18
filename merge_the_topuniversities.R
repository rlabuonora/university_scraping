# Merge data from both sources
library(dplyr)
# topuniversities
locations <- readRDS('./data/geolocated_locations.rds')

# THE
# Scores has no names
scores <- readRDS('./data/scores.rds') %>% 
  rename(university=name)
ranks <- readRDS('./data/ranks.rds') %>% 
  select(university=name, rank)

locations_the <- locations %>% 
  left_join(ranks, by="university") %>% 
  left_join(scores, by="university")

saveRDS(locations_the, './data_output/locations_the.rds')


