# Merge data from both sources
library(dplyr)
library(stringr)
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
  left_join(scores, by="university") %>% 
  rename(industry_income=industry_income.) %>% 
  mutate(across(c(teaching, research, overall_score, 
                  citations,
                  industry_income, international_outlook),
                function(x) if_else(x=="n/a", NA, x))) %>% 
  mutate(across(c(overall_score, rank), 
                function(x) as.numeric(str_remove(x, "–\\d{2}\\.?\\d"))))

saveRDS(locations_the, './data_output/locations_the.rds')


