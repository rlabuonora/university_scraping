# Merge data from both sources
library(dplyr)
library(stringr)

programs_geolocated <- readRDS("./data_output/programs_geolocated.rds")
# topuniversities
locations <- programs_geolocated %>% 
  group_by(university) %>% 
  summarize(programs=n())

# THE
# Scores has no names
scores <- readRDS('./data/scores.rds') %>% 
  rename(university=name) %>% 
  rename(industry_income=industry_income.) %>% 
  mutate(across(c(teaching, research, overall_score, 
                  citations,
                  industry_income, international_outlook),
                function(x) if_else(x=="n/a", NA, as.numeric(x))))

ranks <- readRDS('./data/ranks.rds') %>% 
  select(university=name, rank)

universities <- locations %>% 
  left_join(ranks, by="university") %>% 
  left_join(scores, by="university")

saveRDS(locations_the, './data_output/universities.rds')


