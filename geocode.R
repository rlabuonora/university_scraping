library(tibble)
library(tidygeocoder)
library(dplyr)
library(stringr)

# Geocode program locations
programs <- readRDS('./data/programs_clean.rds') %>% 
  tibble() %>% 
  mutate(location=str_remove(location, "\\+1"))

# locations
locations <- programs %>% 
  count(location, university) %>% 
  select(-n) %>% 
  # If location is not available use University name to geocode
  mutate(location=if_else(location=="", NA, location)) %>% 
  mutate(location=coalesce(location, university))

# API call
geolocated_locations <- locations %>% 
  geocode(location, method = 'google', lat = latitude , long = longitude)

saveRDS(geolocated_locations, './data/geolocated_locations.rds')
locations <- readRDS('./data/geolocated_locations.rds')

programs_clean <- left_join(programs_clean, locations)
saveRDS(programs_clean, "./data_output/programs_geolocated.rds")

