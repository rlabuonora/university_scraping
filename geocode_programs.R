library(tibble)
library(tidygeocoder)
library(dplyr)
library(stringr)

# Geocode program locations
programs <- readRDS('./data/programs_requirements_fee.rds') %>% 
  tibble() %>% 
  mutate(location=str_remove(location, "\\+1"))

# locations
locations <- programs %>% 
  count(location) %>% 
  select(-n)

# API call
geolocated_locations <-locations %>% 
  geocode(location, method = 'google', lat = latitude , long = longitude)


# Merge with program data
programs <- programs %>% 
  left_join(geolocated_locations, by="location")

saveRDS(geolocated_locations, './data/geolocated_locations.rds')

saveRDS(programs, "./data_output/programs_requirements_fee_geolocated.rds")
