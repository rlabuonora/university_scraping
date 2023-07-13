library(tibble)
library(tidygeocoder)
library(dplyr)

programs <- readRDS('./data/programs.rds') %>% 
  tibble()  

# locations
locations <- programs %>% 
  count(location)

geolocated <-locations %>% 
  geocode(location, method = 'osm', lat = latitude , long = longitude)

saveRDS(geolocated, './data/geolocated.rds')
