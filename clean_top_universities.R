library(dplyr)
library(readr)

# Clean topuniversities.com data
programs <- readRDS("./data/programs_requirements_fee_geolocated.rds")


programs_final <- programs %>% 
  mutate(fee=parse_number(str_match(tuition_fee, "(\\d{1,3},?\\d{3})\\s(\\w{3})")[, 2])) %>% 
  mutate(currency=str_match(tuition_fee, "(\\d{1,3},?\\d{3})\\s(\\w{3})")[, 3]) %>% 
  select(university=university_name_text,
         university_link, requirements,
         study_level,
         location, longitude, latitude,
         program_title, program_link=programe_link,
         subject, study_mode, course_intensity, 
         duration, fee, currency)

saveRDS(programs_final, "./data/programs_final.rds")


 
