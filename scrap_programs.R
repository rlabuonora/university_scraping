library(dplyr)
library(purrr)
source('./scrap_program_page.R')

# Test it with a single url
# test it with the first 10 rows of program
programs <- readRDS('./data/programs.rds') %>% 
  janitor::clean_names() %>% 
  select(programe_link, program_title,
         university_name_text, university_link,
         location, study_level, 
         study_mode, course_intensity, subject,
         duration)

  
programs_requirements_fee <- bind_cols(map_df(programs$programe_link, scrap_program_page), programs)
saveRDS(programs_requirements_fee, "./data/programs_requirements_fee.rds")
