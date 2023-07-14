library(dplyr)
library(purrr)
library(rvest)
library(stringr)

# 1. Helper function 
# Scrap a program page to get tuition fee and admission requirements
scrap_program_page <- function(url) {
  
  message(url)
  
  html <- read_html(url)
  
  requirements <-  html %>% 
    html_element("#admissionTab") %>% 
    html_elements(".univ-subsection-full-width-value") %>% 
    html_text() %>% 
    paste0(collapse = "")
  
  if(length(requirements)==0) {
    requirements <- ""
  }
  
  tuition_fee <- html %>% 
    html_element("#p2-tuition-fee-and-scholarships") %>% 
    html_elements(".univ-subsection-value") %>% 
    html_text() %>% 
    str_subset("Tuition Fee/year") %>% 
    head(1)
  
  if(length(tuition_fee)==0) {
    tuition_fee <- ""
  }
  
  return(tibble(url=url, requirements=requirements, 
                tuition_fee=tuition_fee))
}

# 2. load programs dataset
programs <- readRDS('./data/programs.rds') %>% 
  janitor::clean_names() %>% 
  select(programe_link, program_title,
         university_name_text, university_link,
         location, study_level, 
         study_mode, course_intensity, subject,
         duration)

  
# 3. Scrap the programs
programs_requirements_fee <- bind_cols(map_df(programs$programe_link, scrap_program_page), programs)

# 4. Save rds file
saveRDS(programs_requirements_fee, "./data/programs_requirements_fee.rds")
