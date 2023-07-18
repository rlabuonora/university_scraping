library(dplyr)
library(readr)

# Clean topuniversities.com data
programs <- readRDS("./data/programs_requirements_fee.rds")

programs <- programs %>% 
  mutate(fee=parse_number(str_match(tuition_fee, "(\\d{1,3},?\\d{3})\\s(\\w{3})")[, 2])) %>% 
  mutate(currency=str_match(tuition_fee, "(\\d{1,3},?\\d{3})\\s(\\w{3})")[, 3]) %>% 
  select(university=university_name_text,
         university_link, requirements,
         study_level,
         location, 
         program_title, program_link=programe_link,
         subject, study_mode, course_intensity, 
         duration, fee, currency) %>% 
  mutate(university=str_remove(university, "^The ")) %>% 
  mutate(university=case_when(
    university=="Anglia Ruskin University"~"Anglia Ruskin University (ARU)",
    university=="University of Roehampton, London"~"University of Roehampton",
    university=="Atılım Üniversitesi" ~"Atılım University",
    university=="Bradford College" ~"University of Bradford",
    university=="Coventry University London" ~"Coventry University",
    university=="Essex, University of" ~"University of Essex",
    university=="Gulf University for Science and Technology" ~"Gulf College",
    university=="Imam Abdulrahman Bin Faisal University (IAU)" ~"Imam Abdulrahman Bin Faisal University",
    university== "Imam Mohammad Ibn Saud Islamic University – IMSIU"~"Imam Mohammad Ibn Saud Islamic University",
    university=="Imperial College Business School" ~"Imperial College London",
    university=="King's College London" ~"King’s College London",
    university=="Kingston University, London" ~"Kingston University",
    university=="Lancaster University Management School" ~"Lancaster University",
    university=="Leeds Trinity & All Saints" ~"Leeds Trinity University",
    university=="London School of Hygiene & Tropical Medicine" ~"London School of Hygiene and Tropical Medicine",
    university=="Newcastle University Business School" ~"Newcastle University",
    university=="Northumbria University at Newcastle" ~"Northumbria University",
    university=="Prince Sultan University" ~"Prince Sultan University (PSU)",
    university=="Queen Margaret University , Edinburgh" ~"Queen Margaret University",
    university=="Queen's University Belfast" ~"Queen’s University Belfast",
    university=="Royal Holloway University of London" ~"Royal Holloway, University of London",
    university=="Solent University Southampton" ~"Solent University, Southampton",
    university=="University of East Anglia (UEA)" ~"University of East Anglia",
    university=="Teesside University Business School" ~"Teesside University",
    university=="University of Jordan"~"The University of Jordan",
    university=="The London School of Economics and Political Science (LSE)" ~"London School of Economics and Political Science",
    TRUE ~ university
    # university== ~"University of Edinburgh"
  )) %>% 
  mutate(duration_length=as.numeric(str_match(duration, "(\\d+) (Months)")[,2])) %>% 
  mutate(duration_units=str_match(duration, "(\\d+) (Months)")[,3]) %>% 
  mutate(fee_gbp=case_when(currency=="GBP"~fee,
                           currency=="USD"~fee*1.3,
                           currency=="EUR"~fee*1.16,
                           # singapore dollar
                           currency=="SGD"~fee*1.72,
                           TRUE~NA_real_))
  
saveRDS(programs_final, "./data/programs_clean.rds")

 
