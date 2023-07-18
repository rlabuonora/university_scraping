# Merge data from both sources
library(dplyr)
# topuniversities
programs_final <- readRDS('./data/programs_final.rds') %>% 
  mutate(university=str_remove(university, "^The "))


programs_names <- count(programs_final, university) 
# THE
# Scores has no names
scores <- readRDS('./data/scores.rds')
rank <- readRDS('./data/ranks.rds') %>% 
  rename(university=name)

program_names <- programs_final %>% 
  count(university) %>% 
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
    university=="Teesside University Business School" ~"Teesside University",
    university=="University of Jordan"~"The University of Jordan",
    university=="The London School of Economics and Political Science (LSE)" ~"London School of Economics and Political Science",
    TRUE ~ university
   # university== ~"University of Edinburgh"
  ))


merge <- program_names %>% 
  left_join(rank)

not_found <- filter(merge, is.na(rank))

saveRDS(merge, './data_output/universities.rds')


