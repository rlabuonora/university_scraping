library(dplyr)
library(readr)
library(stringr)
library(tidyr)

library(tm)



# Clean topuniversities.com data
programs <- readRDS("./data/programs_requirements_fee.rds")

programs_clean <- programs %>% 
  # Fix program titles
  mutate(fee=parse_number(str_match(tuition_fee, "(\\d{1,3},?\\d{3})\\s(\\w{3})")[, 2])) %>% 
  mutate(currency=str_match(tuition_fee, "(\\d{1,3},?\\d{3})\\s(\\w{3})")[, 3]) %>% 
  select(university=university_name_text,
         university_link, requirements,
         study_level,
         location, 
         program_title, program_link=programe_link,
         subject, study_mode, course_intensity, 
         duration, fee, currency) %>% 
  # Fix university names that do not match with THE website
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
  # Fix duration variable
  mutate(duration_units=str_match(duration, "(\\d+) (Months)")[,3]) %>% 
  # Convert currencies
  mutate(fee_gbp=case_when(currency=="GBP"~fee,
                           currency=="USD"~fee/1.3,
                           currency=="EUR"~fee/1.16,
                           # singapore dollar
                           currency=="SGD"~fee/1.72,
                           TRUE~NA_real_))

# Fix requirements data
programs_clean <- programs_clean %>% 
  mutate(requirements=if_else(
    requirements == "Cambridge CAE Advanced 176+ PTE Academic 62+ International Baccalaureate 30+ GPA 3+ IELTS 5+ TOEFL 72+ IELTS 5+ IELTS 5+ TOEFL 72+ TOEFL 72+",
    "Cambridge CAE Advanced\n176+\n\nPTE Academic\n62+\n\nInternational Baccalaureate\n30+\n\nGPA\n3+\n\nIELTS\n5+\n\nTOEFL\n72+\n",
    requirements
  )) %>% 
  distinct(university, program_title, .keep_all = TRUE)


programs_clean <- programs_clean %>% 
  mutate(requirements=str_trim(requirements)) %>% 
  mutate(requirements=str_replace_all(requirements, "\n\\+\n", "\n0+\n")) %>%
  mutate(requirements=str_replace(requirements, "\n\\+$", "\n0+")) %>% 
  mutate(requirements=str_replace(requirements, "\n \n", "\n\n")) %>% 
  mutate(requirements=if_else(
    program_title=="Computer Science (Cyber Security) with a Year in Industry - BSc (Hons)", 
    "Cambridge CAE Advanced\n176+\n\nPTE Academic\n62+\n\nInternational Baccalaureate\n30+\n\nGPA\n3+\n\nIELTS\n5+\n\nTOEFL\n72+\n",
    requirements))

programs_clean <- programs_clean %>% 
  mutate(y=str_split(str_trim(requirements), "\n\n")) %>% 
  filter(requirements!="") %>% 
  unnest(y) %>% 
  separate(y, into = c("variable", "value"), sep = "\n") %>% 
  mutate(variable=str_trim(variable)) %>% 
  mutate(value=as.numeric(str_remove(str_trim(value), "\\+")))

programs_clean <- programs_clean %>% 
  pivot_wider(names_from = variable, values_from = value, values_fill = NA) %>%
  janitor::clean_names() %>% 
  mutate(ielts = if_else(is.na(ielts), 0, ielts)) %>% 
  filter(ielts<=9) %>% # ielts must be 0-9
  mutate(toefl = if_else(is.na(toefl), 0, toefl)) %>% 
  filter(toefl<=120)


# Use keywords to keep relevant programs
# in some subjects

# Subjects that include relevant programs
subjs <- c(
  "Anatomy and Physiology", # remove
  "Anthropology",
  "Archaeology",
  "Zoology",
  "Dentistry",
  "Veterinary Science",
  "Theology, Divinity and Religious Studies",
  "Sports-related Courses",
  "Linguistics",
  "Pharmacology",
  "History",
  "Food Science",
  "English Language and Literature",
  "Geology",
  "Public Policy",
  "Nursing",
  "Toxicology",
  "Philosophy",
  "Ethnicity, Gender and Diversity",
  "Biological Sciences",
  "Education and Training",
  "Chemistry",
  "Genetics",
  "Geography",
  "Health/Healthcare",
  "Law and Legal Studies",
  "Mathematics",
  "Medicine",      
  "Earth and Marine Sciences",
  "Environmental Sciences",
  "Medicine Related Studies",
  "Psychology",
  "Pharmacy and Pharmacology",
  "Physics and Astronomy",
  "Sociology")

# Keywords to keep
keywords_keep <- c("data", "information", "engineering", "digital",
                   "computational", "computer", "bioinformatics",
                   "intelligence", "cyber", "datamining", "operational",
                   "biotechnology", "computing") %>% 
  paste0(collapse = "|")


# Generate keywords
# Create a corpus
docs <- Corpus(VectorSource(programs_clean$program_title))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

programs_clean$keywords <- docs$content

# Keep programs with relevant keywords
programs_clean <- mutate(programs_clean,
                              key=str_detect(keywords,
                                             keywords_keep),
                              subj = subject %in% subjs ) %>% 
  mutate(keep=(key | ! subj)) %>% 
  filter(keep) %>% 
  select(-key, -subj, -keywords, -keep, -university_link, -program_link) 

saveRDS(programs_clean, "./data/programs_clean.rds")


