source('./scrap_program_page.R')

# Test it with a single url
url <- "https://www.topuniversities.com/universities/university-law/postgrad/llm-mediation-alternative-dispute-resolution-part-time-online"
scrap_program_page(url)

# test it with the first 10 rows of program
programs <- readRDS('./data/programs.rds')
t <- programs %>% 
  head(10) %>% 
  pull(programe_link) %>% 
  as.list() %>% 
  purrr::map(~ scrap_program_page(.x))
  