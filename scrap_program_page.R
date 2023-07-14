library(rvest)
library(stringr)

# scrap country opts
# scrap program page using rvest
url <- "https://www.topuniversities.com/universities/university-law/postgrad/llm-mediation-alternative-dispute-resolution-part-time-online"

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


