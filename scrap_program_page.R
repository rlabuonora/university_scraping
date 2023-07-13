library(rvest)

# scrap country opts
url <- "https://www.topuniversities.com/programs/iran/biological-sciences?country=[OM,PS,AE,BH,IR,IQ,JO,SA,KW,QA,SY,YE,CY,TR,IL,LB,GB,EG]&subjects=[462,468,4049,4055,494,496,554,500,502,508]"
# scrap program page using rvest
scrap_program_page <- function(url) {
  html <- read_html(url)
  requirements <-  html %>% 
    html_element("#admissionTab") %>% 
    html_elements(".univ-subsection-full-width-value") %>% 
    html_text()
  
  tuition_fee <- html %>% 
    html_element("#p2-tuition-fee-and-scholarships") %>% 
    html_elements(".univ-subsection-value") %>% 
    html_text() %>% 
    str_subset("Tuition Fee/year") %>% 
    head(1)
  
  return(list(requirements=requirements, 
              tuition_fee=tuition_fee))
}


