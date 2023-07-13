library(RSelenium)
library(netstat)
library(tidyverse)
library(glue)


# open browser


# wait for page to load


# Understand the html structure of the page
# using chrometools (see captures)

# this should be inside a function

# Each row is a programe
# Each card is a row
# this function takes the card as input
# and returns a vector with data
extract_row <- function(card) {
  
  # Programe title
  programe_element <- card$findChildElement(using="xpath", 
                                            ".//div[contains(@class, 'programeTitle')]//a")
  
  programe_ref <- programe_element$getElementAttribute("href") %>% 
    unlist()
  
  programe_title <- programe_element$getElementText() %>% 
    unlist()
  
  programe_data <- c(programe_link=programe_ref,
    program_title=programe_title)
  
  # University name and website link
  university_name_element <- card$findChildElement(using="xpath", 
                                                   ".//div[contains(@class, 'uni-det')]//p//a")
  university_name_text <- university_name_element$getElementText() %>% 
    unlist()
  
  university_link <- university_name_element$getElementAttribute("href") %>% 
    unlist()
  
  university_data <- c(university_name_text=university_name_text,
                       university_link=university_link)
  # University Location
  location_element <- card$findChildElement(using="css", '.location')
  location <- location_element$getElementText() %>% unlist()
  names(location) <- "location"
  
  # Misc programe info
  uni_info_element <- card$findChildElement(using="css", ".uni-info")
  
  
  uni_info_vector <- uni_info_element$getElementText() %>% 
    unlist() %>% 
    str_split("\n") %>% 
    unlist()
  
  pattern <- "\\b([A-Z]{3})"
  
  # Extract Currency from Fee(AUD), Fee(USD)
  x <- str_extract(uni_info_vector, pattern)
  curr <- c(currency=x[!is.na(x)])
  
  
  uni_info_vector <- str_remove(uni_info_vector, pattern <- "\\(\\b([A-Z]{3})\\)")
  nm <- seq(1, length(uni_info_vector), by=2)
  values <- seq(2, length(uni_info_vector), by=2)
  
  uni_info_data <- uni_info_vector[values]
  names(uni_info_data) <- uni_info_vector[nm]
  
  return(c(programe_data, university_data, location, curr, uni_info_data))
}



scrap_page <- function(i) {
  url <- "https://www.topuniversities.com/programs/iran/biological-sciences?country=[OM,PS,AE,BH,IR,IQ,JO,SA,KW,QA,SY,YE,CY,TR,IL,LB,GB,EG]&subjects=[462,468,4049,4055,494,496,554,500,502,508]"
  message(url)
  
  msg <- rs_driver_object$server$error()
  message(paste0("Error: ", msg))
  
  remDr$navigate(url)
  
  cards <- remDr$findElements(using="css", ".card")
  df <- purrr::map_df(cards, ~extract_row(.))
  saveRDS(df, glue('./temp/page_{i}.rds'))
}


scrap_programs <- function(start_page) {
  
  # First page
  url <- glue("https://www.topuniversities.com/programs/africa?region=[4010,4008]&pagerlimit=[25]")
  remDr$navigate(url)
  cookie_button <- remDr$findElement(using="class name", "eu-cookie-compliance-default-button")
  cookie_button$clickElement()
  # Extract number of total programs in region
  programs_element <- remDr$findElement(using="class name", "number_uni_pgm")
  total_programs <- as.numeric(str_extract(programs_element$getElementText(), "\\d+"))
  last_page <- (total_programs %/% 25) + 1
  
  msg <- rs_driver_object$server$error()
  message(msg)
  # scrap pages
  df <- as.list(start_page:last_page) %>% 
    purrr::map_df(~scrap_page(.))
  
  # on.exit
  remDr$close()
  
}
# start the server
rs_driver_object <- rsDriver(browser='chrome',
                             port=free_port(),
                             chromever = "114.0.5735.16")

remDr <- rs_driver_object$client
scrap_programs(2226)
files <- list.files('./temp', pattern = ".rds", full.names = TRUE)
# Esto puede servir para retomar
# current_page <- files %>% 
#   str_extract("\\d{1,4}") %>% 
#   as.numeric() %>% 
#   max()


# read files
df <- files %>%
  as.list %>%
  purrr::map_df(~readRDS(.))
 
