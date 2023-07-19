library(RSelenium)
library(netstat)
library(tidyverse)
library(glue)

# Scrap university rankings from THE website

# start the server
rs_driver_object <- rsDriver(browser='chrome',
                             port=free_port(),
                             chromever = "114.0.5735.16")
remDr <- rs_driver_object$client

# Navigate to base url
ranking_url <- "https://www.timeshighereducation.com/world-university-rankings/2023/world-ranking#!/length/-1/sort_by/rank/sort_order/asc/cols/stats"
remDr$navigate(ranking_url)

# accept cookies
cookie_button <- remDr$findElement(using="class name", "eu-cookie-compliance-default-button")
cookie_button$clickElement()
rows <- remDr$findElements(using="xpath", "//tbody//tr[@role='row']")

# Helper function to extract element from page
extract_td <- function(row, klass) {
  # name
  element <- row$findChildElement(using="css", klass) %>% 
    unlist()
  text <- element$getElementText() %>% 
    unlist()
  text
}

# Helper function to extract data from row element
# from the rank table
table_row_df_row_rank <- function(row) {
  v <- c(rank=extract_td(row, ".rank"),
    name=extract_td(row, ".ranking-institution-title"),
    stats_student_staff_ratio=extract_td(row, ".stats_student_staff_ratio"),
    stats_pc_intl_students=extract_td(row, ".stats_pc_intl_students"),
    stats_female_male_ratio=extract_td(row, ".stats_female_male_ratio"))
  
  v
}

# scrap ranks
ranks <- purrr::map_df(rows, ~table_row_df_row_rank(.))
saveRDS(ranks, './data/ranks.rds')

# Scrap scores
scores_url <- "https://www.timeshighereducation.com/world-university-rankings/2023/world-ranking#!/length/-1/sort_by/rank/sort_order/asc/cols/scores"
remDr$navigate(scores_url)
rows <- remDr$findElements(using="xpath", "//tbody//tr[@role='row']")


# # Helper function to extract data from row element
# from the scores table
table_row_df_row_scores <- function(row) {
  
  v <- c(name             = extract_td(row, ".ranking-institution-title"),
    overall_score         = extract_td(row, ".overall-score"),
    teaching              = extract_td(row, ".teaching-score"),
    research              = extract_td(row, ".research-score"),
    citations             = extract_td(row, ".citations-score"),
    industry_income.      = extract_td(row, ".citations-score"),
    international_outlook =extract_td(row, ".industry_income-score"))
  v
}

# Iterate on every row
scores <- purrr::map_df(rows, ~table_row_df_row_scores(.))
saveRDS(scores, './data/scores.rds')
remDr$close()
