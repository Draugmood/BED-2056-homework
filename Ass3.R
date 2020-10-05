rm(list=ls())

# libraries
library(tidyverse)
library(rvest)

# Saving urls into variables
r_courses <- "https://www.datacamp.com/courses/tech:r"
py_courses <- "https://www.datacamp.com/courses/tech:python"

# get html from url, pull out nodes for each course using xpath,
# save as a df, with an added column for the language (Python)
py_df <- py_courses %>% 
  read_html() %>%
  html_nodes(xpath='//h4[@class="course-block__title"]') %>% 
  html_text() %>% 
  data.frame("Course" = ., "Language" = "Python")

# same as above but with R courses and R as the "language"
r_df <- r_courses %>% 
  read_html() %>% 
  html_nodes(xpath='//h4[@class="course-block__title"]') %>% 
  html_text() %>% 
  data.frame("Course" = ., "Language" = "R")

# bind by rows into one long df
all_df <- rbind(py_df, r_df)
