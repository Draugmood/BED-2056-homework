rm(list=ls())

# libraries
library(tidyverse)
library(rvest)

# url to scrape from
timeplan <- "http://timeplan.uit.no/emne_timeplan.php?sem=20h&module%5B%5D=BED-2056-1&View=list"

# get html from url, pull out nodes for date of each lecture using xpath
# remove "Mandag" string to only get date
date <- timeplan %>% 
  read_html() %>%
  html_nodes(xpath='//tr[@class="table-primary"]/td[1]') %>% 
  html_text() %>% 
  str_remove("Mandag")


# same as above, but no "Mandag" to remove, and time in stead of date
time <- timeplan %>% 
  read_html() %>%
  html_nodes(xpath='//tr[@class="table-primary"]/td[2]') %>% 
  html_text()


# merge into one df
df <- data.frame("Dato" = date, "Tidspunkt" = time)
