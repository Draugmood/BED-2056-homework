rm(list=ls())

# libraries
library(tidyverse)
library(lubridate)
library(rvest)
library(PxWebApiData)

county <- ApiData("http://data.ssb.no/api/v0/dataset/95274.json?lang=no",
                  getDataByGET = TRUE)

whole_country <- ApiData("http://data.ssb.no/api/v0/dataset/95276.json?lang=no",
                         getDataByGET = TRUE)

dframe <- bind_rows(county[[1]], whole_country[[1]])


# new names, could have used dplyr::rename()
names(dframe) <- c("region", "date", "variable", "value")

# Split date
dframe <- dframe %>% separate(date, 
                              into = c("year", "month"), 
                              sep = "M")

# Make a new proper date variable
dframe <- dframe %>%  mutate(date = ymd(paste(year, month, 1)))

# dplyr::recode()
dframe <- dframe %>%
  mutate(variable = dplyr::recode(variable,
                                   "Utleigde rom" = "rentedrooms",
                                   "Pris per rom (kr)" = "roomprice",
                                   "Kapasitetsutnytting av rom (prosent)" = "roomcap",
                                   "Kapasitetsutnytting av senger (prosent)" = "bedcap",
                                   "Losjiomsetning (1 000 kr)" = "revenue",
                                   "Losjiomsetning per tilgjengeleg rom (kr)" = "revperroom",
                                   "Losjiomsetning, hittil i år (1 000 kr)" = "revsofar",
                                   "Losjiomsetning per tilgjengeleg rom, hittil i år (kr)" = "revroomsofar",
                                   "Pris per rom hittil i år (kr)" = "roompricesofar",
                                   "Kapasitetsutnytting av rom hittil i år (prosent)" = "roomcapsofar",
                                   "Kapasitetsutnytting av senger, hittil i år (prosent)" = "bedcapsofar"))


# List all counties to more easily find which ones need recoding
dframe %>% 
  select(region) %>% 
  unique()

# Recode relevant county formats
dframe <- dframe %>%
  mutate(region = dplyr::recode(region,
                                "Heile landet" = "Countrywide",
                                "Vestfold og Telemark" = "VestfoldOgTelemark",
                                "Møre og Romsdal" = "MoreOgRomsdal",
                                "Trøndelag - Trööndelage" = "Trondelag",
                                "Troms og Finnmark - Romsa ja Finnmárku" = "TromsOgFinnmark"))

dframe <- filter(dframe, variable == "roomcap")

dframe %>%
  ggplot(aes(x = month, y = value, group = region)) +
  geom_line(aes(color = region)) +
  ggtitle("Monthly regional room capacity utilization") +
  labs(x="Month", y="Capacity utilization (%)")
