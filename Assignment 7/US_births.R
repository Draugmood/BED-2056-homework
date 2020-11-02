
library(tidyverse)
library(lubridate)

rm(list=ls())



################ PROPORTION OF BOYS TO GIRLS ################

df_2017_raw <- read_fwf(
  file='Nat2017PublicUS.c20180516.r20180808.txt',
  # Fill, Year, Mo, Time, Day, Fill, Sex, Fill, Weight
  fwf_widths(c(8, 4, 2, 8, 1, 451, 1, 28, 4))
)

df_2017_sex <- df_2017_raw %>% 
  select(X2, X3, X7) %>% 
  rename(., Year = X2, Month = X3,Sex = X7) %>% 
  mutate(Month = as.numeric(Month), Sex = as.factor(Sex)) %>% 
  mutate(Date = dmy(paste(1, .$Month, .$Year, sep="-"))) %>% 
  group_by(Sex, Date = floor_date(Date, "month")) %>% 
  summarise(n = cumsum(n())) %>% 
  mutate("Cummulative" = cumsum(n))

rm(df_2017_raw)


df_2018_raw <- read_fwf(
  file='Nat2018PublicUS.c20190509.r20190717.txt',
  # Fill, Year, Mo, Time, Day, Fill, Sex, Fill, Weight
  fwf_widths(c(8, 4, 2, 8, 1, 451, 1, 28, 4))
)

df_2018_sex <- df_2018_raw %>% 
  select(X2, X3, X7) %>% 
  rename(., Year = X2, Month = X3,Sex = X7) %>% 
  mutate(Month = as.numeric(Month), Sex = as.factor(Sex)) %>% 
  mutate(Date = dmy(paste(1, .$Month, .$Year, sep="-"))) %>% 
  group_by(Sex, Date = floor_date(Date, "month")) %>% 
  summarise(n = cumsum(n())) %>% 
  mutate("Cummulative" = cumsum(n))

rm(df_2018_raw)


df_2019_raw <- read_fwf(
  file='Nat2019PublicUS.c20200506.r20200915.txt',
  # Fill, Year, Mo, Time, Day, Fill, Sex, Fill, Weight
  fwf_widths(c(8, 4, 2, 8, 1, 451, 1, 28, 4))
)

df_2019_sex <- df_2019_raw %>% 
  select(X2, X3, X7) %>% 
  rename(., Year = X2, Month = X3,Sex = X7) %>% 
  mutate(Month = as.numeric(Month), Sex = as.factor(Sex)) %>% 
  mutate(Date = dmy(paste(1, .$Month, .$Year, sep="-"))) %>% 
  group_by(Sex, Date = floor_date(Date, "month")) %>% 
  summarise(n = cumsum(n())) %>% 
  mutate("Cummulative" = cumsum(n))

rm(df_2019_raw)


df_total_sex <- rbind(df_2017_sex, df_2018_sex, df_2019_sex) %>% 
  mutate(Year = factor(year(Date)), Month = month(Date)) %>% 
  select(Sex, Cummulative, Month, Year)

ggplot(df_total_sex, aes(Month, Cummulative, group = Sex, color = Sex)) +
  geom_line() +
  facet_wrap(~Year) +
  xlab("Month (Jan - Dec)") +
  ylab("Cummulative Births by Sex (M/F)") +
  labs(color = "Sex") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text=element_text(size=11, face="bold"),
        axis.title=element_text(size=14, face="bold"))


# Cleanup
rm(df_2017_sex, df_2018_sex, df_2019_sex, df_total_sex)


################ AVERAGE BIRTH WEIGHT BY GENDER ################

df_2017_raw <- read_fwf(
  file='Nat2017PublicUS.c20180516.r20180808.txt',
  # Fill, Year, Mo, Time, Day, Fill, Sex, Fill, Weight
  fwf_widths(c(8, 4, 2, 8, 1, 451, 1, 28, 4))
)

df_2017_weight <- df_2017_raw %>% 
  select(X2, X3, X7, X9) %>% 
  rename(., Year = X2, Month = X3, Sex = X7, Weight = X9) %>% 
  mutate(Month = as.numeric(Month), Sex = as.factor(Sex), Weight = as.numeric(Weight)) %>% 
  mutate(Date = dmy(paste(1, .$Month, .$Year, sep="-"))) %>% 
  group_by(Sex, Date = floor_date(Date, "month")) %>% 
  summarise("avg_weight" = mean(Weight))

rm(df_2017_raw)


df_2018_raw <- read_fwf(
  file='Nat2018PublicUS.c20190509.r20190717.txt',
  # Fill, Year, Mo, Time, Day, Fill, Sex, Fill, Weight
  fwf_widths(c(8, 4, 2, 8, 1, 451, 1, 28, 4))
)

df_2018_weight <- df_2018_raw %>% 
  select(X2, X3, X7, X9) %>% 
  rename(., Year = X2, Month = X3, Sex = X7, Weight = X9) %>% 
  mutate(Month = as.numeric(Month), Sex = as.factor(Sex), Weight = as.numeric(Weight)) %>% 
  mutate(Date = dmy(paste(1, .$Month, .$Year, sep="-"))) %>% 
  group_by(Sex, Date = floor_date(Date, "month")) %>% 
  summarise("avg_weight" = mean(Weight))

rm(df_2018_raw)


df_2019_raw <- read_fwf(
  file='Nat2019PublicUS.c20200506.r20200915.txt',
  # Fill, Year, Mo, Time, Day, Fill, Sex, Fill, Weight
  fwf_widths(c(8, 4, 2, 8, 1, 451, 1, 28, 4))
)

df_2019_weight <- df_2019_raw %>% 
  select(X2, X3, X7, X9) %>% 
  rename(., Year = X2, Month = X3, Sex = X7, Weight = X9) %>% 
  mutate(Month = as.numeric(Month), Sex = as.factor(Sex), Weight = as.numeric(Weight)) %>% 
  mutate(Date = dmy(paste(1, .$Month, .$Year, sep="-"))) %>% 
  group_by(Sex, Date = floor_date(Date, "month")) %>% 
  summarise("avg_weight" = mean(Weight))

rm(df_2019_raw)

df_total_weight <- rbind(df_2017_weight, df_2018_weight, df_2019_weight) %>% 
  mutate(Year = factor(year(Date)), Month = month(Date)) %>% 
  select(Sex, avg_weight, Month, Year)

ggplot(df_total_weight, aes(Month, avg_weight, group = Sex, color = Sex)) +
  geom_line() +
  facet_wrap(~Year) +
  xlab("Month (Jan - Dec)") +
  ylab("Average Birth Weight (g)") +
  ylim(3100, 3400) +
  labs(color = "Sex") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text=element_text(size=11, face="bold"),
        axis.title=element_text(size=14, face="bold"))


# Cleanup
rm(df_2017_weight, df_2018_weight, df_2019_weight, df_total_weight)


################ PROPORTION OF SEX BY DAY OF THE WEEK ################

df_2017_raw <- read_fwf(
  file='Nat2017PublicUS.c20180516.r20180808.txt',
  # Fill, Year, Mo, Time, Day, Fill, Sex, Fill, Weight
  fwf_widths(c(8, 4, 2, 8, 1, 451, 1, 28, 4))
)

df_2017_weekday <- df_2017_raw %>% 
  select(X2, X3, X5, X7) %>% 
  rename(., Year = X2, Month = X3, Weekday = X5, Sex = X7) %>% 
  mutate(Month = as.numeric(Month), Sex = as.factor(Sex)) %>% 
  mutate(Date = dmy(paste(1, .$Month, .$Year, sep="-"))) %>% 
  group_by(Sex, Weekday, Date = floor_date(Date, "month")) %>% 
  summarise(n = cumsum(n())) %>% 
  mutate("Cummulative" = cumsum(n))

rm(df_2017_raw)


df_2018_raw <- read_fwf(
  file='Nat2018PublicUS.c20190509.r20190717.txt',
  # Fill, Year, Mo, Time, Day, Fill, Sex, Fill, Weight
  fwf_widths(c(8, 4, 2, 8, 1, 451, 1, 28, 4))
)

df_2018_weekday <- df_2018_raw %>% 
  select(X2, X3, X5, X7) %>% 
  rename(., Year = X2, Month = X3, Weekday = X5, Sex = X7) %>% 
  mutate(Month = as.numeric(Month), Sex = as.factor(Sex)) %>% 
  mutate(Date = dmy(paste(1, .$Month, .$Year, sep="-"))) %>% 
  group_by(Sex, Weekday, Date = floor_date(Date, "month")) %>% 
  summarise(n = cumsum(n())) %>% 
  mutate("Cummulative" = cumsum(n))

rm(df_2018_raw)


df_2019_raw <- read_fwf(
  file='Nat2019PublicUS.c20200506.r20200915.txt',
  # Fill, Year, Mo, Time, Day, Fill, Sex, Fill, Weight
  fwf_widths(c(8, 4, 2, 8, 1, 451, 1, 28, 4))
)

df_2019_weekday <- df_2019_raw %>% 
  select(X2, X3, X5, X7) %>% 
  rename(., Year = X2, Month = X3, Weekday = X5, Sex = X7) %>% 
  mutate(Month = as.numeric(Month), Sex = as.factor(Sex)) %>% 
  mutate(Date = dmy(paste(1, .$Month, .$Year, sep="-"))) %>% 
  group_by(Sex, Weekday, Date = floor_date(Date, "month")) %>% 
  summarise(n = cumsum(n())) %>% 
  mutate("Cummulative" = cumsum(n))


rm(df_2019_raw)


df_total_weekday <- rbind(df_2017_weekday, df_2018_weekday, df_2019_weekday) %>% 
  mutate(Year = factor(year(Date)), Month = month(Date)) %>% 
  select(Sex, Month, Year, Weekday, Cummulative)

ggplot(df_total_weekday, aes(Month, Cummulative, group = Sex, color = Sex)) +
  geom_line() +
  facet_grid(Weekday ~ Year) +
  xlab("Month (Jan - Dec)") +
  ylab("Cummulative Births by Sex (M/F), by Weekdays (Mon-Sun)") +
  labs(color = "Sex") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text=element_text(size=11, face="bold"),
        axis.title=element_text(size=14, face="bold"))

rm(df_2017_weekday, df_2018_weekday, df_2019_weekday, df_total_weekday)
