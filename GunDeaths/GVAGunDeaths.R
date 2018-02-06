library(xml2)
library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)

setwd("C:/Users/Datahounds/Dropbox/Springboard/R Projects/GunDeaths")

# Items to work on:
# 1. Eliminate comment rows on import or after - DONE
# 2. Replacing suppressed counts w/ State average - ???
# 3. Adrressing missing data rows via Population table - DONE
# 4. Final table layout - DONE
#     State / StateCode / Year / Population / Homicides / HomicideRate / Suicides / SuicideRate
# 
# Issues to consider downstream:
# 1. Linking to other data: Census, BLS, Gun Law Scores, Voting Results
# 2. Investigating alignment of GVA data with CDC data


# Import GVA Teen & Child Accidental Death Data
gva_accidental_teen <- read_csv("GVA_accidental_TEEN.csv")
gva_accidental_child <- read_csv("GVA_accidental_CHILD.csv")


gva_accidental_minor <- bind_rows(Child = gva_accidental_child, Teen = gva_accidental_teen, .id = "Age")

gva_accidental_minor %>%
  select(Date, State, City_County, Killed, Age) %>%
  mutate(Year = as.integer(gsub(".*/","",Date))) %>%
  group_by(Year, State, Age) %>%
  summarise(Incidents = n(), Deaths = sum(Killed)) -> accidental_minor

View(accidental_minor)

population %>%
  filter(Year >= 2014) %>%
  left_join(accidental_minor, by = c("State", "Year")) %>%
  select(State, Year, Population, Incidents, Deaths) %>%
  mutate(Rate = Deaths/Population*1000000) %>%
  group_by(State, Year) -> gva_minor_accident_rate
View(gva_minor_accident_rate)
summary(gva_minor_accident_rate)

gva_minor_accident_rate %>%
  group_by(State) %>%
  summarise(Accidental_Deaths = sum(Deaths), Avg_Accidental_Death_Rate = mean(Rate)) %>%
  arrange(desc(Avg_Accidental_Death_Rate)) -> accidental_minor_aggregate
View(accidental_minor_aggregate)
summary(accidental_minor_aggregate)


# Import GVA Murder-Suicide Data
murder_suicide <- read_csv("GVA_murder-suicide_DV.csv")

# Review data tables and remove duplicate/empty columns, make Rate numeric (maybe calculate separately)
head(murder_suicide)

murder_suicide %>%
  select(Date, State, City_County, Killed) %>%
  mutate(Year = as.integer(gsub(".*/","",Date))) %>%
  group_by(Year, State) %>%
  summarise(Incidents = n(), Deaths = sum(Killed)) -> murder_suicide

  
population %>%
  filter(Year >= 2014) %>%
  left_join(murder_suicide, by = c("State", "Year")) %>%
  select(State, Year, Population, Incidents, Deaths) %>%
  mutate(Rate = Deaths/Population*100000) %>%
  group_by(State, Year) -> gva_ms_rate

gva_ms_rate %>%
  group_by(State) %>%
  summarise(AvgRate = mean(Rate, na.rm = TRUE)) %>%
  arrange(desc(AvgRate)) %>%
  View

View(gva_ms_rate)

population %>%
  filter(Year >= 2014) %>%
  anti_join(murder_suicide, by = join_key) %>%
  View()




