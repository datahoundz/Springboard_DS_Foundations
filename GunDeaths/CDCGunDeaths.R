library(xml2)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Datahounds/Dropbox/Springboard/R Projects/GunDeaths")

# Items to work on:
# 1. Eliminate comment rows on import or after
# 2. Replacing suppressed counts w/ State average
# 3. Adrressing missing data rows via Population table
# 4. Final table layout
#     State / StateCode / Year / Population / Homicides / HomicideRate / Suicides / SuicideRate
# 
# Issues to consider downstream:
# 1. Linking to other data: Census, BLS, Gun Law Scores, Voting Results
# 2. Investigating alignment of GVA data with CDC data



# Import CDC Suicide Data
suicides <- read_tsv("CDC_FirearmSuicide_1999-2016.txt")


# Review data tables and remove duplicate/empty columns, make Rate numeric (maybe calculate separately)
head(suicides)
suicides$Notes <- NULL
suicides$'Year Code' <- NULL
suicides$`Crude Rate` <- as.numeric(suicides$`Crude Rate`)


# Apply more standard and specific column headings
suicides <- suicides %>%
  rename(Suicides = Deaths) %>%
  rename(StateCode = 'State Code') %>%
  rename(SuicideRate = 'Crude Rate')


# Check result
head(suicides)



# Import CDC Homicide Data
homicides <- read_tsv("CDC_FirearmHomicide_1999-2016.txt")

# Repeat same sequence as above but adjusting for homicides
homicides$Notes <- NULL
homicides$'Year Code' <- NULL
homicides$`Crude Rate` <- as.numeric(homicides$`Crude Rate`)

homicides <- homicides %>%
  rename(Homicides = Deaths) %>%
  rename(StateCode = 'State Code') %>%
  rename(HomicideRate = 'Crude Rate')
  
View(homicides)


# Import CDC Population Data (baseline for combining data)
population <- read_tsv("CDC_PopEst_1990-2016.txt")


# Similar adjustments for population table
population$Notes <- NULL
population$`Yearly July 1st Estimates Code` <- NULL

population <- population %>%
  rename(Year = `Yearly July 1st Estimates`) %>%
  rename(StateCode = 'State Code')
  
population <- population %>%
  filter(Year >= 1999)

View(population)
51*18



suicides %>%
  group_by(State) %>%
  summarise(mean(SuicideRate), mean(Suicides))

homicides %>%
  group_by(State) %>%
  summarise(mean(HomicideRate), mean(Homicides))


homicides %>%
  filter(State == "Wyoming")


