library(xml2)
library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)

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
  
head(homicides)


# Import CDC Minor Mortality Data
cdc_minor_mortality <- read_tsv("CDC_PopEst_0-17_AllDeaths_1999-2016.txt")


# Review data tables and remove duplicate/empty columns, make Rate numeric (maybe calculate separately)
head(cdc_minor_mortality)
cdc_minor_mortality$Notes <- NULL
cdc_minor_mortality$'Year Code' <- NULL
cdc_minor_mortality$`Crude Rate` <- as.numeric(suicides$`Crude Rate`)


# Apply more standard and specific column headings
cdc_minor_mortality <- cdc_minor_mortality %>%
  rename(MinorPopulation = Population) %>%
  rename(MinorDeaths = Deaths) %>%
  rename(StateCode = 'State Code') %>%
  rename(MinorMortalityRate = 'Crude Rate')


# Check result
head(cdc_minor_mortality)

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

population %>%
  group_by(State) %>%
  summarise(PopChg = (Population[Year == 2016] - Population[Year == 1999])/Population[Year == 1999]) %>%
  arrange(desc(PopChg))
  
  
join_key <- c("State", "Year")

suicides %>%
  filter(Population >= 1000000) %>%
  group_by(State) %>%
  summarise(AvgRate = mean(SuicideRate), AvgDeaths = mean(Suicides)) %>%
  arrange(desc(AvgRate)) %>%
  
  View()

homicides %>%
  group_by(State) %>%
  summarise(n(), mean(HomicideRate), mean(Homicides), sd(Homicides))

homicides %>%
  filter(State == "Wyoming")

cdc_homcide_suicide <- left_join(population, homicides, by = join_key) %>%
  left_join(suicides, by = join_key) %>%
  select(State, StateCode = StateCode.x, Year, Population = Population.x, Homicides, HomicideRate, Suicides, SuicideRate)
View(cdc_homcide_suicide)

cdc_aggregate <- cdc_homcide_suicide %>%
  group_by(State) %>%
  summarise(AvgHomicideRate = mean(HomicideRate, na.rm = TRUE), AvgSuicideRate = mean(SuicideRate, na.rm = TRUE), Ratio = AvgHomicideRate/AvgSuicideRate) %>%
  arrange(desc(Ratio))
View(cdc_aggregate)

ggplot(cdc_aggregate, aes(x = AvgHomicideRate, y = AvgSuicideRate)) +
       geom_point(position = "jitter") +
       stat_smooth(method = "lm")




View(anti_join(population, homicides, by = join_key))
