# Gun Law Data Sources

library(xml2)
library(readxl)
library(XLConnect)
library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)

giffords_grade <- read.csv("giffords_gunlawscorecard.csv")
state_firearm_laws <- read.csv("state_gun_law_database.csv")
letter_grade_table <- read.csv("LetterGradeConverter.csv")
guns_ammo_rank <- read.csv("guns_ammo_rankings.csv")
fips_codes <- read_excel("State_FIPS_Codes.xlsx")
region_codes <- read_excel("State_FIPS_Codes.xlsx", 2)

head(giffords_grade)
head(letter_grade_table)
str(state_firearm_laws)
str(fips_codes)
str(region_codes)

giffords_grade %>%
  left_join(letter_grade_table, by = c("GunLawGrade" = "Letter")) %>%
  select(State, Year, GunLawGrade, GunLawScore = GPA, GunLawRank, GunDeathRank, BkgrndCheck) %>%
  group_by(State) %>%
  left_join(fips_codes, by = "State") %>%
  group_by(Region_Code, State) %>%
  summarise(AvgLawGrade = mean(GunLawScore), AvgLawRank = mean(GunLawRank), AvgDeathRank = mean(GunDeathRank)) %>%
  arrange(Region_Code, desc(AvgLawGrade), AvgLawRank) -> giffords_aggregate
View(giffords_aggregate)


ggplot(giffords_aggregate, aes(AvgLawRank, AvgDeathRank, col = as.factor(Region_Code))) +
  geom_point(position = "jitter") +
  stat_smooth(method = "lm", se = FALSE)

state_firearm_laws %>%
  select(State = state, Year = year, TotalLaws = lawtotal) %>%
  filter(Year >= 1999) %>%
  group_by(State) %>%
  summarise(AvgLawCount = mean(TotalLaws)) %>%
  arrange(desc(AvgLawCount)) -> firearm_laws_aggregate
View(firearm_laws_aggregate)

state_firearm_laws %>%
  select(State = state, Year = year, TotalLaws = lawtotal) %>%
  inner_join(cdc_homcide_suicide, by = join_key) %>%
  filter(Population >= 1000000, Year >= 2008) %>%
  left_join(fips_codes, by = "State") %>%
  group_by(Region_Code, State) %>%
  ggplot(aes(TotalLaws, SuicideRate, col = as.factor(Region_Code))) +
  geom_point(position = "jitter") +
  stat_smooth(method = "lm", se = FALSE)

head(guns_ammo_rank)

guns_ammo_rank %>%
  filter(Year == 2015) %>%
  left_join(cdc_homcide_suicide, by = join_key) %>%
  left_join(fips_codes, by = "State") %>%
  group_by(Region_Code, State) %>%
  ggplot(aes(GunsAmmoRank, SuicideRate, col = as.factor(Region_Code))) +
  geom_point(position = "jitter") +
  stat_smooth(method = "lm", se = FALSE)


guns_ammo_rank %>%
  left_join(gva_ms_rate, by = join_key) %>%
  filter(Year == 2015, State != "Alaska") %>%
  ggplot(aes(GunsAmmoRank, Rate)) +
  geom_point(position = "jitter") +
  stat_smooth(method = "lm")
  


