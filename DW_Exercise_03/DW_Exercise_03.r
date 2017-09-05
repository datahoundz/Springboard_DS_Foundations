# File Names and format:
# (1) Date in MM-DD-YYYY format
# (2) Time in XX:YY format
# (3) Code
# (4) Value
# 
# The Code field is deciphered as follows:
#   
#   33 = Regular insulin dose
# 34 = NPH insulin dose
# 35 = UltraLente insulin dose
# 48 = Unspecified blood glucose measurement
# 57 = Unspecified blood glucose measurement
# 58 = Pre-breakfast blood glucose measurement
# 59 = Post-breakfast blood glucose measurement
# 60 = Pre-lunch blood glucose measurement
# 61 = Post-lunch blood glucose measurement
# 62 = Pre-supper blood glucose measurement
# 63 = Post-supper blood glucose measurement
# 64 = Pre-snack blood glucose measurement
# 65 = Hypoglycemic symptoms
# 66 = Typical meal ingestion
# 67 = More-than-usual meal ingestion
# 68 = Less-than-usual meal ingestion
# 69 = Typical exercise activity
# 70 = More-than-usual exercise activity
# 71 = Less-than-usual exercise activity
# 72 = Unspecified special event

# dt$X2 <- str_pad(dt$X2, width = 5, side = "left", pad = "0")


library(data.table)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(Hmisc)

counter <- c(01:70)

for (i in counter) {
  df_name <- sprintf("data_%02d", i)
  src_file <- sprintf("data-%02d", i, '"')
  file_loc <- c(paste("C:/Users/Datahounds/Dropbox/Springboard/R Projects/DW_Exercise_03/Diabetes-Data/", src_file, sep = ""))
  assign(paste(df_name), read_delim(file_loc, "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE))
}

View(data_06)

summary(data_67)
hist(as.numeric(data_67$X4))

bg_date <- mdy(data_01$X1)
bg_time <- hms(data_01$X2)
bg_date_time <- paste(mdy(data_01$X1), hms(data_01$X2))


hist(as.numeric(data_67$X4))
subset(data_67, data_67$X3 == 58)
ggplot(data_67, aes(x = X3, fill = "red")) +
  geom_bar()
  
glimpse(data_67)
str(data_67)


# data_67 issues with time field entries
# can pad with leading 0 to correct some
# others may need to be excluded (>24:00)
# concern about how to automatically detect in other files?
# 
# would like to run sequence below as a function but can't seem to get filename
# to be something other than text in quotes (data_01 vs "data_01") which results in error

subject = data_24

  dt <- as.data.table(subject)
  dt$X4 <- as.numeric(dt$X4)
  dt$X5 <- paste(mdy(dt$X1), hms(dt$X2))
  
  
dt[X3 > 35 & X4 > 20] %>%
  group_by(X3) %>%
  summarise(n(), mean(X4), min(X4), max(X4), min(X2), max(X2))


ggplot(dt[X3 > 35 & X4 > 20], aes(x = X2, y = X4, col = as.factor(X3))) +
  geom_point(position = "jitter") +
  facet_grid(. ~ X3) +
  geom_boxplot(alpha = 0.5)



  

