# 0: Load the data in RStudio
# 
# Save the data set as a CSV file called refine_original.csv and load it in
# RStudio into a data frame.

library(readr)
refine_original <- read_csv("C:/Users/scotland/Dropbox/Springboard/R Projects/DW_Exercise_01/refine_original.csv")

library(dplyr)
library(tidyr)

refine_df <- as.data.frame(refine_original)
glimpse(refine_df)
View(refine_df)

# 1: Clean up brand names
# 
# Clean up the 'company' column so all of the misspellings of the brand names
# are standardized. For example, you can transform the values in the column to
# be: philips, akzo, van houten and unilever (all lowercase).

# Created list of clean names against which to check existing company name

clean_name <- c("philips", "akzo", "van houten", "unilever")

# For loop uses agrep() to check each clean name against list of company
# names with 30% fuzzy factor and replaces fuzzy matches with clean name.
# Very happy to stumble on agrep() while looking through string matching
# functions!

for (x in clean_name) {
  refine_df$company[agrep(x, refine_df$company, max.distance = 0.3, ignore.case = TRUE)] <- x
}

print(refine_df$company)


# 2: Separate product code and number
# 
# Separate the product code and product number into separate columns i.e. add
# two new columns called product_code and product_number, containing the product
# code and number respectively

refine_df <- separate(refine_df, "Product code / number", 
                      c("product_code", "product_number"), 
                      sep = "-", remove = TRUE, convert = TRUE)


# # 3: Add product categories
# 
# You learn that the product codes actually represent the following product
# categories:
# 
# p = Smartphone
# v = TV
# x = Laptop
# q = Tablet
# 
# In order to make the data more readable, add a column with the product
# category for each record.

refine_df <- mutate(refine_df, product_category = if_else(product_code == "p", "Smartphone", 
                                                          if_else(product_code == "v", "TV",
                                                                  if_else(product_code == "x", "Laptop", "Tablet"))))



# 4: Add full address for geocoding
# 
# You'd like to view the customer information on a map. In order to do that, the
# addresses need to be in a form that can be easily geocoded. Create a new
# column full_address that concatenates the three address fields (address, city,
# country), separated by commas.

refine_df <- mutate(refine_df, full_address = paste(address, city, country, sep = ","))



# # 5: Create dummy variables for company and product category
# 
# Both the company name and product category are categorical variables i.e. they
# take only a fixed set of values. In order to use them in further analysis you
# need to create dummy variables. Create dummy binary variables for each of them
# with the prefix company_ and product_ i.e.,
# 
# Add four binary (1 or 0) columns for company: company_philips, company_akzo,
# company_van_houten and company_unilever.
# 
# Add four binary (1 or 0) columns for product category: product_smartphone,
# product_tv, product_laptop and product_tablet.

refine_df <- refine_df %>% 
  mutate(product_smartphone = if_else(product_category == "Smartphone", 1, 0)) %>% 
  mutate(product_tv = if_else(product_category == "TV", 1, 0)) %>%   
  mutate(product_laptop = if_else(product_category == "Laptop", 1, 0)) %>% 
  mutate(product_tablet = if_else(product_category == "Tablet", 1, 0))

refine_df <- refine_df %>% 
  mutate(company_philips = if_else(company == "philips", 1, 0)) %>% 
  mutate(company_akzo = if_else(company == "akzo", 1, 0)) %>%   
  mutate(company_van_houten = if_else(company == "van houten", 1, 0)) %>% 
  mutate(company_unilever = if_else(company == "unilever", 1, 0))

View(refine_df)



# Very frustrated as wanted to find a cleaner way to do the above exercise
# Tried to create a lookup table to source column names and criteria
# Could not find a way to set new column name to table value in mutate
# Then tried to do something similar using a for loop over a vector
# and had the same problem creating column name from vector in mutate function


#   dv_product <- refine_df %>%
#     distinct(product_category)
#   dv_product <- dv_product %>%
#     mutate(product_category, dv_cat = tolower(paste("product_", product_category, sep = "")))
#   mutate(refine_df, dv_product$dv_cat = if_else(dv_product$product_category == refine_df$product_category, 1, 0))
#   mutate(refine_df, dv_cat[1,2] = if_else(dv_product$product_category == refine_df$product_category, 1, 0))
#   mutate(refine_df, p_tablet = if_else(dv_product[1,1] == refine_df$product_category, 1, 0))
# Last entry shows table reference works in criteria, just not for column name

# Then tried to do something similar using a for loop over a vector
# and had the same problem creating column name in mutate function

#   dv_cat <- tolower(paste("product_", c(unique(refine_df$product_category)), sep = ""))
#   for (x in dv_cat)
#   mutate(refine_df, x = 1)

# ended up with a column named "x"
