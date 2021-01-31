# Tidy Tuesday - Coffee Ratings

# Packages ----------------------------------------------------------------

library(tidytuesdayR)
library(dplyr)
library(ggplot2)
theme_set(theme_minimal())

# Load --------------------------------------------------------------------

tt <- tt_load('2020-07-07')

tt$coffee_ratings

df <- tt$coffee_ratings

# Inspect -----------------------------------------------------------------

head(df)
names(df)
str(df)
summary(df)

# Find unique level - no clear ID here
nrow(df) # 1339
nrow(distinct(df)) # 1339

# Try variables intrinsic to the coffee but not the rating values
# i.e., type, who owns it, where it's produced, when it was produced
# when harvested/graded
names(df)
coffee_vars <- names(df[c(2:20, 41:42)]) 
nrow(distinct(df[all_of(coffee_vars)])) # 1293 - very close

# add who certified it
coffee_vars <- c(coffee_vars, 'certification_contact')
nrow(distinct(df[all_of(coffee_vars)])) # same - 1293

# units
coffee_vars <- c(coffee_vars, 'unit_of_measurement')
nrow(distinct(df[all_of(coffee_vars)])) # same - 1293

# expiration
coffee_vars <- c(coffee_vars, 'expiration')
nrow(distinct(df[all_of(coffee_vars)])) # same - 1293

# Must be something missing from the dataset that allows us to 
# uniquely identify these records without using information
# from the actual ratings assigned...though we're very close
# Create an artificial ID for analysis purposes
df <- df %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

# Explore -----------------------------------------------------------------

# What types of coffee are there?
count(df, species) # overwhelmingly Arabica

df %>% 
  count(variety, sort = TRUE) %>% 
  print(n = 50) # More here - 226 NA's though

df %>% 
  filter(!is.na(country_of_origin)) %>% 
  count(country_of_origin) %>% 
  ggplot(aes(x = reorder(country_of_origin, n), y = n)) +
  geom_bar(stat = 'identity') +
  coord_flip()

# What countries are represented?
df %>% 
  count(country_of_origin, sort = TRUE) %>% 
  print(n = 50)

# What are the top varieties for the top producing countries?

df %>% 
  group_by(country_of_origin, variety) %>% 
  summarize(n = n()) %>% 
  mutate(total = sum(n),
         pct = n / total) %>% 
  relocate(total, .after = last_col()) %>% 
  arrange(desc(total), country_of_origin, desc(n))

df %>% 
  filter(!is.na(country_of_origin)) %>% 
  count(country_of_origin) %>% 
  ggplot(aes(x = reorder(country_of_origin, n), y = n)) +
  geom_bar(stat = 'identity') +
  coord_flip()
  


