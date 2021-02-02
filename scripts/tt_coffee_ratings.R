# Tidy Tuesday - Coffee Ratings

# Packages ----------------------------------------------------------------

library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext) # for reorder_within() when facetting ggplots
theme_set(theme_minimal() + theme(panel.border = element_blank(),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank()))

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

# What countries are represented?
df %>% 
  count(country_of_origin, sort = TRUE) %>% 
  print(n = 50)

df %>% 
  filter(!is.na(country_of_origin)) %>% 
  count(country_of_origin) %>% 
  ggplot(aes(x = reorder(country_of_origin, n), y = n)) +
  geom_bar(stat = 'identity', fill = 'dark green') +
  coord_flip() +
  xlab("") +
  ylab("Count") +
  ggtitle("Coffee Reviews by Country of Origin")

ggsave("res/top_countries.png")

# What are the top varieties for the top producing countries?

df %>% 
  group_by(country_of_origin, variety) %>% 
  summarize(n = n()) %>% 
  mutate(total = sum(n),
         pct = n / total) %>% 
  relocate(total, .after = last_col()) %>% 
  arrange(desc(total), country_of_origin, desc(n))

top_countries <- df %>% 
  count(country_of_origin, sort = TRUE) %>% 
  slice_max(order_by = n, n = 6) %>% 
  ungroup()

# Need to fix use of reorder_within...not quite right.
df %>% 
  filter(country_of_origin %in% top_countries$country_of_origin) %>% 
  filter(!is.na(variety)) %>% 
  group_by(country_of_origin, variety) %>% 
  summarize(n = n()) %>% 
  slice_max(order_by = n, n = 5) %>% # Top 5 types for each country
  ggplot(aes(x = tidytext::reorder_within(x = variety, within = country_of_origin, n), 
             y = n,
             fill = country_of_origin)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_x_reordered() + 
  facet_wrap(~country_of_origin, scales = 'free_y') +
  guides(fill = FALSE) + 
  labs(y = "Number of Coffee Reviews",
       x = "Variety",
       title = "What were the most common coffee varieties reviewed in each country?",
       subtitle = "Top 6 Countries by Total Number of Reviews")

ggsave("res/variety_by_country.png")

# What are the top-rated varieties?
# First establish a minimum threshold for number of ratings in order to be included
df %>% 
  filter(!is.na(variety)) %>% 
  mutate(country_variety = paste(country_of_origin, variety, sep = "-")) %>% 
  group_by(country_variety) %>% 
  summarize(number_of_ratings = n()) %>% 
  ggplot(aes(x = number_of_ratings)) + 
  geom_histogram(binwidth = 5)

# What's the range in average scores look like?
df %>% 
  filter(!is.na(variety)) %>% 
  mutate(country_variety = paste(country_of_origin, variety, sep = "-")) %>% 
  group_by(country_variety) %>% 
  summarize(n = n(),
            avg_rating = mean(total_cup_points, na.rm = TRUE)) %>% 
  ungroup() %>% 
  summarize(min = min(avg_rating), med = median(avg_rating), max = max(avg_rating))

# Not too interesting
df %>% 
  filter(!is.na(variety)) %>% 
  mutate(country_variety = paste(country_of_origin, variety, sep = "-")) %>% 
  group_by(country_variety) %>% 
  summarize(n = n(),
            avg_rating = mean(total_cup_points, na.rm = TRUE)) %>% 
  filter(n >= 10) %>% 
  slice_max(order_by = avg_rating, n = 10) %>% 
  ggplot(aes(x = reorder(country_variety, avg_rating), y = avg_rating)) +
  geom_col() +
  coord_flip()

# What's the flavor profile of the highest rated varieties?

top_varieties <- df %>% 
  filter(!is.na(variety)) %>% 
  mutate(country_variety = paste(country_of_origin, variety, sep = "-")) %>% 
  group_by(country_variety) %>% 
  summarize(n = n(),
            avg_rating = mean(total_cup_points, na.rm = TRUE)) %>% 
  filter(n >= 10) %>% # at least 10 ratings
  slice_max(order_by = avg_rating, n = 5) # top 5 varieities

flavor_profile <- df %>% 
  mutate(country_variety = paste(country_of_origin, variety, sep = "-")) %>% 
  filter(country_variety %in% top_varieties$country_variety) %>% 
  select(c(country_variety, aroma, flavor, aftertaste, acidity, 
           body, balance, uniformity, sweetness)) %>% 
  pivot_longer(-c(country_variety)) %>% 
  group_by(country_variety, name) %>% 
  summarize(value = mean(value, na.rm = TRUE)) %>% 
  ungroup()

flavor_profile %>% 
  ggplot(aes(country_variety, value)) +   
  geom_bar(aes(fill = name), position = "dodge", stat="identity") +
  scale_fill_viridis_d() +
  coord_flip() +
  labs(y = "Average Rating",
       x = "",
       title = "What are the flavor profiles of the top-rated varieties?",
       subtitle = "Average Ratings by Flavor Characteristic")

ggsave("res/flavor_profile.png")

# Faceted version

flavor_profile %>% 
  ggplot(aes(x = tidytext::reorder_within(x = name, within = country_variety, value), 
           y = value,
           fill = country_variety)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_x_reordered() + 
  facet_wrap(~country_variety, scales = 'free_y') +
  guides(fill = FALSE) + 
  labs(y = "Average Rating",
       x = "",
       title = "What are the flavor profiles of the top-rated varieties?",
       subtitle = "Average Ratings by Flavor Characteristic")

ggsave("res/flavor_profile_facet.png")

# Best aromas?
flavor_profile %>% 
  filter(name == "aroma") %>% 
  ggplot(aes(reorder(country_variety, value), value)) +   
  geom_bar(fill = "dark green", position = "dodge", stat="identity") +
  coord_flip() +
  labs(y = "Average Rating",
       x = "",
       title = "Top Aromas")

# Most well-balanced?
flavor_profile %>% 
  filter(name == "balance") %>% 
  ggplot(aes(reorder(country_variety, value), value)) +   
  geom_bar(fill = "dark blue", position = "dodge", stat="identity") +
  coord_flip() +
  labs(y = "Average Rating",
       x = "",
       title = "Most Well-Balanced Varieties")
