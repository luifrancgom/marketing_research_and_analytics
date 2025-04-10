# Libraries ----
library(tidyverse)
library(skimr)
library(DT)

# Import data ----
# read_csv(file = "http://goo.gl/QPDdMl")
weekly_store <- read_csv(file = "000_data/003_data_chapter3.csv")
weekly_store

# Count data ----
weekly_store |> 
  count(Year)

# Tranform data ----
weekly_store <- weekly_store |> 
  mutate(storeNum = factor(x = storeNum, ordered = FALSE),
         Year = factor(x = Year, levels = 1:2, ordered = TRUE),
         Week = factor(x = Week, levels = 1:52, ordered = TRUE),
         p1prom = as.logical(x = p1prom),
         p2prom = as.logical(x = p2prom))

weekly_store

# Inspect data ----
weekly_store |> 
  glimpse()

# Summarize data ----
weekly_store |> 
  skim()

# Visualize data ----
## Histogram ----
### Layer 1 ----
weekly_store |> 
  ggplot()

### Layer 2 ----
weekly_store |> 
  ggplot() +
  geom_histogram(aes(x = p1sales),
                 color = "black",
                 fill = "blue")

### Layer 3 ----
weekly_store |> 
  ggplot() +
  geom_histogram(aes(x = p1sales),
                 color = "black",
                 fill = "green") +
  labs(x = "Sales of product 1",
       y = "Number of units",
       title = "Distribution of sales of product 1")

# Answering questions with data ----
# In what countries the company sell more units 
# of product 2?
weekly_store |> 
  count(country)

## Prepare data ----
weekly_store |> 
  group_by(country)

weekly_store |> 
  group_by(country) |> 
  summarise(sum_p2sales = sum(p2sales))

weekly_store_by_country_without_order <- weekly_store |> 
  group_by(country) |> 
  summarise(sum_p2sales = sum(p2sales))

weekly_store_by_country <- weekly_store |> 
  group_by(country) |> 
  summarise(sum_p2sales = sum(p2sales)) |> 
  mutate(country = fct_reorder(.f = country, 
                               .x = sum_p2sales))

## Creating a bar plot
### Plot 1
weekly_store_by_country_without_order |> 
  ggplot() + 
  geom_col(aes(x = country, 
               y = sum_p2sales))
  
### Plot 2  
weekly_store_by_country |> 
  ggplot() + 
  geom_col(aes(x = country, 
               y = sum_p2sales))

### Plot 3
weekly_store_by_country |> 
  ggplot() + 
  geom_col(aes(x = country, 
               y = sum_p2sales),
           color = "black")

### Plot 4
weekly_store_by_country |> 
  ggplot() + 
  geom_col(aes(x = country,
               y = sum_p2sales),
           color = "black",
           fill = "purple")

weekly_store_by_country |> 
  ggplot() + 
  geom_col(aes(x = country,
               y = sum_p2sales,
               fill = country),
           color = "black")

### Plot 5
weekly_store_by_country |> 
  ggplot() +
  geom_col(aes(x = country, 
               y = sum_p2sales,
               fill = country),
           color = "black") + 
  labs(x = "Country",
       y = "Total sales",
       title = "Total sales of product 2 by country")


