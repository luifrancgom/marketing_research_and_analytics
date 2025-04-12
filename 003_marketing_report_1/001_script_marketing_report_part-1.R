# Libraries ----
library(sweep)
library(tidyverse)
library(skimr)
library(DT)

# Import data ----
bike_sales <- bike_sales
bike_sales

# Inspect data ----
bike_sales |> 
  glimpse()

# Summarize data ----
## Summary statistics ----
bike_sales |> 
  skim()

## Counting ----
### Products ----
bike_sales |> 
  count(category.secondary, sort = TRUE)

bike_sales |> 
  count(frame, sort = TRUE)

bike_sales |> 
  count(category.secondary,
        frame, sort = TRUE)

### Trying formating the table
bike_sales |> 
  count(category.secondary,
        frame, sort = TRUE) |> 
  datatable()
  
### Clients (B2B)
bike_sales |> 
  count(bikeshop.name, sort = TRUE)

# Data visualization ----
## Histograms ----
bike_sales |> 
  ggplot() +
  geom_histogram(aes(x = quantity),
                 # Number of bins in the histogram
                 bins = 30,
                 color = "black") +
  labs(x = "Quantity of units sold",
       y = "Frequency",
       title = "Distribution of quantity frames sold")

bike_sales |> 
  ggplot() +
  geom_histogram(aes(x = price),
                 bins = 30,
                 color = "black") +
  labs(x = "Price (US Dollars)",
       y = "Frequency",
       title = "Distribution of prices")

## Boxplots ----
bike_sales |> 
  ggplot() +
  geom_boxplot(aes(x = price.ext,
                   y = category.secondary)) +
  scale_x_continuous(labels = scales::label_currency()) +
  labs(x = "Revenue (US Dollars)",
       y = "Category secondary",
       title = "Distribution of revenue by category secondary")

bike_sales |> 
  ggplot() + 
  geom_boxplot(aes(x = price.ext, 
                   y = frame)) +
  scale_x_continuous(labels = scales::label_currency()) +
  labs(x = "Revenue (US Dollars)",
       y = "Frame",
       title = "Distribution of revenue by frame")
  
# Questions answered with data ----
# What are the bike shops that generate more revenue
# to the corporation?

## Parentheses
bike_sales |> 
  summarise(total_revenue = sum(price.ext))

## Prepare data ----
revenue_by_bikeshop <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  mutate(bikeshop.name = fct_reorder(.f = bikeshop.name, 
                                     .x = total_revenue))

revenue_by_bikeshop |> 
  datatable()

## Visualization revenue by bike shop ----
revenue_by_bikeshop |> 
  ggplot() + 
  geom_col(aes(x = total_revenue,
               y = bikeshop.name)) +
  scale_x_continuous(labels = scales::label_currency()) +
  labs(x = "Total revenue (US Dollars)",
       y = "Bike shops",
       title = "Total revenue by bike shop")

## Prepare data ----
revenue_by_category_secondary <- bike_sales |> 
  group_by(category.secondary) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  mutate(category.secondary = fct_reorder(.f = category.secondary, 
                                          .x = total_revenue))

revenue_by_category_secondary

## Visualization total revenue by category secondary ----
revenue_by_category_secondary |> 
  ggplot() +
  geom_col(aes(x = total_revenue, 
               y = category.secondary)) +
  scale_x_continuous(labels = scales::label_currency()) +
  labs(x = "Revenue (US Dollars)",
       y = "Category secondary",
       title = "Total revenue by category secondary")
  

