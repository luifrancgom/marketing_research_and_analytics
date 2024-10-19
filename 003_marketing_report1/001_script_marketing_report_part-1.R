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
## Descriptive statistics ----
bike_sales |> 
  skim()

## Counting ----
bike_sales |> 
  count(category.secondary) |> 
  datatable()

bike_sales |> 
  count(frame)

# Potential combinations: 18

bike_sales |> 
  count(category.secondary, frame)

# Data visualization ----
## Histogram ----
bike_sales |> 
  ggplot() +
  geom_histogram(aes(x = price),
                 color = "black") +
  labs(x = "Prices of the different frames",
       y = "Frequency",
       title = "Distribution of prices for the different frames")

bike_sales |> 
  ggplot() +
  geom_histogram(aes(x = quantity),
                 color = "black") +
  labs(x = "Quantities of the different sales of frames",
       y = "Frequency",
       title = "Distribution of quantities sold for the different frames")
  
## Boxplot ----
bike_sales |> 
  ggplot() + 
  geom_boxplot(aes(x = price.ext, 
                   y = category.secondary)) +
  labs(x = "Revenue per transaction",
       y = "",
       title = "Distribution of the revenue per transaction by second category")

bike_sales |> 
  ggplot() + 
  geom_boxplot(aes(x = price.ext, 
                   y = frame))  +
  labs(x = "Revenue per transaction",
       y = "",
       title = "Distribution of the revenue per transaction by frame")

# Questions answered with data ----
## What are the bike shops, bikeshop.name, 
## that generate more revenue, price.ext, 
## to the corporation?

## Prepare data ----
revenue_by_bikeshops <- bike_sales |> 
  select(bikeshop.name, price.ext) |> 
  group_by(bikeshop.name) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  mutate(bikeshop.name = fct_reorder(.f = bikeshop.name,
                                     .x = total_revenue))

revenue_by_bikeshops |> 
  ggplot() +
  geom_col(aes(x = total_revenue,
               y = bikeshop.name))



