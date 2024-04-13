# Libraries ----
library(sweep)
library(tidyverse)

# Import data ----
bike_sales <- bike_sales

# Inspect data ----
bike_sales |> head()
bike_sales |> View()
bike_sales |> str()
bike_sales |> glimpse()

# Summarize data ----
bike_sales |> summary()
bike_sales |> skim()

# Exploring the available products
bike_sales |> 
  group_by(category.primary) |> 
  summarise(n = n())

bike_sales |> 
  count(category.primary)

# 9 models
bike_sales |> count(category.secondary)

# 2 types of frames
bike_sales |> count(frame)

bike_sales |> 
  count(category.secondary, frame)

bike_sales |> 
  count(bikeshop.name, sort = TRUE)

bike_sales |> 
  count(bikeshop.name, sort = TRUE) |> 
  View()

# Checking revenue by customer
bike_sales_by_bikeshop_revenue <- bike_sales |> 
  mutate(revenue = price * quantity) |> 
  select(bikeshop.name, revenue) |> 
  group_by(bikeshop.name) |> 
  summarise(total_revenue = sum(revenue)) |> 
  arrange(desc(total_revenue))

bike_sales_by_bikeshop_revenue
  