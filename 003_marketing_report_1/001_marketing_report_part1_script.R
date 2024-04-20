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
  select(bikeshop.name, price.ext) |> 
  group_by(bikeshop.name) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  arrange(desc(total_revenue))

bike_sales_by_bikeshop_revenue |> 
  head()

bike_sales_by_bikeshop_revenue |> 
  tail()

bike_sales_by_bikeshop_revenue

# Checking revenue by second category
bike_sales_by_bikeshop_second_category <- bike_sales |> 
  select(category.secondary, price.ext) |> 
  group_by(category.secondary) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  arrange(desc(total_revenue))

bike_sales_by_bikeshop_second_category

# visualization ----
bike_sales |> 
  ggplot() +
  geom_boxplot(aes(x = price.ext, 
                   y = category.secondary)) +
  # scale_x_continuous(breaks = seq.int(from = 0, to = 10000000, by = 1000000)) +
  labs(x = "Revenue by transaction (Dollars)",
       y = "Bike models by second category",
       title = "Distribution of revenue by second category")

bike_sales |> 
  ggplot() +
  geom_boxplot(aes(x = price.ext, 
                   y = frame)) +
  scale_x_continuous(labels = scales::label_dollar()) +
  labs(x = "Revenue by transaction (Dollars)",
       y = "Bike models by frame",
       title = "Distribution of revenue by frame")  


bike_sales_by_bikeshop_revenue |> 
  mutate(bikeshop.name = fct_reorder(bikeshop.name, 
                                     total_revenue)) |> 
  ggplot() +
  geom_col(aes(x = total_revenue, 
               y = bikeshop.name))

