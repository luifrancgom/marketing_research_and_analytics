# Load libraries ----
library(tidyverse)
library(sweep)

# Import data ----
bike_sales <- bike_sales

# Exploring data set ----

## Operator example ----
c(1, 2, 3, 4, 5) |> 
  sum()

## View data set ----
bike_sales |>
  View()

## Count ----
count_model <- bike_sales |> 
  count(
    model,
    sort = TRUE
  )

count_model |> 
  View()

count_bikeshop <- bike_sales |> 
  count(bikeshop.name)

count_order <- bike_sales |> 
  count(order.id)
