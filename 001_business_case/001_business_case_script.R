# Libraries ----
library(tidyverse)
library(sweep)

# Import ----
bike_sales <- bike_sales

# Inspect
bike_sales |> 
  View()

bike_sales |> 
  head(n = 3)

bike_sales |> 
  glimpse()

# Count ----
bike_sales |> 
  count(model)

bike_sales |> 
  count(bikeshop.name, customer.id)
  
bike_sales |> 
  count(order.id)
