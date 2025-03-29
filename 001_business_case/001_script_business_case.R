# Libraries ----
library(tidyverse)
library(sweep)

# Import ----
bike_sales <- bike_sales

# Inspect ----
bike_sales |> View()
?bike_sales

# Counting ----
## Frame ----
bike_sales |> 
  count(frame)
## Bikeshops ----
bike_sales |> 
  count(customer.id, bikeshop.name)




