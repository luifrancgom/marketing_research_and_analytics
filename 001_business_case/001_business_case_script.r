# Load packages ----
library(tidyverse)
library(sweep)

# Import data ----
bike_sales <- bike_sales

# Inspect ----
bike_sales |> 
    View()

bike_sales |> 
    head()

# Count ----
bike_sales |> 
    count(model) |> 
    View()

bike_sales |> 
    count(bikeshop.name)

bike_sales |> 
    count(order.id)
