# Libraries ----
library(sweep)
library(tidyverse)

# Data ----
## Explore bike_sales data base
bike_sales
?bike_sales
dim(bike_sales)

# Pipe operator ---- 
my_vector <- c(-1, -2, -3, -4, -5)

sqrt(abs(sum(my_vector)))

my_vector |>
  # Sum the components of the vector
  sum() |> 
  # Calculate the absolute value
  abs() |> 
  # Calculate the square root
  sqrt()

bike_sales |> 
  head()

bike_sales |> 
  View()