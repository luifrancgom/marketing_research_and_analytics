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
  
