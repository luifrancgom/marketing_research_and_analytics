# Libraries ----
library(tidyverse)
library(sweep)
library(skimr)

# Import ----
bike_sales <- bike_sales

# Inspect
bike_sales |> 
  glimpse()

# Summarize
bike_sales |> 
  skim()
