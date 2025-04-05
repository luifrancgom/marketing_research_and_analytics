# Libraries -----
library(tidyverse)
library(skimr)
library(DT)

# Import ----
## Connecting to the cloud
# read_csv(file = "http://goo.gl/UDv12g")
satisfaction_data <- read_csv(file = "000_data/002_data_chapter2.csv")
satisfaction_data

# Pipe operator ----
numbers <- c(1, 2, 3, 4)
square_root <- sqrt(x = numbers)
sum_numbers <- sum(square_root)

sum_numbers <- c(1, 2, 3, 4) |> 
  sqrt() |> 
  sum()

# Exploring data base ----
satisfaction_data |> 
  head(n = 2)

# Transforming data ----
# https://en.wikipedia.org/wiki/Level_of_measurement
satisfaction_data <- satisfaction_data |> 
  mutate(Segment = factor(x = Segment, ordered = FALSE))

satisfaction_data

# Descriptive statistics ----
satisfaction_data |> 
  skim()

# Logical objects ----
satisfaction_data$Segment == 1

# Filter ----
satisfaction_data |> 
  filter(iProdSAT == 3)

satisfaction_data |> 
  filter(iProdSAT >= 3)

satisfaction_data |> 
  filter(iProdSAT < 0)

satisfaction_data$Segment

# Create variables ----
satisfaction_data <- satisfaction_data |> 
  mutate(customer = 1:nrow(satisfaction_data))

satisfaction_data

# Select variables ----
satisfaction_data |> 
  select(customer, Segment)

# Summary statistics ----
mean_median_iprodsat <- satisfaction_data |> 
  summarise(mean_iprodsat = mean(x = iProdSAT), 
            median_iprodsat = median(x = iProdSAT))

# Analyzing data step by step ----
satisfaction_data

satisfaction_data |> 
  group_by(Segment)

group_by_mean <- satisfaction_data |> 
  group_by(Segment) |> 
  summarise(mean_iprodsat = mean(iProdSAT), 
            mean_isalessat = mean(iSalesSAT))
