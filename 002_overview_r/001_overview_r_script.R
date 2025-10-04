# Libraries ----
library(tidyverse)
library(skimr)
library(DT)

# Import ----
satisfaction_data <- read_csv(file = "000_data/002_chapter_2.csv")
# satisfaction_data <- read_csv(file = "http://goo.gl/UDv12g")

satisfaction_data

# Inspect data ----
satisfaction_data |> 
  head(n = 5)

satisfaction_data |> 
  glimpse()

# Transform
satisfaction_data <- satisfaction_data |> 
  mutate(Segment = factor(x = Segment, 
                          ordered = FALSE))
  
# Summarize
satisfaction_data |> 
  skim()

# Creating variables ----
## Generating a vector of numbers
6:17
1:500
nrow(satisfaction_data)
1:nrow(satisfaction_data)

satisfaction_data <- satisfaction_data |> 
  mutate(customer = 1:nrow(satisfaction_data))

# Filtering
satisfaction_data |> 
  filter(customer == 100)

satisfaction_data |> 
  filter(iProdSAT >= 2)

## Explaining filter ----
1:10 >= 5

# Summarize ----
satisfaction_data |> 
  summarise(mean_prod_sat = mean(iProdSAT),
            median_prod_sat = median(iProdSAT),
            sd_prod_sat = sd(iProdSAT),
            min_prod_sat = min(iProdSAT),
            max_prod_sat = max(iProdSAT))

# Select ----
satisfaction_data |> 
  select(iProdSAT, iProdREC)

# Application ----
## Innefective way
satisfaction_data |> 
  filter(Segment == 1) |> 
  summarise(mean_prod_sat = mean(iProdSAT),
            median_prod_sat = median(iProdSAT))

satisfaction_data |> 
  filter(Segment == 2) |> 
  summarise(mean_prod_sat = mean(iProdSAT),
            median_prod_sat = median(iProdSAT))

## Efective way
satisfaction_data_prod_sat <- satisfaction_data |> 
  group_by(Segment) |> 
  summarise(
    mean_prod_sat = mean(iProdSAT),
    median_prod_sat = median(iProdSAT)
  )

# Table
## Documentation
## https://rstudio.github.io/DT/
satisfaction_data_prod_sat |> 
  datatable(colnames = 
              c("Mean Product Satisfaction" = "mean_prod_sat",
                "Median Product Satisfaction" = "median_prod_sat")) |> 
  formatRound(columns = "Mean Product Satisfaction", 
              digits = 2)


