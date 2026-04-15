# Load libraries ----
library(tidyverse)
library(skimr)

# Import data ----
# c: comma
# s: separate
# v: values
# read_csv(file = "http://goo.gl/UDv12g")
satisfaction_data <- read_csv(file = "000_data/002_data_set_chapter-2.csv")

# Explore data set ----
satisfaction_data |> 
  head(n = 3)

satisfaction_data |> 
  glimpse()

# Transform ----
satisfaction_data <- satisfaction_data |> 
  mutate(
    Segment = factor(x = Segment, ordered = FALSE)
  )

# Summary statistics ----
satisfaction_data |> 
  skim()

# R objects ----
## Tibble
satisfaction_data
## Factor
satisfaction_data$Segment
satisfaction_data$iProdSAT

satisfaction_data$iProdSAT == 3

# Filter data ----
satisfaction_data |> 
  filter(iProdSAT == 3)

satisfaction_data |> 
  filter(iSalesSAT <= 2)

satisfaction_data |> 
  filter(iSalesSAT <= 2, Segment == 3)

# Select data ----
satisfaction_data |> 
  select(iProdSAT, Segment)

# Summarise data ----
satisfaction_data |> 
  summarise(
    min_isales_rec  = min(iSalesREC),
    max_isales_rec  = max(iSalesREC),
    mean_isales_rec = mean(iSalesREC) 
  )

# Joining everything ----
satisfaction_data |> 
  select(iSalesSAT, Segment) |> 
  filter(Segment %in% c(4, 5)) |> 
  summarise(
    min_isales_sat = min(iSalesSAT),
    max_isales_sat = max(iSalesSAT),
    mean_isales_sat = mean(iSalesSAT)
  )
