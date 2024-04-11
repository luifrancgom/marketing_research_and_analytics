# Libraries ----
library(tidyverse)
library(skimr)

# Import data
satisfaction_data <- read_csv(file = "000_data/002_data_chapter2.csv")
satisfaction_data

satisfaction_data |> head()

# Exploring data ----
satisfaction_data |> skim()

# Transform data ----
satisfaction_data <- satisfaction_data |> 
  mutate(Segment = factor(Segment, ordered = FALSE))

satisfaction_data |> skim()

# R objects ----
## Logical
satisfaction_data$iProdSAT == 1
satisfaction_data$iProdSAT[c(1, 2, 3, 4, 5)] > 3
satisfaction_data$iProdSAT[c(1, 500, 50)] >= 4 

as.integer(satisfaction_data$iProdSAT)[1:10]

satisfaction_data$Segment[1:10] > 2

# A way to verify the class of the object
class(satisfaction_data)
class(read.csv(file = "000_data/002_data_chapter2.csv"))

# Create new variables ----
satisfaction_data <- satisfaction_data |> 
  mutate(Customer = 1:500)

# How to select columns ----
satisfaction_data |> 
  select(Segment, Customer)

# Filter the rows ----
satisfaction_data |> 
  filter(iProdSAT >= 5)

satisfaction_data

# Summary statistics ----
satisfaction_data |> 
  summarise(mean_iSalesSAT = mean(iSalesSAT),
            mean_iSalesREC = mean(iSalesREC),
            max_iSalesSAT = max(iSalesSAT))

# Grouping data ----
satisfaction_data |> 
  group_by(Segment) |> 
  summarise(mean_iProdREC = mean(iProdREC))
  
# Manipulating data ----
satisfaction_data |> 
  select(Customer, Segment, iProdSAT) |> 
  filter(iProdSAT >=5) |> 
  group_by(Segment) |> 
  summarise(mean_iProdSAT = mean(iProdSAT))
