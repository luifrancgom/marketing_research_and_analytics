# Packages ----
library(tidyverse)
library(skimr)
library(sweep)

# Import data ----
## Connecting to a server ----

# satisfaction_data <- read_csv(file = "http://goo.gl/UDv12g")
# satisfaction_data |> View()

## Locally
satisfaction_data <- read_csv(file = "000_data/002_data_chapter2.csv")
satisfaction_data

# Exploring data ----
satisfaction_data |> View()
satisfaction_data |> head(n = 5)

summary(object = satisfaction_data)

# Transforming data
## Ordinal variable
### https://es.wikipedia.org/wiki/Nivel_de_medida
satisfaction_data$Segment
satisfaction_data$Segment <- factor(x = satisfaction_data$Segment, 
                                    ordered = FALSE)

satisfaction_data <-  satisfaction_data |> 
  mutate(Segment = factor(x = Segment,
                          ordered = FALSE))
satisfaction_data

satisfaction_data |> skim()

# R objects ----
## Vectors ----

### Logical ----
satisfaction_data$iProdSAT[1:5] == 3
satisfaction_data$iProdSAT[1:5] > 3
satisfaction_data$iProdSAT[1:5] >= 3
satisfaction_data$iProdSAT[1:5] <= 3

### Integers ----
as.integer(satisfaction_data$iSalesSAT[1:10]) 

typeof(satisfaction_data$iSalesSAT)
typeof(as.integer(satisfaction_data$iSalesSAT))

### Factors ----
satisfaction_data$Segment[1:7]

### Data Frames ----
satisfaction_data_frame <- read.csv(file = "000_data/002_data_chapter2.csv")
satisfaction_data_frame

class(satisfaction_data_frame)

### Tibbles ----
class(satisfaction_data)
satisfaction_data

# Creating new variables ----
1:5
1:500
nrow(satisfaction_data)
1:nrow(satisfaction_data)

satisfaction_data <- satisfaction_data |> 
  mutate(customer_id = 1:nrow(satisfaction_data))

satisfaction_data

# Select columns
satisfaction_data |> 
  select(customer_id, iProdSAT)

# Select rows/Filter rows
satisfaction_data |> 
  filter(iProdSAT == 3)

# Summarize variables
satisfaction_data |> 
  summarise(iprodsat_mean = mean(iProdSAT),
            iprodsat_median = median(iProdSAT),
            iprodrec_min = min(iProdREC))

# Grouping variables
satisfaction_data |> 
  group_by(Segment) |> 
  summarise(iprodsat_mean = mean(iProdSAT))

# Project practice ----

## Import data ----
bike_sales <- bike_sales

## Answering questions ----
### Question 1
#### ¿What are the bike shops that generate 
#### more revenue?
revenue_by_bikeshop <- bike_sales |> 
  select(customer.id, bikeshop.name,
         quantity, price) |> 
  mutate(revenue = quantity * price) |> 
  group_by(bikeshop.name) |> 
  summarise(total_revenue = sum(revenue))

revenue_by_bikeshop
