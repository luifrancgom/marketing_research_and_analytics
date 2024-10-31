# Libraries ----
library(tidyverse)
library(skimr)

# Import data ----
segmentation <- read_csv(file = "000_data/005_data_chapter5.csv")
segmentation

# Inspect data ----
segmentation |> 
  glimpse()

# Transform data ----
segmentation <- segmentation |> 
  mutate(gender = factor(x = gender,
                         ordered = FALSE),
         kids = as.integer(kids),
         ownHome = factor(x = ownHome,
                          ordered = FALSE),
         subscribe = factor(x = subscribe,
                            ordered = FALSE),
         Segment = factor(x = Segment,
                          ordered = FALSE))
  
# Summarize data ----
segmentation |> 
  skim()

# Count data ----
segmentation |> 
  count(ownHome)

# Summary statistics by groups ----
## Question 1: Are there differences 
##             between segments in
##             relation to income?
segmentation |> 
  select(Segment, income) |> 
  group_by(Segment) |> 
  summarise(mean_income = mean(income))

segmentation |> 
  select(ownHome, income) |> 
  group_by(ownHome) |> 
  summarise(mean_income = mean(income))

segmentation |> 
  group_by(ownHome, Segment) |> 
  summarise(mean_income = mean(income)) |> 
  ungroup()

## Question 2: Are there differences 
##             between segments in
##             relation to the number
##             of kids?
segmentation |> 
  group_by(ownHome, Segment) |> 
  summarise(number_kids = sum(kids)) |> 
  ungroup()

## Question 3: What are the percentage
##             of subscribers by Segment
##             and by the status of owning
##             a house
subscriber_by_segment_home_ownersh <- segmentation |> 
  count(subscribe, ownHome, Segment) |> 
  group_by(ownHome, Segment) |> 
  mutate(number_percentage = (n / sum(n))*100) |> 
  ungroup() |> 
  arrange(Segment, ownHome)

subscriber_by_segment_home_ownersh

### Example how to arrange ----
df <- tibble(
  name = c("Alice", "Bob", "Charlie", "David"),
  age = c(25, 30, 22, 28)
)

df_sorted <- df |>  
  arrange(age)

## Visualization by groups ----

