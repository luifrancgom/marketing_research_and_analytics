# Libraries ----
library(tidyverse)
library(skimr)
library(cluster)

# Import data ----
segmentation <- read_csv(file = "000_data/005_data_chapter5.csv")

# Inspect data ----
segmentation |> 
  glimpse()

# Transform data ----
segmentation <- segmentation |> 
  mutate(gender  = factor(x = gender, 
                          ordered = FALSE),
         kids    = as.integer(x = kids),
         ownHome = factor(x = ownHome, 
                          ordered = FALSE),
         subscribe = factor(x = subscribe,
                            ordered = FALSE),
         Segment = factor(x = Segment,
                          ordered = FALSE))

# Summary statistics ----
segmentation |> 
  skim()

# Checking inconsistencies ----
segmentation$income < 0

## Identifying inconsistencies in income ----
segmentation |> 
  filter(income < 0)

segmentation_filter_income <- segmentation |> 
  filter(income >= 0)

# Distances ----
## Euclidean distance ----
customers <- tibble(customer = c("a", "b"),
                    age      = c(45, 23),
                    income   = c(3500, 1500))

### Simple example ----
customers |> 
  select(-customer) |> 
  daisy(metric = "euclidean") |> 
  as_tibble()

### Using segmentation data ----
segmentation_dist_euclidean <- segmentation |> 
  select(age, kids) |> 
  daisy(metric = "euclidean") |> 
  as.matrix() |> 
  as_tibble()

segmentation_dist_euclidean |> 
  select(`1`:`5`) |> 
  slice(1:5)

## Gower distance ----

### Simple examples ----
customers2 <- tibble(Customer = c("a", "b"),
                     Sex = c("Female", "Male"),
                     Income = c(3500, 1500),
                     Satisfaction = c("Medium", "High"),
                     Age = c(45, NA)) |>
  mutate(Sex = factor(x = Sex,
                      ordered = FALSE),
         Satisfaction = factor(x = Satisfaction,
                               levels = c("Low", "Medium", "High"),
                               ordered = TRUE))

customers2 |> 
  select(-Customer) |> 
  daisy(metric = "gower")

customers3 <- tibble(Customer = c("a", "b", "c"),
                     Sex = c("Female", "Male", "Female"),
                     Income = c(3500, 1500, 2000),
                     Satisfaction = c("Medium", "High", "Low"),
                     Age = c(45, NA, 25)) |>
  mutate(Sex = factor(x = Sex,
                      ordered = FALSE),
         Satisfaction = factor(x = Satisfaction,
                               levels = c("Low", "Medium", "High"),
                               ordered = TRUE))

customers3 |>
  select(-Customer) |> 
  daisy(metric = "gower")


customers4 <- tibble(Customer = c("a", "b", "c", "d"),
                     Sex = c("Female", "Male", "Female", "Female"),
                     Income = c(1500, 3500, 2000, 500),
                     Satisfaction = c("Medium", "High", "Low", "Low"),
                     Age = c(NA, 45, 25, 20)) |>
  mutate(Sex = factor(x = Sex,
                      ordered = FALSE),
         Satisfaction = factor(x = Satisfaction,
                               levels = c("Low", "Medium", "High"),
                               ordered = TRUE))

customers4 |>
  select(-Customer) |> 
  daisy(metric = "gower")

### Using segmentation data ----
segmentation_dist_gower <- segmentation |> 
  daisy(metric = "gower") |>  
  as.matrix() |> 
  as_tibble()


segmentation_dist_gower |> 
  select(`1`:`5`) |> 
  slice(1:5)
  
  
