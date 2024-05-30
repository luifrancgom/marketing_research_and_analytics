# Libraries ----
library(tidyverse)
library(skimr)
library(cluster)

# Import data -----
# segmentation <- read_csv(file = "http://goo.gl/qw303p") |>
#   select(-Segment) 
segmentation <- read_csv(file = "000_data/005_data_chapter5.csv") |> 
  select(-Segment)
segmentation |> head(n = 5)

# Inspect data -----
segmentation |> glimpse()

# Data transformation ----
segmentation <- segmentation |>  
  mutate(gender = factor(x = gender, 
                         ordered = FALSE),
         kids = as.integer(x = kids),
         ownHome = factor(x = ownHome,
                          ordered = FALSE),
         subscribe = factor(x = subscribe,
                            ordered = FALSE))
segmentation

# Summarizing data ----
segmentation |> skim()

# Euclidean distance ----
customers <- tibble(Customer = c("a", "b"),
                    Age = c(45, 23),
                    Income = c(3500, 1500))
customers

customers |> 
  select(-Customer) |> 
  daisy(metric = "euclidean")

# Gower Distance ----
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

customers2

customers2 |> 
  select(-Customer) |> 
  daisy(metric = "gower")

segmentation_dist <- segmentation |> 
  daisy(metric = "gower")

segmentation_dist_tbl <- segmentation_dist |> 
  as.matrix() |> 
  as_tibble()
  
  


  


