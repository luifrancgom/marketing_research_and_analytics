# Libraries ----
library(tidyverse)
library(skimr)
library(cluster)
library(scales)

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
  
# Hierarchical clustering ----
## Using a simple example ----
customers5 <- tibble(Customer = c("a", "b", "c", "d", "e"),
                     Sex = c("Female", "Male", "Female", "Female", "Male"),
                     Income = c(3500, 1500, 200, 450, 5000),
                     Satisfaction = c("Medium", "High", "Low", "Low", "Medium"),
                     Age = c(45, NA, 34, 23, 55)) |>
  mutate(Sex = factor(x = Sex,
                      ordered = FALSE),
         Satisfaction = factor(x = Satisfaction,
                               levels = c("Low", "Medium", "High"),
                               ordered = TRUE))
customers5

customers5_dist <- customers5 |> 
  select(-Customer) |> 
  daisy(metric = "gower")

customers5_dist |> 
  as.matrix() |> 
  as_tibble()

customers5_hc <- customers5_dist |> 
  hclust(method = "complete")

customers5_hc

plot(customers5_hc)

customers5_hc |> 
  rect.hclust(k = 2, border = "red")

### Extracting clusters ----
customers5_hc_clusters <- customers5_hc |> 
  cutree(k = 2)

customers5 |> 
  mutate(cluster = customers5_hc_clusters)

## Using segmentation data ----
segmentation_dist <- segmentation |> 
  daisy(metric = "gower")

segmentation_hc <- segmentation_dist |> 
  hclust(method = "complete")

plot(segmentation_hc)

segmentation_hc |> 
  rect.hclust(k = 4, border = "red")

segmentation_hc_clusters <- segmentation_hc |> 
  cutree(k = 4)

segmentation |> 
  mutate(cluster = segmentation_hc_clusters)

# K-means ----
kaufman_example <- tibble(name = c("Ilan", "Jacqueline", "Kim", "Lieve", "Leon", "Peter", "Talia", "Tina"),
                          weight_kg = c(15, 49, 13, 45, 85, 66, 12, 10),
                          height_cm = c(95, 156, 95, 160, 178, 176, 90, 78))
kaufman_example

kaufman_example |> 
  ggplot() + 
  geom_point(aes(x = weight_kg,
                 y = height_cm))

## Kmeans using a simple example ----
kaufman_example_kmeans <- kaufman_example |> 
  select(-name) |> 
  kmeans(centers = 2, 
         algorithm = "Hartigan-Wong")
  
kaufman_example_kmeans

### Extracting clusters ----
kaufman_example_kmeans$cluster

kaufman_example |> 
  mutate(cluster = kaufman_example_kmeans$cluster)

## Using segmentation data ----
segmentation_numeric <- segmentation |> 
  select(-Segment) |> 
  mutate(gender = as.integer(gender),
         ownHome = as.integer(ownHome),
         subscribe = as.integer(subscribe))

segmentation_numeric_scale <- segmentation_numeric |> 
  mutate(across(.cols = age:subscribe,
                .fns  = rescale))

segmentation_numeric_scale

set.seed(seed = 1234)

segmentation_numeric_scale_kmeans <- segmentation_numeric_scale |> 
  kmeans(centers = 4, 
         algorithm = "Hartigan-Wong")

### Extracting clusters ----
segmentation_numeric_scale_kmeans$cluster

segmentation |> 
  mutate(cluster = segmentation_numeric_scale_kmeans$cluster)
  

