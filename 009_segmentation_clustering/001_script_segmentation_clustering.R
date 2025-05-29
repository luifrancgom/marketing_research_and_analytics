# Libraries ----
library(tidyverse)
library(skimr)
library(cluster)

# Import -----
# segmentation <- read_csv(file = "http://goo.gl/qw303p")
segmentation <- read_csv(file = "000_data/005_data_chapter5.csv") |> 
  select(-c(Segment))
segmentation

# Inspect ----
segmentation |> 
  glimpse()

# Transform ----
segmentation <- segmentation |> 
  mutate(across(.cols = c(gender,
                          ownHome,
                          subscribe), 
                .fns = \(x) factor(x = x, 
                                   ordered = FALSE))) |> 
  mutate(kids = as.integer(x = kids))

segmentation

## Anonymous functions ----
my_function <- \(x) x + 2
my_function(x = 3)

# Summary statistics ----
segmentation |> 
  skim()

# Clustering ----
## Distances ----
### Euclidean ----
customers <- tibble(Customer = c("a", "b", "c"),
                    Age      = c(45, 23, 34),
                    Income   = c(3500, 1500, 2000))
customers

customers_dist_euclid <- customers |> 
  select(-Customer) |> 
  daisy(metric = "euclidean")

customers_dist_euclid

### Gower ----
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

customer2_dist_gower <- customers2 |>
  select(-Customer) |> 
  daisy(metric = "gower")
  
customer2_dist_gower


customers3 <- tibble(Customer = c("a", "b", "c"),
                     Sex = c("Female", "Male", "Male"),
                     Income = c(3500, 1500, 1000),
                     Satisfaction = c("High", "Medium", "Low"),
                     Age = c(45, NA, 17)) |>
  mutate(Sex = factor(x = Sex,
                      ordered = FALSE),
         Satisfaction = factor(x = Satisfaction,
                               levels = c("Low", "Medium", "High"),
                               ordered = TRUE))

customers3_dist_gower <- customers3 |> 
  select(-Customer) |> 
  daisy(metric = "gower")
  
customers3_dist_gower

## Segmentation ----
segmentation_dist <- segmentation |> 
  daisy(metric = "gower")
  
segmentation_dist |> 
  as.matrix() |> 
  as_tibble() |> 
  select(`1`:`5`) |> 
  slice(1:5)

## Hierarchical clustering ----
customers4 <- tibble(Customer = c("a", "b", "c", "d", "e"),
                     Sex = c("Female", "Male", "Female", "Female", "Male"),
                     Income = c(3500, 1500, 200, 450, 5000),
                     Satisfaction = c("Medium", "High", "Low", "Low", "Medium"),
                     Age = c(45, NA, 34, 23, 55)) |>
  mutate(Sex = factor(x = Sex,
                      ordered = FALSE),
         Satisfaction = factor(x = Satisfaction,
                               levels = c("Low", "Medium", "High"),
                               ordered = TRUE))

customers4

customers4_dist_gower <- customers4 |> 
  select(-Customer) |> 
  daisy(metric = "gower")
  
customers4_dist_gower  

customers4_hc <- customers4_dist_gower |> 
  hclust(method = "complete")

rect.hclust(tree = customers4_hc,
            k = 2,
            border = "red")

plot(customers4_hc)

customers4_hc_clusters <- customers4_hc |> 
  cutree(k = 2)

customers4 |> 
  mutate(cluster = customers4_hc_clusters)

### Segmentation ----
#### Clusters: 2 ----
segmentation_hc <- segmentation_dist |> 
  hclust(method = "complete")

plot(segmentation_hc)

segmentation_hc |> 
  rect.hclust(k = 2,
              border = "red")

segmentation_hc_clusters2 <- segmentation_hc |> 
  cutree(k = 2)

segmentation_cluster2 <- segmentation |> 
  mutate(cluster = segmentation_hc_clusters2)

segmentation_cluster2

segmentation_cluster |> 
  count(cluster)

#### Clusters: 4 ----
segmentation_hc_cluster4 <- segmentation_hc |> 
  cutree(k = 4)

plot(segmentation_hc)

segmentation_hc |> 
  rect.hclust(k = 4,
              border = "red")

segmentation_cluster4 <- segmentation |> 
  mutate(cluster = segmentation_cluster4)

segmentation_cluster4 |> 
  count(cluster)

  
  
  