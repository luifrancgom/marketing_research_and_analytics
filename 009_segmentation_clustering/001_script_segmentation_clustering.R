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

segmentation_cluster2 |> 
  count(cluster)

#### Clusters: 4 ----
segmentation_hc_cluster4 <- segmentation_hc |> 
  cutree(k = 4)

plot(segmentation_hc)

segmentation_hc |> 
  rect.hclust(k = 4,
              border = "red")

segmentation_cluster4 <- segmentation |> 
  mutate(cluster = segmentation_hc_cluster4)

segmentation_cluster4 |> 
  count(cluster)

## K-means ----
### Kaufamn example ----
kaufman_example <- tibble(name = c("Ilan", "Jacqueline", "Kim", 
                                   "Lieve", "Leon", "Peter", 
                                   "Talia", "Tina"),
                          weight_kg = c(15, 49, 13, 45, 
                                        85, 66, 12, 10),
                          height_cm = c(95, 156, 95, 160, 
                                        178, 176, 90, 78))
kaufman_example

kaufman_example |> 
  ggplot() + 
  geom_point(aes(x = weight_kg,
                 y = height_cm))

kaufman_example_kmean <- kaufman_example |> 
  select(-name) |> 
  kmeans(centers = 2, 
         algorithm = "Hartigan-Wong")
  
kaufman_example_kmean$cluster

kaufman_example_kmean_clusters <- kaufman_example |> 
  mutate(cluster = kaufman_example_kmean$cluster)

kaufman_example_kmean_clusters

### Segmentation numeric ----
segmentation_numeric <- segmentation |> 
  mutate(across(.cols = c(gender,
                          ownHome,
                          subscribe), 
                .fns = as.integer))

segmentation_number_scale <- segmentation_numeric |> 
  mutate(across(.cols = age:subscribe, 
                .fns = scales::rescale))
  
segmentation_number_scale

#### Re-scale variables ----
1:10
scales::rescale(x = 1:10)

segmentation_number_scale_kmeans <- segmentation_number_scale |> 
  kmeans(centers = 4, 
         algorithm = "Hartigan-Wong")

segmentation_number_scale_kmeans$cluster

segmentation_number_scale_clusters <- segmentation |> 
  mutate(cluster = segmentation_number_scale_kmeans$cluster)
  
segmentation_number_scale_clusters  


  