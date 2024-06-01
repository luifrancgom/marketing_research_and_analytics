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
  
# Hierarchical clustering ----
customers3 <- tibble(Customer = c("a", "b", "c", "d", "e"),
                     Sex = c("Female", "Male", "Female", "Female", "Male"),
                     Income = c(3500, 1500, 200, 450, 5000),
                     Satisfaction = c("Medium", "High", "Low", "Low", "Medium"),
                     Age = c(45, NA, 34, 23, 55)) |>
  mutate(Sex = factor(x = Sex,
                      ordered = FALSE),
         Satisfaction = factor(x = Satisfaction,
                               levels = c("Low", "Medium", "High"),
                               ordered = TRUE))
customers3

customer3_dist <- daisy(x = select(customers3, 
                                   -Customer),
                        metric = "gower")

customers3_hc <- hclust(d = customer3_dist,
                        method = "complete")
  
customers3_hc
plot(customers3_hc)
rect.hclust(tree = customers3_hc, 
            k = 2,
            border = "blue")

customers3_hc_clusters <- cutree(tree = customers3_hc,
                                 k = 2)
customers3_hc_clusters

customers3 |> 
  mutate(cluster = customers3_hc_clusters) |> 
  filter(cluster == 2)

# k-means ----
kaufman_example <- tibble(name = c("Ilan", "Jacqueline", "Kim", "Lieve", "Leon", "Peter", "Talia", "Tina"),
                          weight_kg = c(15, 49, 13, 45, 85, 66, 12, 10),
                          height_cm = c(95, 156, 95, 160, 178, 176, 90, 78))
kaufman_example

kaufman_example_kmeans <- kaufman_example |> 
  select(-name) |> 
  kmeans(centers = 2, 
         iter.max = 1000,
         algorithm = "Hartigan-Wong")

kaufman_example_kmeans

kaufman_example_kmeans$cluster

kaufman_example_kmeans_clusters <- kaufman_example |> 
  mutate(cluster = kaufman_example_kmeans$cluster)
  
kaufman_example_kmeans_clusters  

# k-means apply to segmentation
segmentation

segmentation_numeric <- segmentation |>
  mutate(gender = as.integer(gender),
         ownHome = as.integer(ownHome),
         subscribe = as.integer(subscribe))
segmentation_numeric

scales::rescale(x = 1:10)

segmentation_numeric_scale <- segmentation_numeric |> 
  mutate(across(.cols = age:subscribe, 
                .fns = scales::rescale))

set.seed(seed = 1234)
segmentation_numeric_scale_kmeans <- segmentation_numeric_scale |> 
  kmeans(centers = 4,
         algorithm = "Hartigan-Wong")

segmentation_numeric_scale_kmeans$cluster

segmentation_kmeans_clusters <- segmentation |> 
  mutate(cluster = segmentation_numeric_scale_kmeans$cluster)

segmentation_kmeans_clusters

  
  


