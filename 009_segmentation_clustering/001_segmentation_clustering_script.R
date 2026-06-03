# Libraries ----
library(tidyverse)
library(skimr)
library(cluster)
library(parameters)

# Import data ----
# "/data/data_sets_marketing/005_data_set_chapter-5.csv"
segmentation <- read_csv(
  file = "000_data/005_data_set_chapter-5.csv"
) |> 
  select(-Segment)

# Inspect data ----
segmentation |> 
  glimpse()

# Transform data ----
segmentation <- segmentation |> 
  mutate(
    gender = factor(x = gender, ordered = FALSE),
    kids = as.integer(x = kids),
    ownHome = factor(x = ownHome, ordered = FALSE),
    subscribe = factor(x = subscribe, ordered = FALSE)
  )

# Summarize data ----
segmentation |> 
  skim()

# Segmentation ----
## Distances ----
### Euclidean ----
a <- c(45, 3500)
b <- c(23, 1500)

(a - b)^2 |> 
  sum() |> 
  sqrt()

customers <- tibble(
  Customer = c("a", "b"),
  Age = c(45, 23),
  Income = c(3500, 1500)
)

customers |> 
  select(-Customer) |> 
  daisy(metric = "euclidean")

### Gower ----
customers2 <- tibble(
  Customer = c("a", "b"),
  Sex = c("Female", "Male"),
  Satisfaction = c("Medium", "High"),
  Age = c(45, NA),
  Income = c(3500, 1500)
) |> 
  mutate(
    Sex = factor(x = Sex, ordered = FALSE),
    Satisfaction = factor(
      x = Satisfaction,
      levels = c("Low", "Medium", "High"), 
      ordered = TRUE)
  )

customers2 |> 
  select(-Customer) |> 
  daisy(metric = "gower")

customers21 <- tibble(
  Customer = c("a", "b", "c"),
  Sex = c("Female", "Male", "Male"),
  Satisfaction = c("Low", "Medium", "High"),
  Age = c(45, NA, 22),
  Income = c(3500, 1500, 1300)
) |> 
  mutate(
    Sex = factor(x = Sex, ordered = FALSE),
    Satisfaction = factor(
      x = Satisfaction,
      levels = c("Low", "Medium", "High"), 
      ordered = TRUE)
  )

customers21 |> 
  select(-Customer) |> 
  daisy(metric = "gower")

segmentation_dist <- segmentation |> 
  daisy(metric = "gower")

segmentation_dist |> 
  as.matrix() |> 
  as_tibble() |> 
  select(`1`:`5`) |> 
  slice(1:5)

## Clusters ----
### Hierarchical clustering ----
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

customers3_dist <- customers3 |> 
  select(-Customer) |> 
  daisy(metric = "gower")

customers3_hclust <- hclust(
  d = customers3_dist,
  method = "complete"
)

plot(customers3_hclust)
rect.hclust(
  tree = customers3_hclust, 
  k = 2, 
  border = "red" 
)

customers3_hc_clusters <- customers3_hclust |> 
  cutree(k = 2)


customers3 |> 
  mutate(cluster = customers3_hc_clusters)


segmentation_hclust <- hclust(
  d = segmentation_dist, 
  method = "complete"
) 
plot(segmentation_hclust)
rect.hclust(
  tree = segmentation_hclust, 
  k = 4,
  border = "red" 
)

segmentation_hc_cluster <- segmentation_hclust |> 
  cutree(k = 4)

segmentation |> 
  mutate(cluster = segmentation_hc_cluster)

n_clusts_segmentation <- segmentation |> 
  n_clusters(
    standardize = TRUE,
    nbclust_method = "complete",
    n_max = 10
  )

n_clusts_segmentation |> 
  as_tibble() |> 
  arrange(n_Clusters) |> 
  View()

n_clusts_segmentation |> 
  as_tibble() |>
  count(n_Clusters) |> 
  arrange(desc(n))

### K-means ----
kaufman_example <- tibble(name = c("Ilan", "Jacqueline", "Kim", "Lieve", "Leon", "Peter", "Talia", "Tina"),
                          weight_kg = c(15, 49, 13, 45, 85, 66, 12, 10),
                          height_cm = c(95, 156, 95, 160, 178, 176, 90, 78))

kaufman_example

kaufman_example |> 
  ggplot() + 
  geom_point(aes(x = weight_kg, y = height_cm))

set.seed(seed = 1234)
kaufman_example_kmeans <- kaufman_example |> 
  select(-name) |> 
  kmeans(
    centers = 2,
    algorithm = "Hartigan-Wong"
  )

kaufman_example_kmeans_clusters <- kaufman_example |> 
  mutate(
    cluster = kaufman_example_kmeans$cluster
  )

kaufman_example_kmeans_clusters

n_clusts_kaufman_example <- kaufman_example |> 
  select(-name) |> 
  n_clusters(
    standardize = TRUE,
    nbclust_method = "kmeans",
    n_max = 7
  )

n_clusts_kaufman_example

n_clusts_kaufman_example |> 
  as_tibble()
