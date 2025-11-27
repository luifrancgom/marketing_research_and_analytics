# Libraries ----
library(tidyverse)
library(skimr)
library(cluster)

# Import ----
segmentation <- read_csv(file = "000_data/005_chapter_5.csv") |> 
  select(-Segment)

# Inspect ----
segmentation |> 
  glimpse()

# Transform ----
segmentation <- segmentation |>
  mutate(gender = factor(gender, ordered = FALSE),
         kids = as.integer(kids),
         ownHome = factor(ownHome, ordered = FALSE),
         subscribe = factor(subscribe, ordered = FALSE))

# Summarize ----
segmentation |> 
  skim()

# Distance ----
a <- c(45, 3500)
b <- c(23, 1500)

(a - b)^2 |> 
  sum() |> 
  sqrt()

## Example 1 ----
customers <- tibble(
  customer = c("a", "b"),
  age      = c(45, 23),
  income   = c(3500, 1500)
)

customers |> 
  select(-customer) |> 
  daisy(
    metric = "euclidean"  
  )

# Example 2
customers2 <- tibble(Customer = c("a", "b", "c"),
                     Sex = c("Female", "Male", "Male"),
                     Income = c(3500, 1500, 150),
                     Satisfaction = c("High", "Medium", "Low"),
                     Age = c(45, NA, 23)) |> 
  mutate(Sex = factor(x = Sex, 
                      ordered = FALSE),
         Satisfaction = factor(x = Satisfaction, 
                               levels = c("Low", "Medium", "High"),
                               ordered = TRUE))

customers2 |>
  select(-Customer) |> 
  daisy(
    metric = "gower"
  )

## Data base ----
segmentation_dist <- segmentation |> 
  daisy(
    metric = "gower"
  ) |> 
  as.matrix() |> 
  as_tibble()
  
segmentation_dist |> 
  select(`1`:`5`) |> 
  slice(1:5)

# Segmentation ----
## Herarchical clustering ----
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

customer3_dist <- customers3 |> 
  select(-Customer) |> 
  daisy(
    metric = "gower"
  )

customers3_hc <- customer3_dist |> 
  hclust(
    method = "complete"
  )

plot(customers3_hc)

customers3_hc |> 
  rect.hclust(
    k = 2
  )

customer3_hc_clusters <- cutree(
  tree = customers3_hc,
  k = 2
  )

customers3 |> 
  mutate(cluster = customer3_hc_clusters)

### Data base ----
segmentation_dist <- segmentation |> 
  daisy(
    metric = "gower"
  )

segmentation_hc <- segmentation_dist |> 
  hclust(
    method = "complete"
  )
  
segmentation_hc_cluster <- segmentation_hc |> 
  cutree(
    k = 4
  )

segmentation_cluster <- segmentation |> 
  mutate(
    cluster = segmentation_hc_cluster
  )
  
segmentation_cluster |> 
  count(cluster)

## K-means
kaufman_example <- tibble(name = c("Ilan", "Jacqueline", "Kim", "Lieve", "Leon", "Peter", "Talia", "Tina"),
                          weight_kg = c(15, 49, 13, 45, 85, 66, 12, 10),
                          height_cm = c(95, 156, 95, 160, 178, 176, 90, 78))

kaufman_example

kaufman_example |> 
  ggplot() +
  geom_point(
    aes(x = weight_kg,
        y = height_cm)
  )
  
  

