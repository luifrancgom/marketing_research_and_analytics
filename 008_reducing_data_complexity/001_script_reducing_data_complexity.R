# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(tidyheatmaps)

# Import data ----
consumer_brand <- read_csv(file = "000_data/008_data_chapter8.csv")

# Inspect data ----
consumer_brand |> 
  glimpse()

# Count data -----
consumer_brand |> 
  count(brand)

# Transform data ----
consumer_brand_scale <- consumer_brand |> 
  mutate(across(.cols =  perform:rebuy, 
                .fns  = ~ scale(x = .x,
                                center = TRUE,
                                scale  = TRUE)[,1]))

consumer_brand_scale

## Example
(1:10 - mean(x = 1:10)) / sd(x = 1:10)

scale(x = 1:10, 
      center = TRUE, 
      scale = TRUE)[,1]

# Summary statistics ----
consumer_brand_scale |> 
  skim()

# Correlation matrix ----
correlation_matrix <- consumer_brand_scale |> 
  # How to select continuous columns
  select(perform:rebuy) |> 
  correlate(use = "pairwise.complete.obs", 
            method = "pearson",
            diagonal = NA)

correlation_matrix

# How to extract the names of a tibble
names(correlation_matrix)[2:10]

## Visualization data ----
correlation_matrix |> 
  autoplot(triangular = "lower", 
           method = "HC")

# Reducing data complexity ----
brand_mean <- consumer_brand_scale |> 
  group_by(brand) |> 
  summarise(across(.cols = everything(),
                   .fns  = mean))

brand_mean

## Using means and heatmaps ----

### Prepare data ----
brand_mean_longer <- brand_mean |> 
  pivot_longer(cols = perform:rebuy, 
               names_to = "perceptual_adjectives", 
               values_to = "value_mean") |> 
  mutate(brand = fct_reorder(.f = brand, 
                             .x = value_mean),
         perceptual_adjectives = fct_reorder(.f = perceptual_adjectives,
                                             .x = value_mean))

brand_mean_longer

### Visualize data ----
brand_mean_longer |> 
  tidyheatmap(rows = brand,
              columns = perceptual_adjectives,
              values = value_mean, 
              cluster_rows = TRUE,
              cluster_cols = TRUE,
              clustering_method = "complete", 
              display_numbers = TRUE, 
              border_color = "black")


