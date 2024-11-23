# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(tidyheatmaps)
library(tidymodels)
library(ggbiplot)

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

# Principal component analysis ----
## A small example ----
set.seed(seed = 1234)
consumer_brand_sample <- consumer_brand |> 
  slice_sample(n = 1, by = brand) |> 
  select(brand, perform, leader)

consumer_brand_sample

consumer_brand_sample |> 
  summarise(mean_leader  = mean(leader),
            sdv_leader   = sd(leader),
            mean_perform = mean(perform),
            sdv_perform  = sd(perform))

## An image as an example ----
library(imager)

boat_grey <- load.image(file = "000_images/008_boat_gray_512_x_512.tiff")

boat_grey_tbl <- boat_grey |> 
  as.data.frame() |> 
  as_tibble()

boat_grey_tbl_wide <- boat_grey_tbl |> 
  pivot_wider(id_cols = x, 
              names_from = y, 
              values_from = value)
  
boat_grey_tbl_wide

## Applying PCA small sample ----
consumer_brand_sample_matrix <- consumer_brand_sample |> 
  select(-brand) |> 
  as.matrix()

consumer_brand_sample_pca <- consumer_brand_sample_matrix |> 
  prcomp(center = TRUE, 
         scale. = TRUE)

consumer_brand_sample_pca


scores <- consumer_brand_sample_pca$x

scores_tbl <- scores |> 
  as_tibble()

scores_tbl$PC1  

### Measuring how god is the approximation ----
eingevalues <- consumer_brand_sample_pca |> 
  tidy(matrix = "eigenvalues")

eingevalues |> 
  mutate(variance = std.dev^2,
         .after = std.dev)
  
## Visualizing pca ----
ggplot_pca <- consumer_brand_sample_pca |> 
  ggscreeplot() + 
  scale_x_continuous(breaks = 1:2)

ggplot_pca

ggbiplot(pcobj = consumer_brand_sample_pca,
         groups = consumer_brand_sample$brand, 
         scale = 1, 
         pc.biplot = FALSE)

## Applying PCA full sample ----
consumer_brand_pca <- consumer_brand |> 
  select(-brand) |> 
  prcomp(center = TRUE, 
         scale. = TRUE)
  
consumer_brand_pca |> 
  ggbiplot(groups = consumer_brand$brand,
           scale = 1,
           pc.biplot = FALSE)

