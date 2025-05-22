# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(tidyheatmaps)
library(imager)
library(ggbiplot)

# Import ----
# consumer_brand <- read_csv("http://goo.gl/IQl8nc")
consumer_brand <- read_csv(file = "000_data/008_data_chapter8.csv")
consumer_brand

# Inspect ----
consumer_brand |> 
  glimpse()

# Transform data ----
consumer_brand <- consumer_brand |> 
  mutate(across(.cols = where(is.double), 
                .fns = as.integer)) |> 
  mutate(brand = factor(x = brand, 
                        ordered = FALSE))

## Rescale ----
my_data <- 1:9
my_data

mean(my_data)
my_data - mean(my_data)
mean(my_data - mean(my_data))

sd(my_data)
my_data / sd(my_data)
sd(my_data / sd(my_data))

my_data_scale <- (my_data - mean(my_data)) / sd(my_data)
mean(my_data_scale)
sd(my_data_scale)

scale(x = my_data, 
      center = TRUE, 
      scale = TRUE)[,1]

consumer_brand

consumer_brand_scale <- consumer_brand |> 
  mutate(across(.cols = where(is.integer), 
                .fns = \(x) scale(x = x, 
                                  center = TRUE,
                                  scale  = TRUE)[,1]))

### Anonymous functions
my_function <- \(x) x + 2 
my_function(x = 1)

# Summary statistics ----
consumer_brand_scale |> 
  skim()

## How computers compute?
sqrt(3)^2 == 3

## Correlations ----
correlation_matrix <- consumer_brand_scale |> 
  select(-brand) |> 
  corrr:::correlate()

correlation_matrix

correlation_matrix |> 
  autoplot(triangular = "lower")

# Heat maps ----
brand_mean <- consumer_brand_scale |> 
  group_by(brand) |> 
  summarise(across(.cols = where(is.double), 
                   .fns = mean))

brand_mean_longer <- brand_mean |> 
  pivot_longer(cols = where(is.double), 
               names_to = "perceptual_adjective", 
               values_to = "value_mean")

brand_mean_longer

brand_mean_longer |> 
  tidyheatmap(rows = brand, 
              columns = perceptual_adjective, 
              values = value_mean, 
              border_color = "black", 
              cluster_rows = TRUE, 
              cluster_cols = TRUE, 
              clustering_method = "complete", 
              display_numbers = TRUE, 
              fontsize = 12) # Chapter 11
  
# Principal component analysis ----
set.seed(seed = 1234)
consumer_brand_sample <- consumer_brand |> 
  slice_sample(n = 1, by = brand) |> 
  select(brand, perform, leader)

consumer_brand_sample

## Example image ----
boat_gray <- load.image(file = "000_images/008_boat_gray_512_x_512.tiff")
plot(boat_gray)

boat_gray_longer <- boat_gray |> 
  as.data.frame() |> 
  as_tibble()
  
boat_gray_longer |> 
  pivot_wider(id_cols = x, 
              names_from = y, 
              values_from = value)

## Simple example
consumer_brand_sample$leader |> 
  mean()

consumer_brand_sample$leader |> 
  sd()

consumer_brand_sample_matrix <- consumer_brand_sample |> 
  select(-brand) |> 
  as.matrix()

consumer_brand_sample_matrix_pca <- consumer_brand_sample_matrix |> 
  prcomp(center = TRUE,
         scale. = TRUE)

consumer_brand_sample_matrix_pca

consumer_brand_sample_matrix_pca$x
consumer_brand_sample_matrix_pca$rotation

## Visualization ----
consumer_brand_sample_matrix_pca |> 
  ggbiplot(groups = consumer_brand_sample$brand) +
  labs(x = "PC1",
       y = "PC2",
       color = "Brands", 
       title = "Reducing data complexity: consumer brands") 

## All the database
consumer_brand_matrix <- consumer_brand |> 
  select(-brand) |>
  as.matrix()

consumer_brand_matrix_pca <- consumer_brand_matrix |>   
  prcomp(center = TRUE,
         scale. = TRUE)

consumer_brand_matrix_pca

consumer_brand_matrix_pca |> 
  ggbiplot(groups = consumer_brand$brand)
  
  

