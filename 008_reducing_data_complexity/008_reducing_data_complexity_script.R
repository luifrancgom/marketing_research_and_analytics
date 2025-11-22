# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(tidyheatmaps)
# If you want to use it
# install.packages("imager")
library(imager)
library(tidymodels)
library(ggbiplot)

# Import ----
# consumer_brand <- read_csv(file = "http://goo.gl/IQl8nc")
consumer_brand <- read_csv(file = "000_data/008_chapter_8.csv")

# Inspect ----
consumer_brand |> 
  glimpse()

# Count ----
consumer_brand |> 
  count(brand)

consumer_brand |> 
  count(perform, brand)

# Summarise data ----
consumer_brand |> 
  skim()

# Scale data
?scale

tibble(x = 1:10,
       y = (1:10)*100)

(1:10 - mean(1:10)) / sd(1:10)
((1:10)*100 - mean((1:10)*100)) / sd((1:10)*100)

scale(x = 1:10,
      center = TRUE,
      scale = TRUE)[,1]

scale(x = (1:10)*100,
      center = TRUE,
      scale = TRUE)[,1]

function_add_2 <- \(x) x + 2
function_add_2(x = 100)

consumer_brand_scale <- consumer_brand |> 
  mutate(across(.cols = perform:rebuy, 
                .fns = \(x) scale(x = x,
                                  center = TRUE,
                                  scale = TRUE)[,1]))

# Correlation matrix ----
correlation_matrix <- consumer_brand_scale |> 
  select(perform:rebuy) |> 
  corrr::correlate(
    method = "pearson", 
    use = "pairwise.complete.obs"
    )

correlation_matrix |> 
  autoplot(triangular = "lower", 
           method = "HC")

# Heatmaps ----
brand_mean <- consumer_brand_scale |> 
  group_by(brand) |> 
  summarise(
    across(.cols = perform:rebuy, 
           .fns = mean)
  )

brand_mean_longer <- brand_mean |> 
  pivot_longer(
    cols = perform:rebuy, 
    names_to = "perceptual_adjectives",
    values_to = "value_mean"
  )

brand_mean_longer |> 
  tidyheatmap(
    rows = brand,
    columns = perceptual_adjectives, 
    values = value_mean, 
    display_numbers = TRUE,
    border_color = "black",
    fontsize = 12,
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    clustering_method = "complete"
  )

# Principal component analysis (PCA) ----
## Example ----
set.seed(seed = 1234)
consumer_brand_sample <- consumer_brand |>
  slice_sample(n = 1, by = brand) |> 
  select(brand, perform, leader)

consumer_brand_sample

## Applying PCA ----
boat_gray <- load.image(file = "000_images/008_boat_gray_512_x_512.tiff") |> 
  plot()

boat_gray_long <- boat_gray |> 
  as.data.frame() |> 
  as_tibble()
  
boat_gray_wide <- boat_gray_long |> 
  pivot_wider(
    id_cols = x, 
    names_from = y, 
    values_from = value)
  
## Apply example ----
consumer_brand_sample_matrix <- consumer_brand_sample |> 
  select(-brand) |> 
  as.matrix()

consumer_brand_sample_matrix

consumer_brand_sample_pca <- consumer_brand_sample_matrix |> 
  prcomp(
    center = TRUE, 
    scale. = TRUE
  )

consumer_brand_sample_pca

### Components ----
consumer_brand_sample_pca$x

### Standard deviation ----
eigenvalues <- consumer_brand_sample_pca |> 
  tidy(matrix = "eigenvalues") |> 
  mutate(variance = std.dev^2,
         .after = std.dev)

### Visualization ----
consumer_brand_sample_pca |> 
  ggscreeplot() +
  scale_x_continuous(breaks = 1:2)

consumer_brand_sample_pca |> 
  ggbiplot(
    groups = consumer_brand_sample$brand,
    scale = 1, pc.biplot = FALSE
  )

## Apply database ----
consumer_brand |> 
  glimpse()

consumer_brand_pca <- consumer_brand |>
  select(-brand) |> 
  prcomp(
    center = TRUE,
    scale. = TRUE
  )
  
consumer_brand_pca |> 
  ggbiplot(
    groups = consumer_brand$brand,
    scale = 1, pc.biplot = FALSE
  )
