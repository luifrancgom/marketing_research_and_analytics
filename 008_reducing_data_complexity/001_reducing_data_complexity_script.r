# Load libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(tidyheatmaps)
library(tidymodels)
library(ggbiplot)

# Import data ----
# "/data/data_sets_marketing/008_data_set_chapter-8.csv"
# "http://goo.gl/IQl8nc"
consumer_brand <- read_csv(
  file = "000_data/008_data_set_chapter-8.csv"
)

# Filter data ----
consumer_brand |> 
  filter(brand == "a")

# Inspect data ----
consumer_brand |> 
  glimpse()

# Count data ----
consumer_brand |> 
  count(brand)

# Transform ----
## Scale ----
x <- 1:6
x1 <- (1:6)*10
y <- c(30, -1, 10, 20, 50, 21)

mean_x <- mean(x)
sd_x   <- sd(x)

mean_y <- mean(y)
sd_y   <- sd(y)

(y - mean_y) / sd_y


# Media de los datos sea 0
# Desviación estandar sea 1
(x - mean_x) / sd_x

consumer_brand_scale <- consumer_brand |>
  mutate(
    across(
      .cols = perform:rebuy,
      .fns  = \(x) scale(x = x, center = TRUE, scale = TRUE)[,1]
    )
  )

consumer_brand_scale

?scale
scale(
  x = consumer_brand$perform,
  center = TRUE,
  scale  = TRUE
)[,1]

### Anonymous functions ----
# f(x) = x^2
func_sum <- \(x) sum(x) 

func_sum(y)

# Summarize ----
consumer_brand_scale |> 
  skim()

# Correlation ----
correlation_matrix <- consumer_brand_scale |> 
  select(-brand) |> 
  correlate(
    use = "pairwise.complete.obs",
    method = "pearson"
  )

correlation_matrix |> 
  autoplot(
    method = "HC", # hierarchical clustering
    triangular = "lower"
  )

# Heatmaps ----
brand_mean <- consumer_brand_scale |> 
  group_by(brand) |> 
  summarize(
    across(
      .cols = perform:rebuy,
      .fns  = mean
    )
  ) |> 
  ungroup()

brand_mean_longer <- brand_mean |> 
  pivot_longer(
    cols = perform:rebuy, 
    names_to = "perceptual_adjectives",
    values_to = "value_mean"
  )

brand_mean_longer

tidyheatmap(
  df = brand_mean_longer,
  rows = brand,
  columns = perceptual_adjectives,
  values = value_mean, 
  border_color = "black",
  display_numbers = TRUE,
  fontsize = 12,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  clustering_method = "complete"
)

# Principal component analysis ----
## Reduced example ----
set.seed(seed = 1234)
consumer_brand_sample <- consumer_brand |> 
  slice_sample(
    n = 1,
    by = brand
  ) |> 
  select(
    brand, perform, leader
  )

consumer_brand_sample

## Example with images ----
boat <- imager::load.image(
  file = "000_images/008_boat_gray_512_x_512.tiff" 
)

plot(boat)

boat_gray <- boat |> 
  as.data.frame() |> 
  as_tibble()

boat_gray_wider <- boat_gray |> 
  pivot_wider(
    id_cols = x,
    names_from = y,
    values_from = value
  )

consumer_brand_sample_tbl <- consumer_brand_sample |> 
  select(-brand) 

consumer_brand_sample_tbl_pca <- consumer_brand_sample_tbl |> 
  prcomp(
    center = TRUE,
    scale. = TRUE
  )

consumer_brand_sample_tbl_pca

consumer_brand_sample_tbl_pca$center
consumer_brand_sample_tbl_pca$scale
consumer_brand_sample_tbl_pca$x
consumer_brand_sample_tbl_pca$rotation

consumer_brand_sample_tbl_pca |> 
  tidy(matrix = "eigenvalues") |> 
  mutate(variance = std.dev^2, .after = std.dev) |> 
  mutate(percent2 = variance / sum(variance), .after = percent)

consumer_brand_sample_tbl_pca |> 
  ggscreeplot() +
  scale_x_continuous(breaks = 1:2)

consumer_brand_sample_tbl_pca |> 
  ggbiplot(
    groups = consumer_brand_sample$brand,
    scale = 1,
    pc.biplot = TRUE
  )

consumer_brand_pca <- consumer_brand |> 
  select(-brand) |> 
  prcomp(
    center = TRUE,
    scale. = TRUE
  )

consumer_brand_pca$x

consumer_brand_pca |> 
  tidy(matrix = "eigenvalues") 

consumer_brand_pca |> 
  ggscreeplot() +
  scale_x_continuous(breaks = 1:9)

consumer_brand_pca |> 
  ggbiplot(
    groups = consumer_brand$brand,
    scale = 1,
    pc.biplot = TRUE
  )
