# Libraries ----
# library(imager)
library(tidyverse)
library(skimr)
library(corrr)
library(tidyheatmaps)

# Import data ----
# consumer_brand <- read_csv("http://goo.gl/IQl8nc")
consumer_brand <- read_csv("000_data/008_data_chapter8.csv")

# Explore data ----
consumer_brand |> glimpse()

# Transform data ----
## Standarizing
1:10
mean(x = 1:10)
sd(x = 1:10)

1:10 - mean(x = 1:10)
(1:10 - mean(x = 1:10)) |> mean()

(1:10 - mean(x = 1:10)) / sd(x = 1:10)

(1000:1010 - mean(x = 1000:1010)) / sd(x = 1000:1010)

scale(x = 1:10, center = TRUE, scale = TRUE)[, 1]

## Not efficient
consumer_brand |> 
  mutate(perform = scale(x = perform, 
                         center = TRUE, 
                         scale = TRUE)[,1],
         leader = scale(x = leader, 
                         center = TRUE, 
                         scale = TRUE)[,1])

# Efficient
consumer_brand_scale <- consumer_brand |> 
  mutate(across(.cols = perform:rebuy,
                .fns = ~ scale(x = .x,
                               center = TRUE,
                               scale = TRUE)[,1]))

consumer_brand_scale

# Summarize data ----
consumer_brand_scale |> skim()

# Calculating correlations ----
correlation_matrix <- consumer_brand_scale |> 
  select(perform:rebuy) |> 
  correlate(method = "pearson",
            use = "pairwise.complete.obs")

correlation_matrix

correlation_matrix |> 
  autoplot(triangular = "lower",
           method = "HC")

# Using the mean
brand_mean <- consumer_brand_scale |> 
  group_by(brand) |> 
  summarise(across(.cols = perform:trendy,
                   .fns = ~ mean(x = .x)))

brand_mean <- consumer_brand_scale |> 
  group_by(brand) |> 
  summarise(across(.cols = perform:trendy,
                   .fns = mean))

brand_mean

brand_mean_long <- brand_mean |> 
  pivot_longer(cols = perform:trendy, 
               names_to = "perceptual_adjective",
               values_to = "value_mean")

tidyheatmap(df = brand_mean_long,
            rows = brand,
            columns = perceptual_adjective,
            values = value_mean,
            border_color = "black",
            display_numbers = TRUE,
            clustering_method = "complete",
            cluster_rows = TRUE, 
            cluster_cols = TRUE)

# Using images to understand data complexity ----

# boat_gray <- imager::load.image(file = "000_images/008_boat_gray_512_x_512.tiff")
# plot(boat_gray)
# 
# boat_gray_long <- boat_gray |> 
#   as.data.frame() |> 
#   as_tibble()
# 
# boat_gray_long
# 
# boat_gray_wider <- boat_gray_long |>
#   pivot_wider(id_cols = x, 
#               names_from = y, 
#               values_from = value)
# 
# boat_gray_wider