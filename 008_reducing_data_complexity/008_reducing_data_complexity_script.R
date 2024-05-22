# Libraries ----
# library(imager)
library(tidyverse)


# Import data ----
# consumer_brand <- read_csv("http://goo.gl/IQl8nc")
consumer_brand <- read_csv("000_data/008_data_chapter8.csv")

# Explore data ----
consumer_brand |> glimpse()

# Transform data ----


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