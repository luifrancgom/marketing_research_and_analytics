# Libraries ----
library(tidyverse)
library(skimr)

# Import data -----
# segmentation <- read_csv(file = "http://goo.gl/qw303p") |>
#   select(-Segment) 
segmentation <- read_csv(file = "000_data/005_data_chapter5.csv") |> 
  select(-Segment)
