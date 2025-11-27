# Libraries ----
library(tidyverse)
library(skimr)

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
  
