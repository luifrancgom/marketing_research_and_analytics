# Libraries ----
library(tidyverse)
library(skimr)

# Import ----
satisfaction_data <- read_csv(file = "000_data/002_chapter_2.csv")
# satisfaction_data <- read_csv(file = "http://goo.gl/UDv12g")

satisfaction_data

# Inspect data ----
satisfaction_data |> 
  head(n = 5)

satisfaction_data |> 
  glimpse()

# Transform
satisfaction_data <- satisfaction_data |> 
  mutate(Segment = factor(x = Segment, 
                          ordered = FALSE))
  
# Summarize
satisfaction_data |> 
  skim()

# Creating variables ----
## Generating a vector of numbers
6:17
1:500
nrow(satisfaction_data)
1:nrow(satisfaction_data)

satisfaction_data <- satisfaction_data |> 
  mutate(customer = 1:nrow(satisfaction_data))

# Filtering
satisfaction_data |> 
  filter(customer == 100)

satisfaction_data |> 
  filter(iProdSAT >= 2)

