# Load packages ----
library(tidyverse)
library(skimr)

# Import ----
# "/data/data_sets_marketing/002_data_set_chapter-2.csv"
# "http://goo.gl/UDv12g"
satisfaction_data <- read_csv(
    file = "000_data/002_chapter_2.csv"
)

# Inspect ----
satisfaction_data |> 
    head()

satisfaction_data |>
    glimpse()

# Transform ---
satisfaction_data <- satisfaction_data |> 
    mutate(
        Segment = factor(
            x = Segment,
            ordered = FALSE
        )
    )

# Resumir
satisfaction_data |> 
    skim()
