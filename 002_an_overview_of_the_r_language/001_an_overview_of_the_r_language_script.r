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
        ),
        customer = 1:nrow(satisfaction_data)
    )

# Select ----
satisfaction_data |>
    select(
        customer,
        iProdSAT
    )

# Filter ----
satisfaction_data |>
    filter(
        iProdSAT >= 5
    )

# >, <, ==, >=, <=

# Groups ----
satisfaction_data |>
    group_by(Segment) |>
    select(
        iProdSAT,
        iSalesSAT
    ) |>
    summarise(
        mean_iProSAT = mean(iProdSAT),
        median_iSalesSAT = median(iSalesSAT)
    )

# Descriptive statistics ----
satisfaction_data |>
    summarise(
        mean_iProSAT = mean(iProdSAT),
        median_iSalesSAT = median(iSalesSAT)
    )

# Resume ----
satisfaction_data |>
    skim()
