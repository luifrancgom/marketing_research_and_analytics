# Load packages ----
library(tidyverse)
library(skimr)

# Import data ----
weekly_store <- read_csv(
    file = "000_data/003_chapter_3.csv"
)

# Inspect ----
weekly_store |>
    glimpse()

# Transform ----
weekly_store <- weekly_store |>
    mutate(
        storeNum = factor(storeNum, ordered = FALSE),
        Year = factor(Year, levels = 1:2, ordered = TRUE),
        Week = factor(Week, levels = 1:52, ordered = TRUE),
        p1prom = as.logical(p1prom),
        p2prom = as.logical(p2prom)
    )

# Summarise
weekly_store |>
    skim()

# Count ---
weekly_store |>
    count(country)

# Visualization ----
## First identify the variables of interest
## p2sales
## country
weekly_store_sales_by_country <- weekly_store |>
    group_by(country) |>
    summarise(sum_p2sales = sum(p2sales)) |>
    mutate(
        country = fct_reorder(
            .f = country,
            .x = sum_p2sales
        )
    )

weekly_store_sales_by_country

weekly_store_sales_by_country |>
    ggplot() +
    geom_col(
        aes(
            x = country,
            y = sum_p2sales
        ),
        color = "black"
    ) +
    scale_x_discrete(
        labels = c(
            "Australia",
            "China",
            "Brazil",
            "United States",
            "Britain",
            "Japan",
            "Germany"
        )
    ) +
    labs(
        x = NULL,
        y = "Sales (units)"
    )
