# Load packages ----
library(tidyverse)
library(skimr)
library(DT)
library(scales)

# Import ----
segmentation <- read_csv(
    file = "000_data/005_chapter_5.csv"
)

# Inspect ----
segmentation |>
    glimpse()

# Transform ----
segmentation <- segmentation |>
    mutate(
        gender = factor(
            x = gender,
            ordered = FALSE
        ),
        kids = as.integer(kids),
        ownHome = factor(
            x = ownHome,
            ordered = FALSE
        ),
        subscribe = factor(
            x = subscribe,
            ordered = FALSE
        ),
        Segment = factor(
            x = Segment,
            ordered = FALSE
        )
    )

# Summarize ----
segmentation |>
    skim()

# Filter ----
segmentation |>
    filter(
        income < 0
    )

# Descriptive statistics by group ---
segmentation |>
    group_by(
        Segment,
        ownHome
    ) |>
    summarise(
        mean_income = mean(income)
    ) |>
    ungroup()

sum_kids_table <- segmentation |>
    group_by(
        Segment,
        ownHome
    ) |>
    summarise(
        sum_kids = sum(kids)
    ) |>
    ungroup()

sum_kids_table |>
    datatable(
        colnames = c(
            "Segment",
            "Ownership home",
            "Total kids"
        )
    )

subscriber_by_segment_home_ownership <- segmentation |>
    count(
        subscribe,
        Segment,
        ownHome
    ) |>
    group_by(
        Segment,
        ownHome
    ) |>
    mutate(
        n_pct = (n / sum(n)) * 100
    ) |>
    arrange(Segment, ownHome) |> 
    ungroup()

subscriber_by_segment_home_ownership |>
    datatable(
        colnames = c(
            "Status subscription",
            "Segment",
            "Ownership home",
            "Number",
            "Percentage"
        )
    ) |>
    formatRound(
        columns = 5,
        digits = 2
    )

## Visualization ---
subscriber_by_segment_home_ownership |> 
    ggplot() + 
    geom_col(
        aes(
            x = subscribe,
            y = n_pct
        )
    ) +
    facet_wrap(
        facets = vars(
            Segment,
            ownHome
        ), 
        nrow = 2 
    ) +
    labs(
        x = "Subscription status",
        y = "Percentage (%)"
    )

# Visualization by groups using a continous variable ----
segmentation |> 
    ggplot() +
    geom_boxplot(
        aes(
            x = income,
            y = Segment
        )
    ) +
    facet_wrap(
        facets = vars(ownHome)
    ) +
    scale_x_continuous(
        labels = label_currency()
    ) +
    labs(
        x = "Yearly Disposable income (USD Dollars)"
    )
