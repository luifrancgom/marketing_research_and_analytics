# Load packages
library(sweep)
library(tidyverse)
library(skimr)
library(scales)
library(DT)

# Import data ----
bike_sales <- bike_sales |>
    mutate(
        order.id = factor(
            x = order.id,
            ordered = TRUE
        ),
        order.line = factor(
            x = order.line,
            ordered = TRUE
        ),
        customer.id = factor(
            x = customer.id,
            ordered = FALSE
        ),
        product.id = factor(
            x = product.id,
            ordered = FALSE
        )
    )

# Inspect data ----
bike_sales |>
    glimpse()

# Summarize ----
bike_sales |>
    skim()

# Visualization ----
## Correlation between continous variables ----
bike_sales |>
    ggplot() +
    geom_point(
        aes(
            x = price,
            y = quantity,
            color = frame
        )
    ) +
    labs(
        x = "Price (USD Dollars/frame)",
        y = "Quantity (Units)",
        color = "Frame"
    )

bike_sales |>
    ggplot() +
    geom_point(
        aes(
            x = price,
            y = quantity,
            color = frame
        )
    ) +
    scale_x_continuous(
        transform = "log",
        labels = label_currency(accuracy = 1)
    ) +
    scale_y_continuous(
        transform = "log",
        labels = label_currency(accuracy = 1)
    ) +
    labs(
        x = "Price (USD Dollars/frame - Logarithmic scale)",
        y = "Quantity (Units  - Logarithmic scale)",
        color = "Frame"
    )

### Correlation ----
cor_price_quantity <- cor(
    x = bike_sales$price,
    y = bike_sales$quantity
)

cor_price_quantity |>
    format(scientific = FALSE)

cor_log_price_log_quantity <- cor(
    x = log(bike_sales$price),
    y = log(bike_sales$quantity)
)

cor_log_price_log_quantity |>
    format(scientific = FALSE)

## Association between categorical variables ----
bike_sales |>
    ggplot() +
    geom_point(
        aes(
            x = frame,
            y = category.secondary
        ),
        position = position_jitter(
            width = 0.2,
            height = 0.2
        )
    ) +
    labs(
        x = "Frame",
        y = "Category secondary"
    )

bike_sales |>
    count(
        frame,
        category.secondary
    ) |>
    ggplot() +
    geom_text(
        aes(
            x = frame,
            y = category.secondary,
            label = n
        )
    )

# Descriptive statistics by group ----
## Category secondary and frame statistics ----
mean_median_sd_revenue_by_cat2_frame <- bike_sales |>
    group_by(
        category.secondary,
        frame
    ) |>
    summarise(
        mean_revenue = mean(price.ext),
        median_revenue = median(price.ext),
        sd_revenue = sd(price.ext)
    ) |>
    ungroup()

mean_median_sd_revenue_by_cat2_frame |>
    datatable(
        colnames = c(
            "Category secondary",
            "Frames",
            "Mean",
            "Median",
            "Standard deviation"
        )
    ) |>
    formatRound(
        columns = c(
            3,
            5
        ),
        digits = 2
    )

mean_median_sd_revenue_by_bikeshop <- bike_sales |>
    group_by(
        bikeshop.name
    ) |>
    summarise(
        mean_revenue = mean(price.ext),
        median_revenue = median(price.ext),
        sd = sd(price.ext)
    )

mean_median_sd_revenue_by_bikeshop |>
    datatable(
        colnames = c(
            "Bikeshop",
            "Mean",
            "Median",
            "Standard deviation"
        )
    ) |>
    formatRound(
        columns = c(2, 4),
        digits = 2
    )

## Category secondary and frame pct revenue ----
pct_revenue_by_cat2_frame <- bike_sales |>
    group_by(
        category.secondary,
        frame
    ) |>
    summarise(
        total_revenue = sum(price.ext)
    ) |>
    ungroup() |>
    mutate(
        total_revenue_pct = (total_revenue / sum(total_revenue)) * 100
    ) |>
    arrange(
        desc(total_revenue_pct)
    )

pct_revenue_by_cat2_frame |>
    datatable(
        colnames = c(
            "Category secondary",
            "Frame",
            "Total revenue",
            "Percentage"
        )
    ) |>
    formatRound(
        columns = 4,
        digits = 2
    )

pct_revenue_by_bikeshop <- bike_sales |>
    group_by(
        bikeshop.name
    ) |>
    summarise(
        total_revenue = sum(price.ext)
    ) |>
    mutate(
        pct_revenue = (total_revenue / sum(total_revenue)) * 100
    ) |>
    arrange(
        desc(pct_revenue)
    )

pct_revenue_by_bikeshop |>
    datatable(
        colnames = c(
            "Bikeshop",
            "Total revenue",
            "Percentage"
        )
    ) |>
    formatRound(
        columns = 3,
        digits = 2
    )
