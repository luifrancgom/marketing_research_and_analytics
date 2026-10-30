# Load packages ----
library(tidyverse)
library(sweep)
library(skimr)
library(DT)
library(scales)

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

# Inspect ----
bike_sales |>
    glimpse()

# Summarise ---
bike_sales |>
    skim()

# Count ----
bike_sales_tbl <- bike_sales |>
    count(category.secondary)

bike_sales_tbl |>
    datatable(
        colnames = c(
            "Category secondary",
            "N"
        )
    )

bike_sales |>
    count(frame)

bike_sales |>
    count(
        category.secondary,
        frame
    )

bike_sales |>
    count(bikeshop.name)

# Visualization ----
bike_sales |>
    ggplot() +
    geom_histogram(
        aes(x = quantity),
        color = "black"
    ) +
    labs(
        x = "Quantities (units)",
        y = "Frequency"
    )

bike_sales |>
    ggplot() +
    geom_histogram(
        aes(x = price),
        color = "black"
    ) +
    labs(
        x = "Dollars (USD/frame)",
        y = "Frequency"
    )

bike_sales |>
    ggplot() +
    geom_boxplot(
        aes(
            x = price.ext,
            y = category.secondary
        )
    ) +
    scale_x_continuous(
        transform = "log10",
        labels = label_currency()
    ) +
    labs(
        x = "Revenue (US Dollars)",
        y = "Category Secondary"
    )

bike_sales |>
    ggplot() +
    geom_boxplot(
        aes(
            x = price.ext,
            y = frame
        )
    ) +
    scale_x_continuous(
        transform = "log10",
        labels = label_currency()
    ) +
    labs(
        x = "Revenue (US Dollars)",
        y = "Frame"
    )

# Answering questions with data ----
revenue_by_bike_shop <- bike_sales |> 
    group_by(bikeshop.name) |> 
    summarize(total_revenue = sum(price.ext)) |> 
    mutate(bikeshop.name = fct_reorder(
        .f = bikeshop.name,
        .x = total_revenue
    ))

revenue_by_bike_shop |> 
    ggplot() +
    geom_col(
        aes(
            x = total_revenue,
            y = bikeshop.name
        )
    ) +
    scale_x_continuous(
        labels = label_currency()
    ) +
    labs(
        x = "Total revenue (US Dollars)",
        y = "Bike shop"
    )

revenue_by_cat2 <- bike_sales |> 
    group_by(category.secondary) |> 
    summarize(total_revenue = sum(price.ext)) |> 
    mutate(category.secondary = fct_reorder(
        .f = category.secondary,
        .x = total_revenue
    ))

revenue_by_cat2 |> 
    ggplot() +
    geom_col(
        aes(
            x = total_revenue,
            y = category.secondary
        )
    ) +
    scale_x_continuous(
        labels = label_currency()
    ) +
    labs(
        x = "Total revenue (US Dollars)",
        y = "Category Secondary"
    )
