# Load packages
library(sweep)
library(tidyverse)
library(skimr)
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
