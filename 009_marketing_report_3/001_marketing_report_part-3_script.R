# Load packages ----
library(sweep)
library(tidyverse)
library(scales)
library(tidymodels)
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

# Count data ----
bike_sales |>
    count(
        category.secondary
    )

bike_sales |>
    count(
        bikeshop.state
    )

# Product gap ----
## Prepare data ----
total_sales_by_model_cat2_frame <- bike_sales |>
    select(
        model,
        category.secondary,
        frame,
        price.ext
    ) |>
    group_by(
        model,
        category.secondary,
        frame
    ) |>
    summarise(
        total_revenue = sum(price.ext)
    ) |>
    ungroup() |>
    arrange(
        desc(total_revenue)
    ) |>
    mutate(
        category.secondary = fct_reorder(
            .f = category.secondary,
            .x = total_revenue
        )
    )

total_sales_by_model_cat2_frame

## Visualization ----
total_sales_by_model_cat2_frame |>
    ggplot() +
    geom_point(
        aes(
            x = frame,
            y = total_revenue
        ),
        position = position_jitter(
            width = 0.2,
            height = 0.2
        )
    ) +
    facet_wrap(
        facets = vars(category.secondary)
    ) +
    scale_y_continuous(
        labels = label_currency()
    ) +
    labs(
        x = "Frame",
        y = "Total revenue (US Dollars)"
    )

# Price new product ----
## Fit model
model_1 <- lm(
    data = bike_sales,
    formula = price ~ category.secondary
)

model_1 |>
    tidy()

### Model 2 ----
model_2 <- lm(
    data = bike_sales,
    formula = price ~ category.secondary + frame
)

model_2 |>
    tidy() |>
    datatable(
        colnames = c(
            "Term",
            "Estimate",
            "Standard Error",
            "Statistic",
            "P-value"
        )
    ) |>
    formatRound(
        columns = 2:5,
        digits = 3
    )

model_3 <- lm(
    data = bike_sales,
    formula = price ~ category.secondary + frame + category.secondary:frame
)

model_3 |>
    tidy() |>
    select(
        term,
        estimate
    )

model_4 <- lm(
    data = bike_sales,
    formula = price ~ category.secondary + frame + bikeshop.state
)

model_4 |>
    tidy() |>
    select(
        term,
        estimate,
        p.value
    ) |>
    View()

## Predict ----
new_data <- tibble(
    category.secondary = c(
        "Cyclocross",
        "Fat Bike",
        "Over Mountain",
        "Sport",
        "Triathalon"
    ),
    frame = c(
        "Aluminum",
        "Carbon",
        "Aluminum",
        "Carbon",
        "Aluminum"
    )
)

predict(
    object = model_2,
    newdata = new_data
) |> 
  enframe(
    name = "observation",
    value = "pred_price"
  ) |> 
    bind_cols(
        new_data
    ) |> 
    select(
        category.secondary,
        frame,
        pred_price
    ) |> 
    datatable(
        colnames = c(
            "Category secondary",
            "Frame",
            "Predict price"
        )
    ) |> 
    formatRound(
        columns = 3,
        digits = 0
    )

