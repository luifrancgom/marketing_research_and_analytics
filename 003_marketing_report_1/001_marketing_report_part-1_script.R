# Load packages ----
library(tidyverse)
library(sweep)
library(skimr)
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
