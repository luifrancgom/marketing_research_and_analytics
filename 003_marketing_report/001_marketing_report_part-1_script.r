# Load libraries ----
library(tidyverse)
library(sweep)
library(skimr)
library(DT)

# Import data ----
bike_sales <- bike_sales |> 
  mutate(
    bikeshop.name      = factor(x = bikeshop.name, ordered = FALSE),
    bikeshop.city      = factor(x = bikeshop.city, ordered = FALSE),
    bikeshop.state     = factor(x = bikeshop.state, ordered = FALSE),
    model              = factor(x = model, ordered = FALSE),
    category.primary   = factor(x = category.primary, ordered = FALSE),
    category.secondary = factor(x = category.secondary, ordered = FALSE),
    frame              = factor(x = frame, ordered = FALSE),
    order.id           = factor(x = order.id, ordered = FALSE),
    order.line         = factor(x = order.line, ordered = TRUE),
    customer.id        = factor(x = customer.id, ordered = FALSE),
    product.id         = factor(x = product.id, ordered = FALSE)
  )

# Inspect data ----
bike_sales |> 
  glimpse()

# Summary statistics ----

## Descriptive statistics ----
bike_sales |> 
  skim()

## Counting ----
category_secondary_count <- bike_sales |> 
  count(category.secondary)

category_secondary_count |> 
  datatable(
    colnames = c(
      "Category Secondary",
      "Count"
    )
  )

frame_count <- bike_sales |> 
  count(frame)

category_secondary_frame_count <- bike_sales |> 
  count(
    category.secondary,
    frame
  )

# New products
# Cyclocross Aluminum
# Fat Bike Carbon
# Over Mountain Aluminum
# Sport Carbon
# Triathalon Aluminum

bikeshop_name_count <- bike_sales |> 
  count(bikeshop.name)

bikeshop_name_count
