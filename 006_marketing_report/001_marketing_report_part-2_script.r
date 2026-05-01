# Load libraries ----
library(sweep)
library(tidyverse)
library(scales)

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

# Relation between continuous variables ----
## Price vs quantity by frame ----
bike_sales |> 
  ggplot() +
  geom_point(
    aes(x = price, y = quantity,
        color = frame)
  ) +
  scale_x_continuous(
    labels = label_currency()
  ) +
  labs(
    x = "Price (US Dollars/Frame)",
    y = "Quantity (Units)",
    color = "Frame type"
  )

cor_price_quantity <- cor(x = bike_sales$price, y = bike_sales$quantity) |> 
  format(scientific = FALSE)

## Price vs quantity transformed by frame
bike_sales |> 
  ggplot() +
  geom_point(
    aes(x = price, y = quantity,
        color = frame)
  ) +
  scale_x_continuous(
    labels = label_currency(accuracy = 1),
    transform = "log10"
  ) +
  scale_y_continuous(
    transform = "log10"
  ) +
  labs(
    x = "Price (US Dollars/Frame)",
    y = "Quantity (Units)",
    color = "Frame type"
  )

cor_price_quantity_trans <- cor(
  x = log(bike_sales$price, base = 10), 
  y = log(bike_sales$quantity, base = 10)) |> 
  format(scientific = FALSE)

## Frame vs Category Secondary ----
bike_sales |> 
  ggplot() +
  geom_point(
    aes(x = frame, y = category.secondary),
    position = position_jitter(
      width = 0.2, height = 0.2)
  ) +
  labs(
    x = "Frame type",
    y = "Category Secondary"
  )

frame_category_secondary_count <- bike_sales |> 
  count(frame, category.secondary)

frame_category_secondary_count |> 
  ggplot() +
  geom_label(
    aes(
      x = frame,
      y = category.secondary,
      label = n
    )
  ) +
  labs(
    x = "Frame type",
    y = "Category Secondary"
  )
