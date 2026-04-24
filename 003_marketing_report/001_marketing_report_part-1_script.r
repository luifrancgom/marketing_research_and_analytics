# Load libraries ----
library(tidyverse)
library(sweep)
library(skimr)
library(DT)
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

# Data visualization ----
## Histogram ----
bike_sales |> 
  count(quantity)

bike_sales |> 
  ggplot() +
  geom_histogram(
    aes(x = quantity),
    color = "black"
  ) + 
  labs(
    x = "Sales (units)",
    y = "Frequency",
    subtitle = "Distribution of sales in units of frames"
  )

bike_sales |> 
  ggplot() +
  geom_histogram(
    aes(x = price),
    color = "black"
  ) +
  scale_x_continuous(
    labels = label_currency()
  ) +
  labs(
    x = "Frame prices (US Dollars)",
    y = "Frequency",
    subtitle = "Distribution of frame prices"
  )

## Boxplots ----
bike_sales |> 
  ggplot() +
  geom_boxplot(
    aes(x = price.ext, y = category.secondary)
  ) +
  scale_x_continuous(
    labels = label_currency(),
    transform = "log10"
  ) +
  labs(
    x = "Revenue (US Dollars)",
    y = "Category Secondary",
    subtitle = "Distribution of revenue by category secondary"
  )

bike_sales |> 
  ggplot() +
  geom_boxplot(
    aes(x = price.ext, y = frame)
  ) +
  scale_x_continuous(
    label = label_currency(),
    transform = "log10"
  ) +
  labs(
    x = "Revenue (US Dollars)",
    y = "Frame",
    subtitle = "Distribution of revenue by frame"
  )

# Answering questions with data ----

# What are the bike shops, bikeshop.name, 
# that generate more revenue, price.ext, to 
# the corporation?

total_revenue_by_bikeshops <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  mutate(bikeshop.name = fct_reorder(
    .f = bikeshop.name, .x = total_revenue
  )
)

total_revenue_by_bikeshops |> 
  ggplot() +
  geom_col(
    aes(x = total_revenue, y = bikeshop.name),
    color = "black"
  ) +
  scale_x_continuous(
    label = label_currency()
  ) +
  labs(
    x = "Total revenue (US Dollars)",
    y = "Bike Shop",
    subtitle = "Revenue by bike shop"
  )


# What are the specific bicycle categories, 
# category.secondary, that generate more revenue, 
# price.ext, to the corporation?
total_revenue_by_category_secondary <- bike_sales |> 
  group_by(category.secondary) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  mutate(category.secondary = fct_reorder(
    .f = category.secondary, .x = total_revenue
  )
)

total_revenue_by_category_secondary |> 
  ggplot() +
  geom_col(
    aes(x = total_revenue, y = category.secondary),
    color = "black"
  ) +
  scale_x_continuous(
    label = label_currency()
  ) +
  labs(
    x = "Total revenue (US Dollars)",
    y = "Category secondary",
    subtitle = "Revenue by category secondary"
  )
