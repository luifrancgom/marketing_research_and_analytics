# Load libraries ----
library(sweep)
library(tidyverse)
library(scales)
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

# Descriptive statistics by group ----
## Mean, median, sd ----
revenue_stats_by_cat2_frame <- bike_sales |> 
  group_by(category.secondary, frame) |> 
  summarise(
    mean_revenue   = mean(price.ext),
    median_revenue = median(price.ext),
    sd_revenue     = sd(price.ext) 
  ) |> 
  ungroup() |> 
  arrange(desc(mean_revenue))

revenue_stats_by_cat2_frame |> 
  datatable(
    colnames = c(
      "Category secondary",
      "Frame",
      "Mean",
      "Median",
      "Standard deviation"
    )
  ) |> 
  formatRound(
    columns = c(3, 5),
    digits = 2
  )

revenue_stats_by_bikeshop <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(
    mean_revenue   = mean(price.ext),
    median_revenue = median(price.ext),
    sd_revenue     = sd(price.ext) 
  ) |> 
  ungroup() |> 
  arrange(desc(mean_revenue))

revenue_stats_by_bikeshop |> 
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

## Percentage of revenue ----
pct_revenue_by_cat2_frame <- bike_sales |> 
  group_by(category.secondary, frame) |> 
  summarise(
    total_revenue = sum(price.ext), 
  ) |> 
  ungroup() |> 
  mutate(
    pct_revenue = (total_revenue / sum(total_revenue))*100
  ) |> 
  arrange(desc(pct_revenue))

pct_revenue_by_cat2_frame |> 
  datatable(
    colnames =  c(
      "Category secondary",
      "Frame",
      "Total revenue",
      "Percentage total revenue"
    )
  ) |> 
  formatRound(
    columns = 4
  )

pct_revenue_by_bikeshop <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(
    total_revenue = sum(price.ext), 
  ) |> 
  ungroup() |> 
  mutate(
    pct_revenue = (total_revenue / sum(total_revenue))*100
  ) |> 
  arrange(desc(pct_revenue))

pct_revenue_by_bikeshop |> 
  datatable(
    colnames =  c(
      "Bikeshop",
      "Total revenue",
      "Percentage total revenue"
    )
  ) |> 
  formatRound(
    columns = 3
  )
