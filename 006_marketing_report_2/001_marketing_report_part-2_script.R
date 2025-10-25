# Libraries ----
library(tidyverse)
library(sweep)
library(skimr)
library(scales)

# Import ----
bike_sales <- bike_sales |> 
  mutate(order.id = factor(x = order.id, 
                           ordered = TRUE),
         order.line = factor(x = order.line,
                             ordered = TRUE),
         customer.id = factor(x = customer.id,
                              ordered = FALSE),
         bikeshop.name = factor(x = bikeshop.name,
                                ordered = FALSE),
         bikeshop.city = factor(x = bikeshop.city,
                                ordered = FALSE),
         bikeshop.state = factor(x = bikeshop.name,
                                 ordered = FALSE),
         product.id = factor(x = product.id,
                             ordered = FALSE),
         model = factor(x = model,
                        ordered = FALSE),
         category.primary = factor(category.primary,
                                   ordered = FALSE),
         category.secondary = factor(x = category.secondary,
                                     ordered = FALSE),
         frame = factor(x = frame,
                        ordered = FALSE))

# Summarize ----
bike_sales |> 
  skim()

# Relation continuous var ----
# Colors: hex codes
# https://htmlcolorcodes.com/
# https://www.color-hex.com/
bike_sales |> 
  ggplot() + 
  geom_point(aes(x = price,
                 y = quantity,
                 color = frame)) +
  scale_color_manual(values = c("#043074",
                                "#FDC600")) +
  labs(x = "Price (US Dollars / 1 frame)",
       y = "Quantity",
       color = "Frame",
       subtitle = "Relationship between quantity and price")

## Correlation ----
cor_price_quantity <- cor(x = bike_sales$price,
    y = bike_sales$quantity) |> 
  number(accuracy = 0.000001)

cor_price_quantity

bike_sales |> 
  ggplot() +
  geom_point(aes(x = price,
                 y = quantity,
                 color = frame)) +
  scale_x_continuous(transform = "log10") +
  scale_y_continuous(transform = "log10") +
  scale_color_manual(values = c("#043074",
                                "#FDC600")) +
  labs(x = "Price (US Dollars / 1 frame)",
       y = "Quantity",
       color = "Frame",
       subtitle = "Relationship between quantity and price using a logarithmic scale in base 10")

log_10_price <- log(x = bike_sales$price,
                    base = 10)
log_10_quantity <- log(x = bike_sales$quantity,
                       base = 10) 
cor_log_10_price_quantity <- cor(x = log_10_price,
                                 y = log_10_quantity) |> 
  number(accuracy = 0.000001)

# Relation categorical var ----
## Scatterplot ----
bike_sales |> 
  ggplot() +
  geom_point(aes(x = frame,
                 y = category.secondary),
             position = position_jitter(width = 0.2,
                                        height = 0.2)) +
  labs(x = "Frame",
       y = "Category Secondary",
       subtitle = "Frame vs Category secondary")

## Text ----
bike_sales |> 
  count(frame, category.secondary) |> 
  ggplot() +
  geom_label(aes(x = frame,
                 y = category.secondary,
                 label = n)) +
  labs(x = "Frame",
       y = "Category secondary",
       subtitle = "Frame vs Category secondary")


