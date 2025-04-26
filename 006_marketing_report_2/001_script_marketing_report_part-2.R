# Libraries ----
library(tidyverse)
library(sweep)

# Import data ----
bike_sales <- bike_sales

# Inspect data ----
bike_sales |> 
  glimpse()

# Transform data ----
bike_sales <- bike_sales |> 
  mutate(order.id = factor(x = order.id, 
                           ordered = FALSE),
         order.line = factor(x = order.line, 
                             ordered = TRUE),
         customer.id = factor(x = customer.id, 
                              ordered = FALSE),
         bikeshop.name = factor(x = bikeshop.name, 
                                ordered = FALSE),
         bikeshop.city = factor(x = bikeshop.city, 
                                ordered = FALSE),
         bikeshop.state = factor(x = bikeshop.state, 
                                 ordered = FALSE),
         product.id = factor(x = product.id, 
                             ordered = FALSE),
         model = factor(x = model, 
                       ordered = FALSE),
         category.primary = factor(x = category.primary, 
                                   ordered = FALSE),
         category.secondary = factor(x = category.secondary, 
                                     ordered = FALSE),
         frame = factor(x = frame, 
                        ordered = FALSE))

# Relation between continuous variables ----
## price vs quantity (color by frame)
bike_sales |> 
  ggplot() +
  geom_point(aes(x = quantity, 
                 y = price,
                 color = frame)) +
  labs(x = "Quantity",
       y = "Price (per unit in US Dollars)",
       color = "Frame",
       title = "Quantities sold vs price of frames")

## price vs quantity (color by frame) transformed
bike_sales |> 
  ggplot() +
  geom_point(aes(x = quantity, 
                 y = price,
                 color = frame)) +
  scale_x_continuous(transform = "log") +
  scale_y_continuous(transform = "log") +
  labs(x = "Quantity",
       y = "Price (per unit in US Dollars)",
       color = "Frame",
       title = "Quantities sold vs price of frames")

## Summary statistics ----
corr_price_quantity <- bike_sales |> 
  summarise(corr_price_quantity = cor(quantity, price))

# Product gap ----
bike_sales |> 
  ggplot() + 
  geom_point(aes(x = frame, y = category.secondary),
             position = position_jitter(width = 0.2,
                                        height = 0.2))

bike_sales |> 
  count(frame, category.secondary) |> 
  ggplot() +
  geom_label(aes(x = frame,
                 y = category.secondary,
                 label = n))
  

