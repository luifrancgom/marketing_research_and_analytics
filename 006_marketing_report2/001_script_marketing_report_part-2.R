# Libraries ----
library(sweep)
library(tidyverse)
library(scales)

# Import data ----
bike_sales <- bike_sales

# Inspect data ----
bike_sales |> glimpse()

# Transform data ----
bike_sales <- bike_sales |> 
  mutate(order.id = factor(x = order.id, 
                           ordered = FALSE),
         order.line = factor(x = order.id, 
                             ordered = TRUE),
         customer.id = factor(x = customer.id, 
                              ordered = FALSE),
         bikeshop.name = factor(x = bikeshop.name, 
                                ordered = FALSE),
         bikeshop.city = factor(x = bikeshop.city, 
                                ordered = FALSE),
         product.id = factor(x = product.id, 
                                ordered = FALSE),
         category.primary = factor(x = category.primary , 
                                   ordered = FALSE),
         category.secondary = factor(x = category.secondary, 
                                     ordered = FALSE),
         frame = factor(x = frame, 
                        ordered = FALSE))

# Data visualization ----
## Price vs quantity ----
bike_sales_p_q_frame <- bike_sales |> 
  select(price, quantity, frame)

### Without transformation ----
bike_sales_p_q_frame |> 
  ggplot() + 
  geom_point(aes(x = price, y = quantity,
                 color = frame)) +
  scale_x_continuous(labels = label_dollar()) +
  labs(x = "Price (US Dollars)",
       y = "Quantity (units)",
       color = "Frame",
       title = "Relation between price and units sale")

### Experiment ----  
# Note: checking if by facetting we ge something new
## Definitely no!!!
bike_sales_p_q_frame_cat2 <- bike_sales |> 
  select(price, quantity, frame, category.secondary)

bike_sales_p_q_frame_cat2 |> 
  ggplot() + 
  geom_point(aes(x = price, y = quantity,
                 color = frame)) +
  scale_x_continuous(labels = label_dollar()) +
  facet_wrap(facets = vars(category.secondary)) +
  labs(x = "Price (US Dollars)",
       y = "Quantity (units)",
       color = "Frame",
       title = "Relation between price and units sale")

### With logarithmic transformation
bike_sales_p_q_frame |> 
  ggplot() + 
  geom_point(aes(x = price, y = quantity,
                 color = frame)) +
  scale_x_continuous(labels = label_dollar(), 
                     transform = "log10") +
  scale_y_continuous(transform = "log10") +
  labs(x = "Price (US Dollars) - Logarithmic Base 10 scale",
       y = "Quantity (units)  - Logarithmic Base 10 scale",
       color = "Frame",
       title = "Relation between price and units sale")

## Frame vs Second category ----
bike_sales_frame_cat2 <- bike_sales |> 
  select(frame, category.secondary)

bike_sales_frame_cat2 |> 
  ggplot() +
  geom_point(aes(x = frame, y = category.secondary),
             position = position_jitter(width = 0.2,
                                        height = 0.2)) +
  labs(x = "Frame material",
       y = "Secondary Category",
       title = "Identifying product gaps")

# Descriptive statistics by group ----

  

