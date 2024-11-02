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
## Mean, median, sd by frame and secondary category ----
bike_sales_frame_cat2_revenue <- bike_sales |> 
  select(frame, category.secondary, price.ext)

mean_median_sd <- bike_sales_frame_cat2_revenue |> 
  group_by(frame, category.secondary) |> 
  summarise(mean_revenue = mean(price.ext),
            median_revenue = median(price.ext),
            sd_revenue = sd(price.ext)) |> 
  ungroup() |> 
  arrange(desc(median_revenue))

mean_median_sd

## Percentage of revenue by frame and secondary category ----
pct_revenue <- bike_sales_frame_cat2_revenue |> 
  group_by(frame, category.secondary) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  ungroup() |> 
  mutate(pct_total_revenue = (total_revenue / sum(total_revenue))*100) |> 
  arrange(desc(pct_total_revenue))
  
pct_revenue

## Mean, median, sd by bike shop ----  
bike_sales_bikeshop_revenue <- bike_sales |> 
  select(bikeshop.name, price.ext)

mean_median_sd_bikeshop <- bike_sales_bikeshop_revenue |> 
  group_by(bikeshop.name) |> 
  summarise(mean_revenue = mean(price.ext),
            median_revenue = median(price.ext),
            sd_revenue = sd(price.ext)) |> 
  ungroup() |> 
  arrange(desc(median_revenue))

mean_median_sd_bikeshop

## Percentage of revenue by bike shop ----
pct_revenue_bikeshop <- bike_sales_bikeshop_revenue |> 
  group_by(bikeshop.name) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  ungroup() |> 
  mutate(pct_total_revenue = (total_revenue / sum(total_revenue))*100) |> 
  arrange(desc(pct_total_revenue))

pct_revenue_bikeshop


  

