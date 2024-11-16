# Libraries -----
library(sweep)
library(tidyverse)
library(skimr)
library(scales)

# Import data -----
bike_sales <- bike_sales |> 
  mutate(order.id           = factor(x = order.id, 
                                     ordered = FALSE),
         order.line         = factor(x = order.id, 
                                     ordered = TRUE),
         customer.id        = factor(x = customer.id, 
                                     ordered = FALSE),
         bikeshop.name      = factor(x = bikeshop.name, 
                                     ordered = FALSE),
         bikeshop.city      = factor(x = bikeshop.city, 
                                ordered = FALSE),
         bikeshop.state     = factor(x = bikeshop.state, 
                                     ordered = FALSE),
         product.id         = factor(x = product.id, 
                                     ordered = FALSE),
         model              = factor(x = model, 
                                     ordered = FALSE),
         category.primary   = factor(x = category.primary , 
                                     ordered = FALSE),
         category.secondary = factor(x = category.secondary, 
                                     ordered = FALSE),
         frame              = factor(x = frame, 
                                     ordered = FALSE))

# Inspect data -----
bike_sales |> glimpse()

# Summarize data ----
bike_sales |> skim()

# Visualization product gap ----
bike_sales |> 
  count(model)

## Prepare data ----
total_sales_by_cat2_frame_model <- bike_sales |> 
  group_by(model, category.secondary, frame) |> 
  summarize(total_sales = sum(price.ext)) |> 
  ungroup() |> 
  arrange(desc(total_sales)) |> 
  mutate(category.secondary = fct_reorder(.f = category.secondary,
                                          .x = total_sales))

total_sales_by_cat2_frame_model

## Creating the plot ----
total_sales_by_cat2_frame_model |> 
  ggplot() + 
  geom_point(aes(x = frame, y = total_sales),
             position = position_jitter(width  = 0.2,
                                        height = 0.2)) +
  scale_y_continuous(labels = label_dollar()) + 
  facet_wrap(facets = vars(category.secondary)) +
  labs(x = "Frame type",
       y = "Total sales (US Dollars)",
       title = "Producto gap and total sales", 
       subtitle = "Period: 2011-01-07 - 2015-12-25")
  
  


