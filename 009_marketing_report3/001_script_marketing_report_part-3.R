# Libraries -----
library(sweep)
library(tidyverse)

# Import data -----
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
         bikeshop.state = factor(x = bikeshop.state, 
                                ordered = FALSE),
         product.id = factor(x = product.id, 
                             ordered = FALSE),
         model      = factor(x = model, 
                             ordered = FALSE),
         category.primary = factor(x = category.primary , 
                                   ordered = FALSE),
         category.secondary = factor(x = category.secondary, 
                                     ordered = FALSE),
         frame = factor(x = frame, 
                        ordered = FALSE))

# Inspect data -----
bike_sales |> glimpse()

