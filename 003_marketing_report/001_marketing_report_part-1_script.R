# Libraries ----
library(tidyverse)
library(sweep)
library(skimr)
library(DT)

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

# Inspect ----
bike_sales |> 
  glimpse()

# Summarize ----
## Summary statistics ----
bike_sales |> 
  skim()

## Count ----
category_secondary <- bike_sales |> 
  count(category.secondary) |> 
  select(category.secondary)

category_secondary |> 
  datatable(
    colnames = c("Category secondary" = "category.secondary") 
  )

frame <- bike_sales |> 
  count(frame) |> 
  select(frame)

frame |> 
  datatable(
    colnames = c("Frame" = "frame") 
  )

category_secondary_frame <- bike_sales |> 
  count(category.secondary,
        frame) |> 
  select(category.secondary, frame)

category_secondary_frame |> 
  datatable(
    colnames = c("Category secondary", "Frame") 
  )

# Cyclocross   Aluminum
# Fat Bike     Carbon
# Overmountain Aluminum
# Sport        Carbon
# Triathalon   Carbon

bike_shops <- bike_sales |> 
  count(bikeshop.name, 
        sort = TRUE)

bike_shops |> 
  datatable(
    colnames = c("Bike shop" = "bikeshop.name",
                 "Transactions" = "n")
  )

# Data visualization ----
## Histograms ----

