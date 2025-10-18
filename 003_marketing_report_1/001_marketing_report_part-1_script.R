# Libraries ----
library(tidyverse)
library(sweep)
library(skimr)
library(DT)
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

bike_sales |> 
  count(quantity,
        sort = TRUE)

# Data visualization ----
## Histograms ----
bike_sales |> 
  ggplot() +
  geom_histogram(aes(x = quantity),
                 color = "black",
                 fill = "steelblue",
                 bins = 30) +
  labs(x = "Units sold",
       y = "Frequency",
       subtitle = "Distribution of units sold")

bike_sales |> 
  ggplot() +
  geom_histogram(aes(x = price),
                 color = "black",
                 fill = "steelblue",
                 bins = 30) +
  labs(x = "Price (US Dollars)",
       y = "Frequency",
       subtitle = "Distribution of frame prices")
  
## Boxplots ----
bike_sales |> 
  mutate(category.secondary = fct_reorder(.f = category.secondary,
                                          .x = price.ext, 
                                          .fun = median)) |> 
  ggplot() +
  geom_boxplot(aes(x = price.ext,
                   y = category.secondary),
               fill = "steelblue") +
  scale_x_continuous(labels = label_currency(), 
                     transform = "log10") +
  labs(x = "Revenue (US Dollars)",
       y = "Category secondary",
       subtitle = "Distribution of revenue by category secondary")

bike_sales |> 
  ggplot() +
  geom_boxplot(aes(x = price.ext,
                   y = frame),
               fill = "steelblue") +
  scale_x_continuous(labels = label_currency(),
                     transform = "log10") +
  labs(x = "Revenue (US Dollars)",
       y = "Frame material",
       subtitle = "Distribution of revenue by frame material")
  
# Answering questions with data ----
bike_sales_revenue_by_cat2 <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(revenue = sum(price.ext)) |> 
  mutate(bikeshop.name = fct_reorder(.f = bikeshop.name,
                                     .x = revenue))

bike_sales_revenue_by_cat2 |> 
  ggplot() +
  geom_col(aes(x = revenue,
               y = bikeshop.name),
           color = "black",
           fill = "steelblue") +
  scale_x_continuous(label = label_currency()) +
  labs(x = "Revenue (US Dollars)",
       y = "Bike shops",
       subtitle = "Revenue by bike shops")
  
bike_sales_revenue_by_frame <- bike_sales |> 
  group_by(category.secondary) |> 
  summarise(revenue = sum(price.ext)) |> 
  mutate(category.secondary = fct_reorder(.f = category.secondary,
                                          .x = revenue))

bike_sales_revenue_by_frame |> 
  ggplot() +
  geom_col(aes(x = revenue,
               y = category.secondary),
           fill = "steelblue",
           color = "black") +
  scale_x_continuous(labels = label_currency()) +
  labs(x = "Revenue (US Dollars)",
       y = "Category secondary",
       subtitle = "Revenue by category secondary")
  










