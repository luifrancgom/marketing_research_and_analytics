# Libraries -----
library(sweep)
library(tidyverse)
library(skimr)
library(scales)
library(tidymodels)

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
  
# Modeling to predict price ----
bike_sales |> 
  count(bikeshop.state)

          # lm: linear model
model1 <- lm(formula = price ~ category.secondary*frame*bikeshop.state,
             data = bike_sales)

model1_tidy <- model1 |> tidy()


### Checking model assumptions
plot(model1)

alpha <- 0.05

model1_tidy |> 
  filter(p.value >= 0.05)

# lm: linear model
model2 <- lm(formula = price ~ category.secondary*frame,
             data = bike_sales)

### Checking assumptions
plot(model2)

model2_tidy <- model2 |> tidy()
model2_tidy |> 
  filter(p.value < 0.05)

## Price predictions ----
new_data <- tibble(category.secondary = c("Fat Bike", "Over Mountain"),
                   frame              = c("Carbon"  , "Aluminum"))

predictions <- predict(object = model2,
                       newdata = new_data) |> 
  enframe(name = "observation", value = "price_prediction") |> 
  bind_cols(new_data) |> 
  arrange(desc(price_prediction))

predictions

# Data complexity reduction and segmentation ----
## Prepare data ----
bikeshop_sales_total_revenue <- bike_sales |> 
  select(bikeshop.name, 
         price.ext, 
         model, 
         category.primary, 
         category.secondary, 
         frame) |> 
  group_by(bikeshop.name, 
           model, 
           category.primary, 
           category.secondary, 
           frame) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  ungroup()

bikeshop_sales_total_revenue

### Normalization ----
bikeshop_sales_total_revenue_pct <- bikeshop_sales_total_revenue |> 
  group_by(bikeshop.name) |> 
  mutate(total_revenue_pct = total_revenue / sum(total_revenue)) |> 
  ungroup() 

### Consumer-product table ----
customer_product_table <- bikeshop_sales_total_revenue_pct |> 
  select(bikeshop.name, model, total_revenue_pct) |> 
  pivot_wider(id_cols = bikeshop.name, 
              names_from = model,
              values_from = total_revenue_pct,
              values_fill = 0)

customer_product_table


