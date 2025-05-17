# Libraries ----
library(tidyverse)
library(sweep)
library(skimr)
library(DT)
library(tidymodels)

# Import data ----
bike_sales <- bike_sales

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

# Inspect ----
bike_sales |> 
  glimpse()

bike_sales |> 
  skim()

bike_sales |> 
  count(category.secondary)

bike_sales |> 
  group_by(category.secondary) |> 
  summarise(mean_price = mean(price)) |> 
  arrange(desc(mean_price))

# Product gap ----
## Prepare data ----
total_revenue_by_cat2_frame <- bike_sales |> 
  group_by(category.secondary, frame) |> 
  summarise(revenue = sum(price.ext)) |> 
  ungroup() |> 
  arrange(desc(revenue)) |> 
  mutate(category.secondary = fct_reorder(.f = category.secondary,
                                          .x = revenue))

## Visualization ----
total_revenue_by_cat2_frame |> 
  ggplot() + 
  geom_point(aes(x = frame,
                 y = revenue),
             position = position_jitter(width = 0.2,
                                        height = 0.2)) +
  scale_y_continuous(labels = scales::label_currency()) + 
  facet_wrap(facets = vars(category.secondary)) +
  labs(x = "Frame",
       y = "Revenue (US Dollars)")

# Determine the price of the new product ----
## Alpha
alpha <- 0.05

## Model 1 ----
model1 <- lm(formula = price ~ category.secondary,
             data = bike_sales)

model_tidy1 <- model1 |> 
  tidy()

model_tidy1 |> 
  mutate(decision_rule = alpha > p.value)

## Model 2 ----
model2 <- lm(formula = price ~ category.secondary + frame,
             data = bike_sales)

model2

model_tidy2 <- model2 |> 
  tidy()

model_tidy2

## Model 3 ----
model3 <- lm(formula = price ~ category.secondary*frame,
             data = bike_sales)

model3

model_tidy3 <- model3 |> 
  tidy()

model_tidy3

## Model 4 ----
model4 <- lm(formula = price ~ category.secondary + frame + bikeshop.state,
             data = bike_sales)

model_tidy4 <- model4 |> 
  tidy()

model_tidy4 |> 
  mutate(decision_rule = alpha > p.value) |> 
  View()

## Predictions ----
new_data <- tibble(category.secondary = c("Cyclocross",
                                          "Fat Bike",
                                          "Over Mountain",
                                          "Sport",
                                          "Triathalon"),
                   frame = c("Aluminum",
                             "Carbon",
                             "Aluminum",
                             "Carbon",
                             "Aluminum"))

new_data

predict_prices_model2 <- predict(object = model2, 
        newdata = new_data) |> 
  enframe(name = "observations",
          value = "pred_price") |> 
  bind_cols(new_data) |> 
  arrange(desc(pred_price))

predict_prices_model2
