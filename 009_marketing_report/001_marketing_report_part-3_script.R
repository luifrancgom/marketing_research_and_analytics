# Load libraries ----
library(tidyverse)
library(scales)
library(sweep)
library(tidymodels)
library(DT)

# Import data ----
bike_sales <- bike_sales |> 
  mutate(
    bikeshop.name      = factor(x = bikeshop.name, ordered = FALSE),
    bikeshop.city      = factor(x = bikeshop.city, ordered = FALSE),
    bikeshop.state     = factor(x = bikeshop.state, ordered = FALSE),
    model              = factor(x = model, ordered = FALSE),
    category.primary   = factor(x = category.primary, ordered = FALSE),
    category.secondary = factor(x = category.secondary, ordered = FALSE),
    frame              = factor(x = frame, ordered = FALSE),
    order.id           = factor(x = order.id, ordered = FALSE),
    order.line         = factor(x = order.line, ordered = TRUE),
    customer.id        = factor(x = customer.id, ordered = FALSE),
    product.id         = factor(x = product.id, ordered = FALSE)
  )

# Product gap ----
## Count data ----
bike_sales |> 
  count(model)

bike_sales |> 
  count(category.secondary)

bike_sales |> 
  count(bikeshop.state)

## Prepare data ----
total_sales_by__model_cat2_frame <- bike_sales |> 
  select(
    model,
    category.secondary,
    frame,
    price.ext
  ) |> 
  group_by(
    model, category.secondary, frame
  ) |> 
  summarise(
    total_revenue = sum(price.ext)
  ) |> 
  ungroup() |> 
  arrange(desc(total_revenue)) |> 
  mutate(category.secondary = fct_reorder(
    .f = category.secondary,
    .x = total_revenue
  )
)

total_sales_by__model_cat2_frame

## Visualize data ----
total_sales_by__model_cat2_frame |> 
  ggplot() + 
  geom_point(
    aes(x = frame, y = total_revenue),
    position = position_jitter(
      width = 0.2,
      height = 0.2
    )
  ) +
  scale_y_continuous(
    labels = label_currency()
  ) +
  facet_wrap(
    facets = vars(category.secondary)
  ) +
  labs(
    x = "Frame",
    y = "Total revenue (US dollars)"
  )

# Linear models ----
## Estimation ----
# Reference category: Cross Country Race
model1 <- lm(
  formula = price ~ category.secondary,
  data = bike_sales
)

model1_tidy <- model1 |> 
  tidy()

model1_tidy

model2 <- lm(
  formula = price ~ category.secondary + frame,
  data = bike_sales
)

# Select model
model2_tidy <- model2 |> 
  tidy()

model2_tidy |> 
  datatable(
    colnames = c(
      "Term",
      "Estimated parameters",
      "Standard error",
      "Statistic",
      "P-value"
    )
  ) |> 
  formatRound(
    columns = c(2, 3, 4, 5),
    digits = 3
  )

# Discard
model3 <- lm(
  formula = price ~ category.secondary + frame + category.secondary:frame,
  data = bike_sales
)

model3_tidy <- model3 |> 
  tidy()

model3_tidy

model4 <- lm(
  formula = price ~ category.secondary + frame + bikeshop.state,
  data = bike_sales
)

model4_tidy <- model4 |> 
  tidy()

model4_tidy |> 
  View()

## Prediction ----
new_data <- tibble(
  category.secondary = "Fat Bike",
  frame = "Carbon"
)

model2_new_data_pred <- predict(
  object = model2,
  newdata = new_data
) |> 
  enframe(
    name = "Observation",
    value = "Predicted Price"
  ) |> 
  bind_cols(new_data)

model2_new_data_pred |> 
  datatable(
    colnames = c(
      "Observation",
      "Predicted price",
      "Category secondary",
      "Frame"
    )
  ) |> 
  formatRound(
    columns = c(2),
    digits = 2
  )
