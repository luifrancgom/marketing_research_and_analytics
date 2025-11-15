# Libraries ----
library(sweep)
library(tidyverse)
library(scales)
library(tidymodels)
library(performance)

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
         bikeshop.state = factor(x = bikeshop.state,
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

# Count ----
bike_sales |> 
  count(model,
        category.secondary,
        frame)

bike_sales |> 
  count(bikeshop.state) |> 
  arrange(bikeshop.state)

# Grouping ----
total_revenue_by_model_cat2_frame <- bike_sales |> 
  group_by(model, 
           category.secondary,
           frame) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  ungroup() |> 
  mutate(category.secondary = fct_reorder(
      .f = category.secondary,
      .x = total_revenue
  ))

# Visualization ----
total_revenue_by_model_cat2_frame |> 
  ggplot() + 
  geom_point(
    aes(x = frame, y = total_revenue),
    position = position_jitter(height = 0.2,
                               width = 0.2)
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

# Modeling ----
# Comparison category: Cross Country Race

## Alpha ----
alpha <- 0.05

## Model 1 ----
model_1 <- lm(
  formula = price ~ category.secondary,
  data = bike_sales
)

model_1_tidy <- model_1 |> 
  tidy()

model_1_tidy |>
  filter(alpha > p.value)

# check_model(model_1, 
#             panel = FALSE) |> 
#   plot()

# Model 2 ----
model_2 <- lm(
  formula = price ~ category.secondary + frame,
  data = bike_sales
)

model_2_tidy <- model_2 |> 
  tidy() 

model_2_tidy |> 
  View()

## Model 3 ----
model_3 <- lm(
  formula = price ~ category.secondary*frame,
  data = bike_sales
)

model_3_tidy <- model_3 |> 
  tidy()

model_3_tidy |> 
  View()

## Model 4 ----
model_4 <- lm(
  formula = price ~ category.secondary + frame + bikeshop.state,
  data = bike_sales
)

model_4_tidy <- model_4 |> 
  tidy()

model_4_tidy |> 
  View()

## Compare models ----
anova_lm <- anova(
  model_1,
  model_2,
  model_4,
  test = "F"
)

anova_lm_tidy <- anova_lm |> 
  tidy()

anova_lm_tidy  

## Predict
new_data <- tibble(
  category.secondary = c("Fat Bike",
                         "Fat Bike"),
  frame = c("Carbon",
            "Carbon"),
  bikeshop.state = c("CA",
                     "PA")
)

model_3_pred <- predict(
  object = model_3,
  newdata = new_data
)

model_3_pred |> 
  enframe(
    name = "obsevation",
    value = "pred_price"
  ) |> 
  bind_cols(
    new_data
  )
