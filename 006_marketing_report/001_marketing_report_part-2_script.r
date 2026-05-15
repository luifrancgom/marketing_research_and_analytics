# Load libraries ----
library(sweep)
library(tidyverse)
library(scales)
library(DT)
library(tidymodels)

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

# Relation between continuous variables ----
## Price vs quantity by frame ----
bike_sales |> 
  ggplot() +
  geom_point(
    aes(x = price, y = quantity,
        color = frame)
  ) +
  scale_x_continuous(
    labels = label_currency()
  ) +
  labs(
    x = "Price (US Dollars/Frame)",
    y = "Quantity (Units)",
    color = "Frame type"
  )

cor_price_quantity <- cor(x = bike_sales$price, y = bike_sales$quantity) |> 
  format(scientific = FALSE)

## Price vs quantity transformed by frame
bike_sales |> 
  ggplot() +
  geom_point(
    aes(x = price, y = quantity,
        color = frame)
  ) +
  scale_x_continuous(
    labels = label_currency(accuracy = 1),
    transform = "log10"
  ) +
  scale_y_continuous(
    transform = "log10"
  ) +
  labs(
    x = "Price (US Dollars/Frame)",
    y = "Quantity (Units)",
    color = "Frame type"
  )

cor_price_quantity_trans <- cor(
  x = log(bike_sales$price, base = 10), 
  y = log(bike_sales$quantity, base = 10)) |> 
  format(scientific = FALSE)

## Frame vs Category Secondary ----
bike_sales |> 
  ggplot() +
  geom_point(
    aes(x = frame, y = category.secondary),
    position = position_jitter(
      width = 0.2, height = 0.2)
  ) +
  labs(
    x = "Frame type",
    y = "Category Secondary"
  )

frame_category_secondary_count <- bike_sales |> 
  count(frame, category.secondary)

frame_category_secondary_count |> 
  ggplot() +
  geom_label(
    aes(
      x = frame,
      y = category.secondary,
      label = n
    )
  ) +
  labs(
    x = "Frame type",
    y = "Category Secondary"
  )

# Descriptive statistics by group ----
## Mean, median, sd ----
revenue_stats_by_cat2_frame <- bike_sales |> 
  group_by(category.secondary, frame) |> 
  summarise(
    mean_revenue   = mean(price.ext),
    median_revenue = median(price.ext),
    sd_revenue     = sd(price.ext) 
  ) |> 
  ungroup() |> 
  arrange(desc(mean_revenue))

revenue_stats_by_cat2_frame |> 
  datatable(
    colnames = c(
      "Category secondary",
      "Frame",
      "Mean",
      "Median",
      "Standard deviation"
    )
  ) |> 
  formatRound(
    columns = c(3, 5),
    digits = 2
  )

revenue_stats_by_bikeshop <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(
    mean_revenue   = mean(price.ext),
    median_revenue = median(price.ext),
    sd_revenue     = sd(price.ext) 
  ) |> 
  ungroup() |> 
  arrange(desc(mean_revenue))

revenue_stats_by_bikeshop |> 
  datatable(
    colnames = c(
      "Bikeshop",
      "Mean",
      "Median",
      "Standard deviation"
    )
  ) |> 
  formatRound(
    columns = c(2, 4),
    digits = 2
  )

## Percentage of revenue ----
pct_revenue_by_cat2_frame <- bike_sales |> 
  group_by(category.secondary, frame) |> 
  summarise(
    total_revenue = sum(price.ext), 
  ) |> 
  ungroup() |> 
  mutate(
    pct_revenue = (total_revenue / sum(total_revenue))*100
  ) |> 
  arrange(desc(pct_revenue))

pct_revenue_by_cat2_frame |> 
  datatable(
    colnames =  c(
      "Category secondary",
      "Frame",
      "Total revenue",
      "Percentage total revenue"
    )
  ) |> 
  formatRound(
    columns = 4
  )

pct_revenue_by_bikeshop <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(
    total_revenue = sum(price.ext), 
  ) |> 
  ungroup() |> 
  mutate(
    pct_revenue = (total_revenue / sum(total_revenue))*100
  ) |> 
  arrange(desc(pct_revenue))

pct_revenue_by_bikeshop |> 
  datatable(
    colnames =  c(
      "Bikeshop",
      "Total revenue",
      "Percentage total revenue"
    )
  ) |> 
  formatRound(
    columns = 3
  )

# Statistical tests ----
## Chiq test ----
bike_sales |> 
  count(category.secondary, frame)

### Prepare data ----
total_revenue_by_cat2_frame <- bike_sales |> 
  group_by(category.secondary, frame) |> 
  summarise(
    total_revenue = sum(price.ext), 
  ) |> 
  ungroup() 

table_models <- total_revenue_by_cat2_frame |> 
  mutate(model = str_c(
    category.secondary, frame, sep = " "
  )
) |> 
select(model, total_revenue)

chi_test_total_revenue_cat2_frame <- table_models |> 
  deframe() |> 
  chisq.test(
    p = rep.int(x = 1 / 13, times = 13) 
  ) |> 
  tidy()

alpha <- 0.05

qchisq(p = alpha, df = 12, lower.tail = FALSE)

chi_test_total_revenue_cat2_frame |> 
  datatable(
    colnames = c(
      "Statistic",
      "P-value",
      "Degrees of freedom",
      "Method"
    )
  ) |> 
  formatRound(
    columns = c(1), 
    digits = 0
  )

total_revenue_by_bikeshop <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(
    total_revenue = sum(price.ext), 
  ) |> 
  ungroup()

chi_test_total_revenue_bikeshop <- total_revenue_by_bikeshop |> 
  deframe() |> 
  chisq.test(
    p = rep.int(x = 1 / 30, times = 30) 
  ) |> 
  tidy()

chi_test_total_revenue_bikeshop |> 
  datatable(
        colnames = c(
      "Statistic",
      "P-value",
      "Degrees of freedom",
      "Method"
    )
  ) |> 
  formatRound(
    columns = c(1),
    digits = 0
  )

## T-test ----
mean_revenue_by_frame <- bike_sales |> 
  group_by(frame) |> 
  summarise(
    mean_revenue = mean(price.ext)
  )

mean_revenue_by_frame

alpha <- 0.05

t_test_mean_revenue_by_frame <- bike_sales |> 
  t_test(
    formula = price.ext ~ frame,
    order = c("Aluminum", "Carbon"),
    alternative = "two-sided",
    mu = 0,
    conf_level = 1 - alpha
  )

t_test_mean_revenue_by_frame |> 
  select(-c(lower_ci, upper_ci)) |> 
  datatable(
    colnames = c(
      "Statistic",
      "Degrees of freedom",
      "P-value",
      "Alternative hypothesis",
      "Mean difference estimation"
    )
  ) |> 
  formatRound(
    columns = c(1, 2, 3, 5),
    digits = 3
  )

# Anova test
# price = f(category.secondary)
# price = f(category.secondary, frame)
bike_sales |> 
  summarise(mean_price = mean(price))

bike_sales |> 
  group_by(category.secondary) |> 
  summarise(
    mean_price = mean(price)
  )

model1 <- aov(
  formula = price ~ category.secondary,
  data = bike_sales
) |> 
anova() |> 
tidy()

model1

model2 <- aov(
  formula = price ~ category.secondary + frame,
  data = bike_sales
) |> 
anova() |> 
tidy()

model2

model3 <- aov(
  formula = price ~ category.secondary + frame + category.secondary:frame,
  data = bike_sales
) |> 
anova() |> 
tidy()

model3 |> 
  datatable(
    colnames = c(
      "Term",
      "Degrees of freedom",
      "Sum of squares",
      "Mean or squares",
      "Statistic",
      "P-value"
    )
  ) |> 
  formatRound(
    columns = c(3, 4, 5, 6),
    digits = 3
  )

model4 <- aov(
  formula = price ~ category.secondary*frame*bikeshop.state,
  data = bike_sales
) |> 
anova() |> 
tidy()

model4 |> 
  filter(alpha > p.value)
