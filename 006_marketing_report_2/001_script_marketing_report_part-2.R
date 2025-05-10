# Libraries ----
library(tidyverse)
library(sweep)
library(DT)
library(tidymodels)

# Import data ----
bike_sales <- bike_sales

# Inspect data ----
bike_sales |> 
  glimpse()

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

# Relation between continuous variables ----
## price vs quantity (color by frame)
bike_sales |> 
  ggplot() +
  geom_point(aes(x = quantity, 
                 y = price,
                 color = frame)) +
  labs(x = "Quantity",
       y = "Price (per unit in US Dollars)",
       color = "Frame",
       title = "Quantities sold vs price of frames")

## price vs quantity (color by frame) transformed
bike_sales |> 
  ggplot() +
  geom_point(aes(x = quantity, 
                 y = price,
                 color = frame)) +
  scale_x_continuous(transform = "log") +
  scale_y_continuous(transform = "log") +
  labs(x = "Quantity",
       y = "Price (per unit in US Dollars)",
       color = "Frame",
       title = "Quantities sold vs price of frames")

## Summary statistics ----
corr_price_quantity <- bike_sales |> 
  summarise(corr_price_quantity = cor(quantity, price))

# Product gap ----
bike_sales |> 
  ggplot() + 
  geom_point(aes(x = frame, y = category.secondary),
             position = position_jitter(width = 0.2,
                                        height = 0.2))

bike_sales |> 
  count(frame, category.secondary) |> 
  ggplot() +
  geom_label(aes(x = frame,
                 y = category.secondary,
                 label = n))

# Descriptive statistics by group ----
## Category secondary, frame: revenue ----
bike_sales |> 
  count(category.secondary, frame)

stats_by_cat2_frame <- bike_sales |> 
  group_by(category.secondary,
           frame) |> 
  summarise(mean_revenue = mean(price.ext),
            median_revenue = median(price.ext),
            sd_revenue = sd(price.ext)) |> 
  ungroup()

names_cat2_frame <- c("Category secondary" = "category.secondary",
                      "Frame" = "frame",
                      "Mean revenue" = "mean_revenue",
                      "Median revenue" = "median_revenue",
                      "Standard deviation" = "sd_revenue")
  
stats_by_cat2_frame |> 
  datatable(colnames = names_cat2_frame) |> 
  formatRound(columns = c("Mean revenue",
                          "Standard deviation"), 
              digits = 2)

## Visualization ---
bike_sales |> 
  filter(category.secondary == "Elite Road",
         frame == "Carbon") |> 
  ggplot() + 
  geom_histogram(aes(x = price.ext)) +
  scale_x_continuous(labels = scales::label_currency())

## Category secondary, frame: percentage revenue ----
revenue_by_cat2_frame <- bike_sales |> 
  group_by(category.secondary, frame) |> 
  summarise(revenue = sum(price.ext)) |> 
  ungroup() |> 
  mutate(pct_revenue = (revenue / sum(revenue))*100) |> 
  arrange(desc(pct_revenue))

names_revenue_cat2_frame <- c("Category secondary" = "category.secondary",
                              "Frame" = "frame",
                              "Revenue" = "revenue",
                              "Percentage of revenue" = "pct_revenue")
  
revenue_by_cat2_frame |> 
  datatable(colnames = names_revenue_cat2_frame) |> 
  formatRound(columns = c("Percentage of revenue"), 
              digits = 2)

## Bike shops: revenue ----
stats_by_bike_shop <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(mean_revenue = mean(price.ext),
            median_revenue = median(price.ext),
            sd_revenue = sd(price.ext))

names_bike_shop <- c("Bike shop" = "bikeshop.name",
                     "Mean revenue" = "mean_revenue",
                     "Median revenue" = "median_revenue",
                     "Standard deviation" = "sd_revenue")
  
stats_by_bike_shop |> 
  datatable(colnames = names_bike_shop) |> 
  formatRound(columns = c("Mean revenue",
                          "Standard deviation"), 
              digits = 2)

## Bike shop: percentage revenue ----
revenue_by_bike_shop <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(revenue = sum(price.ext)) |> 
  mutate(pct_revenue = (revenue / sum(revenue))*100) |> 
  arrange(desc(pct_revenue))

names_revenue_bike_shop <- c("Bike shop" = "bikeshop.name",
                             "Revenue" = "revenue",
                             "Percentage of revenue" = "pct_revenue")

revenue_by_bike_shop |> 
  datatable(colnames = names_revenue_bike_shop) |> 
  formatRound(columns = c("Percentage of revenue"), 
              digits = 2)

# Statistical tests ----
## Chiq test given probabilities ----
### Category secondary, frame ----
bike_sales |> 
  count(category.secondary, frame)

table_cat2_frame <- bike_sales |> 
  group_by(category.secondary, frame) |> 
  summarise(revenue = sum(price.ext)) |> 
  ungroup() |> 
  mutate(pct_revenue = revenue / sum(revenue)) |> 
  arrange(desc(pct_revenue))

table_models_cat2_frame <- table_cat2_frame |> 
  mutate(model = str_c(category.secondary,
                       frame, 
                       sep = " ")) |> 
  select(model, revenue)

table_models_cat2_frame

chi2_test_cat2_frame <- deframe(table_models_cat2_frame) |>
  chisq.test(p = rep.int(x = 1/13, times = 13))

chi2_test_cat2_frame |> 
  tidy()

### Bike shops ----
bike_sales |> count(bikeshop.name)

table_bikeshops <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(revenue = sum(price.ext)) |> 
  mutate(pct_revenue = revenue / sum(revenue)) |> 
  arrange(desc(pct_revenue))

table_models_bikeshops <- table_bikeshops |> 
  select(bikeshop.name, revenue)

chi2_test_bikeshops <- table_models_bikeshops |> 
  deframe() |> 
  chisq.test(p = rep.int(x = 1/30, times = 30))
  
chi2_test_bikeshops |> 
  tidy()

## T-test ----
### frame ----
alpha <- 0.05 # 0.1, 0.05, 0.01
t_test_revenue_frame <- t.test(price.ext ~ frame,
                               data = bike_sales,
                               alternative = "two.sided",
                               mu = 0,
                               conf.level = 1 - alpha)

t_test_revenue_frame |> 
  tidy()

## Anova ----
model1 <- aov(formula = price ~ category.secondary,
              data = bike_sales) |> 
  anova() |> 
  tidy()

model2 <- aov(formula = price ~ category.secondary + frame,
              data = bike_sales) |> 
  anova() |> 
  tidy()

model3 <- aov(formula = price ~ category.secondary*frame,
              data = bike_sales) |> 
  anova() |> 
  tidy()

model4 <- aov(formula = price ~ category.secondary*frame*bikeshop.city,
              data = bike_sales) |> 
  anova() |> 
  tidy()






  
