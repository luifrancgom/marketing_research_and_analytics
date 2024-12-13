# Libraries ----
library(sweep)
library(tidyverse)
library(scales)
library(tidymodels)

# Import data ----
bike_sales <- bike_sales

# Inspect data ----
bike_sales |> glimpse()

# Transform data ----
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
         product.id = factor(x = product.id, 
                                ordered = FALSE),
         category.primary = factor(x = category.primary , 
                                   ordered = FALSE),
         category.secondary = factor(x = category.secondary, 
                                     ordered = FALSE),
         frame = factor(x = frame, 
                        ordered = FALSE))

# Data visualization ----
## Price vs quantity ----
bike_sales_p_q_frame <- bike_sales |> 
  select(price, quantity, frame)

### Without transformation ----
bike_sales_p_q_frame |> 
  ggplot() + 
  geom_point(aes(x = price, y = quantity,
                 color = frame)) +
  scale_x_continuous(labels = label_dollar()) +
  labs(x = "Price (US Dollars)",
       y = "Quantity (units)",
       color = "Frame",
       title = "Relation between price and units sale")

### Experiment ----  
# Note: checking if by facetting we ge something new
## Definitely no!!!
bike_sales_p_q_frame_cat2 <- bike_sales |> 
  select(price, quantity, frame, category.secondary)

bike_sales_p_q_frame_cat2 |> 
  ggplot() + 
  geom_point(aes(x = price, y = quantity,
                 color = frame)) +
  scale_x_continuous(labels = label_dollar()) +
  facet_wrap(facets = vars(category.secondary)) +
  labs(x = "Price (US Dollars)",
       y = "Quantity (units)",
       color = "Frame",
       title = "Relation between price and units sale")

### With logarithmic transformation
bike_sales_p_q_frame |> 
  ggplot() + 
  geom_point(aes(x = price, y = quantity,
                 color = frame)) +
  scale_x_continuous(labels = label_dollar(), 
                     transform = "log10") +
  scale_y_continuous(transform = "log10") +
  labs(x = "Price (US Dollars) - Logarithmic Base 10 scale",
       y = "Quantity (units)  - Logarithmic Base 10 scale",
       color = "Frame",
       title = "Relation between price and units sale")

## Frame vs Second category ----
bike_sales_frame_cat2 <- bike_sales |> 
  select(frame, category.secondary)

bike_sales_frame_cat2 |> 
  ggplot() +
  geom_point(aes(x = frame, y = category.secondary),
             position = position_jitter(width = 0.2,
                                        height = 0.2)) +
  labs(x = "Frame material",
       y = "Secondary Category",
       title = "Identifying product gaps")

# Descriptive statistics by group ----
## Mean, median, sd by frame and secondary category ----
bike_sales_frame_cat2_revenue <- bike_sales |> 
  select(frame, category.secondary, price.ext)

mean_median_sd <- bike_sales_frame_cat2_revenue |> 
  group_by(frame, category.secondary) |> 
  summarise(mean_revenue = mean(price.ext),
            median_revenue = median(price.ext),
            sd_revenue = sd(price.ext)) |> 
  ungroup() |> 
  arrange(desc(median_revenue))

mean_median_sd

## Percentage of revenue by frame and secondary category ----
pct_revenue <- bike_sales_frame_cat2_revenue |> 
  group_by(frame, category.secondary) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  ungroup() |> 
  mutate(pct_total_revenue = (total_revenue / sum(total_revenue))*100) |> 
  arrange(desc(pct_total_revenue))
  
pct_revenue

## Mean, median, sd by bike shop ----  
bike_sales_bikeshop_revenue <- bike_sales |> 
  select(bikeshop.name, price.ext)

mean_median_sd_bikeshop <- bike_sales_bikeshop_revenue |> 
  group_by(bikeshop.name) |> 
  summarise(mean_revenue = mean(price.ext),
            median_revenue = median(price.ext),
            sd_revenue = sd(price.ext)) |> 
  ungroup() |> 
  arrange(desc(median_revenue))

mean_median_sd_bikeshop

## Percentage of revenue by bike shop ----
pct_revenue_bikeshop <- bike_sales_bikeshop_revenue |> 
  group_by(bikeshop.name) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  ungroup() |> 
  mutate(pct_total_revenue = (total_revenue / sum(total_revenue))*100) |> 
  arrange(desc(pct_total_revenue))

pct_revenue_bikeshop

# Statistical tests ----
## Frame - Category Secondary ----
table_models <- pct_revenue |> 
  mutate(model = str_c(frame, category.secondary, 
                       sep = " ")) |> 
  select(model, total_revenue)

chiq_test_proportion_model <- table_models |> 
  deframe() |> 
  chisq.test(p = rep.int(x = 1/13, times = 13),
             correct = FALSE) |> 
  tidy()

chiq_test_proportion_model

## Bikeshops ----
table_bikeshops <- pct_revenue_bikeshop |> 
  select(bikeshop.name, total_revenue)

chiq_test_proportion_bikeshop <- table_bikeshops |> 
  deframe() |> 
  chisq.test(correct = FALSE) |> 
  tidy()

chiq_test_proportion_bikeshop


## T-test
bike_sales |> 
  count(frame)

t_test_revenue_frame <- bike_sales |> 
  t_test(formula = price.ext ~ frame, 
         # H0: mean(Aluminum) - mean(Carbon) = 0 
         order = c("Aluminum", "Carbon"), 
         mu = 0, 
         alternative = "two-sided",
         # 1 - alpha = 1 - 0.05
         conf_level = 1 - 0.05)

# Conclusion: There is a difference between
#             the revenue per transaction between
#             Aluminum and Carbon frame

t_test_revenue_frame

bike_sales |> 
  ggplot(aes(x = frame, y = price.ext)) + 
  geom_point() +
  stat_summary(fun = "median",
               color = "red") + 
  scale_y_continuous(labels = dollar_format(), 
                     transform = "log10")

## Anova test ----
model_1 <- aov(formula = price ~ category.secondary,
               data = bike_sales) |>
  anova()

model_1

model_2 <- aov(formula = price ~ category.secondary + frame,
               data = bike_sales) |> 
  anova()

model_2

model_3 <- aov(formula = price ~ category.secondary + frame + category.secondary:frame,
               data = bike_sales) |> 
  anova()

model_3

model_3_easy <- aov(formula = price ~ category.secondary*frame,
                    data = bike_sales) |> 
  anova()

model_3_easy

model_4 <- aov(formula = price ~ category.secondary*frame*bikeshop.state,
               data = bike_sales) |> 
  anova()

model_4

model_5 <- aov(formula = price ~ category.secondary*frame*bikeshop.city,
               data = bike_sales) |> 
  anova()

model_5
  