# Libraries ----
library(tidyverse)
library(sweep)
library(skimr)
library(scales)
library(DT)
library(tidymodels)

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

# Summarize ----
bike_sales |> 
  skim()

# Relation continuous var ----
# Colors: hex codes
# https://htmlcolorcodes.com/
# https://www.color-hex.com/
bike_sales |> 
  ggplot() + 
  geom_point(aes(x = price,
                 y = quantity,
                 color = frame)) +
  scale_color_manual(values = c("#043074",
                                "#FDC600")) +
  labs(x = "Price (US Dollars / 1 frame)",
       y = "Quantity",
       color = "Frame",
       subtitle = "Relationship between quantity and price")

## Correlation ----
cor_price_quantity <- cor(x = bike_sales$price,
    y = bike_sales$quantity) |> 
  number(accuracy = 0.000001)

cor_price_quantity

bike_sales |> 
  ggplot() +
  geom_point(aes(x = price,
                 y = quantity,
                 color = frame)) +
  scale_x_continuous(transform = "log10") +
  scale_y_continuous(transform = "log10") +
  scale_color_manual(values = c("#043074",
                                "#FDC600")) +
  labs(x = "Price (US Dollars / 1 frame)",
       y = "Quantity",
       color = "Frame",
       subtitle = "Relationship between quantity and price using a logarithmic scale in base 10")

log_10_price <- log(x = bike_sales$price,
                    base = 10)
log_10_quantity <- log(x = bike_sales$quantity,
                       base = 10) 
cor_log_10_price_quantity <- cor(x = log_10_price,
                                 y = log_10_quantity) |> 
  number(accuracy = 0.000001)

# Relation categorical var ----
## Scatterplot ----
bike_sales |> 
  ggplot() +
  geom_point(aes(x = frame,
                 y = category.secondary),
             position = position_jitter(width = 0.2,
                                        height = 0.2)) +
  labs(x = "Frame",
       y = "Category Secondary",
       subtitle = "Frame vs Category secondary")

## Text ----
bike_sales |> 
  count(frame, category.secondary) |> 
  ggplot() +
  geom_label(aes(x = frame,
                 y = category.secondary,
                 label = n)) +
  labs(x = "Frame",
       y = "Category secondary",
       subtitle = "Frame vs Category secondary")

# Descriptive statistics by group ----
## Category secondary, frame ----
### Mean, median, sd ----
bike_sales_cat2_frame_stats_rev <- bike_sales |> 
  group_by(category.secondary, frame) |> 
  summarise(
    mean_rev = mean(x = price.ext),
    median_rev = median(x = price.ext),
    sd_rev = sd(x = price.ext)
  ) |> 
  ungroup()

bike_sales_cat2_frame_stats_rev |> 
  datatable(
    colnames = c("Category secondary",
                 "Frame",
                 "Mean revenue",
                 "Median revenue",
                 "Standard deviation revenue") 
      ) |> 
  formatRound(
    columns = c("mean_rev",
                "sd_rev"), 
    digits = 2  
  )
  
### Total and percentage revenue ----
bike_sales_cat2_frame_total_pct_rev <- bike_sales |> 
  group_by(category.secondary, frame) |> 
  summarise(total_rev = sum(price.ext)) |> 
  ungroup() |> 
  mutate(total_rev_pct = (total_rev / sum(total_rev)) * 100)
  
bike_sales_cat2_frame_total_pct_rev |> 
  datatable(
    colnames = c("Category secondary",
                 "Frame",
                 "Total revenue",
                 "Percentage of total revenue") 
  ) |> 
  formatRound(
    columns = c("total_rev_pct") ,
    digits = 2
      )

## Bikeshop ---- 
### Mean, median, sd ----
bike_sales_bikeshop_stats_rev <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(
    mean_rev = mean(x = price.ext),
    median_rev = median(x = price.ext),
    sd_rev = sd(x = price.ext)
  )

bike_sales_bikeshop_stats_rev |> 
  datatable(
    colnames = c("Bikeshop",
                 "Mean revenue",
                 "Median revenue",
                 "Standard deviation revenue")
  ) |> 
  formatRound(
    columns = c("mean_rev",
                "sd_rev"),
    digits = 2
  )

### Total and percentage revenue ----
bike_sales_bikeshop_total_rev_pct <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(total_rev = sum(price.ext)) |> 
  mutate(total_rev_pct = (total_rev / sum(total_rev)) * 100)

bike_sales_bikeshop_total_rev_pct |> 
  datatable(
    colnames = c("Bikeshop",
                 "Total revenue",
                 "Percentage of total revenue")
  ) |> 
  formatRound(
    columns = c("total_rev_pct"),
    digits = 2
  )
  
# Statistical tests ----
revenue_cat2_frame <- bike_sales |> 
  group_by(frame, category.secondary) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  ungroup() |> 
  arrange(desc(total_revenue)) |> 
  mutate(total_revenue_pct = (total_revenue / sum(total_revenue))*100)

revenue_cat2_frame

## Prepare data ----
### Category secondary, frame ----
table_model <- revenue_cat2_frame |> 
  mutate(model = str_c(frame,
                       category.secondary,
                       sep = " ")) |> 
  select(model, total_revenue_pct)

chi_test <- deframe(table_model) |> 
  chisq.test(p = rep.int(x = 1/13, times = 13)) |> 
  tidy()

chi_test |> 
  datatable(
    colnames = c("Statistic",
                 "P-value",
                 "Parameter",
                 "Method") 
  ) |> 
  formatRound(
    columns = "statistic",
    digits = 2
  ) |> 
  formatRound(
    columns = "p.value",
    digits = 7
    )

### Bikeshop
revenue_bikeshop <- bike_sales |> 
  group_by(bikeshop.name) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  arrange(desc(total_revenue)) |> 
  mutate(total_revenue_pct = (total_revenue / sum(total_revenue)) * 100)

table_model_bikeshop <- revenue_bikeshop |> 
  select(bikeshop.name, total_revenue_pct) |> 
  deframe()

chi_test_bikeshop <- table_model_bikeshop |> 
  chisq.test(p = rep.int(x = 1/30, times = 30)) |> 
  tidy()

chi_test_bikeshop |> 
  datatable(
    colnames = c("Statistic", 
                 "P-value",
                 "Parameter",
                 "Method")
  ) |> 
  formatRound(
    columns = "statistic",
    digits = 2
  ) |> 
  formatRound(
    columns = "p.value",
    digits = 7
  )

## t-test ----
alpha <- 0.05

t_test_frame <- bike_sales |> 
  t_test(formula = price.ext ~ frame, 
         order = c("Aluminum", "Carbon"),
         mu = 0, 
         alternative = "two-sided",
         conf_level = 1 - alpha)

t_test_frame |> 
  datatable(
    colnames = c("Statistic",
                 "Parameter",
                 "P-value",
                 "Alternative hypothesis",
                 "Estimated mean",
                 "Lower CI",
                 "Upper CI")
  ) |> 
  formatRound(
    columns = c("statistic",
                "t_df",
                "p_value",
                "estimate",
                "lower_ci",
                "upper_ci"),
    digits = 3
  )
  
## Anova ----
model_1 <- bike_sales |> 
  aov(formula = price ~ category.secondary) |> 
  anova() |> 
  tidy()

model_2 <- bike_sales |> 
  aov(formula = price ~ category.secondary + frame) |> 
  anova() |> 
  tidy()

model_3 <- bike_sales |> 
  aov(formula = price ~ category.secondary + frame + category.secondary:frame) |> 
  anova() |> 
  tidy()

model_3 |> 
  datatable(
    colnames = c("Term",
                 "Paramter",
                 "Sum of squares",
                 "Mean of squares",
                 "Statistic",
                 "P-value")
  ) |> 
  formatRound(
    columns = c("sumsq",
                "meansq",
                "statistic",
                "p.value"),
    digits = 3
  )

model_4 <- bike_sales |> 
  aov(formula = price ~ category.secondary*frame*bikeshop.state) |> 
  anova() |> 
  tidy()
  
  
  
