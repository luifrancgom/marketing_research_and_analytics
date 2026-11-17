# Load packages
library(sweep)
library(tidyverse)
library(skimr)
library(scales)
library(DT)
library(tidymodels)

# Import data ----
bike_sales <- bike_sales |>
    mutate(
        order.id = factor(
            x = order.id,
            ordered = TRUE
        ),
        order.line = factor(
            x = order.line,
            ordered = TRUE
        ),
        customer.id = factor(
            x = customer.id,
            ordered = FALSE
        ),
        product.id = factor(
            x = product.id,
            ordered = FALSE
        )
    )

# Inspect data ----
bike_sales |>
    glimpse()

# Summarize ----
bike_sales |>
    skim()

# Visualization ----
## Correlation between continous variables ----
bike_sales |>
    ggplot() +
    geom_point(
        aes(
            x = price,
            y = quantity,
            color = frame
        )
    ) +
    labs(
        x = "Price (USD Dollars/frame)",
        y = "Quantity (Units)",
        color = "Frame"
    )

bike_sales |>
    ggplot() +
    geom_point(
        aes(
            x = price,
            y = quantity,
            color = frame
        )
    ) +
    scale_x_continuous(
        transform = "log",
        labels = label_currency(accuracy = 1)
    ) +
    scale_y_continuous(
        transform = "log",
        labels = label_currency(accuracy = 1)
    ) +
    labs(
        x = "Price (USD Dollars/frame - Logarithmic scale)",
        y = "Quantity (Units  - Logarithmic scale)",
        color = "Frame"
    )

### Correlation ----
cor_price_quantity <- cor(
    x = bike_sales$price,
    y = bike_sales$quantity
)

cor_price_quantity |>
    format(scientific = FALSE)

cor_log_price_log_quantity <- cor(
    x = log(bike_sales$price),
    y = log(bike_sales$quantity)
)

cor_log_price_log_quantity |>
    format(scientific = FALSE)

## Association between categorical variables ----
bike_sales |>
    ggplot() +
    geom_point(
        aes(
            x = frame,
            y = category.secondary
        ),
        position = position_jitter(
            width = 0.2,
            height = 0.2
        )
    ) +
    labs(
        x = "Frame",
        y = "Category secondary"
    )

bike_sales |>
    count(
        frame,
        category.secondary
    ) |>
    ggplot() +
    geom_text(
        aes(
            x = frame,
            y = category.secondary,
            label = n
        )
    )

# Descriptive statistics by group ----
## Category secondary and frame statistics ----
mean_median_sd_revenue_by_cat2_frame <- bike_sales |>
    group_by(
        category.secondary,
        frame
    ) |>
    summarise(
        mean_revenue = mean(price.ext),
        median_revenue = median(price.ext),
        sd_revenue = sd(price.ext)
    ) |>
    ungroup()

mean_median_sd_revenue_by_cat2_frame |>
    datatable(
        colnames = c(
            "Category secondary",
            "Frames",
            "Mean",
            "Median",
            "Standard deviation"
        )
    ) |>
    formatRound(
        columns = c(
            3,
            5
        ),
        digits = 2
    )

mean_median_sd_revenue_by_bikeshop <- bike_sales |>
    group_by(
        bikeshop.name
    ) |>
    summarise(
        mean_revenue = mean(price.ext),
        median_revenue = median(price.ext),
        sd = sd(price.ext)
    )

mean_median_sd_revenue_by_bikeshop |>
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

## Category secondary and frame pct revenue ----
pct_revenue_by_cat2_frame <- bike_sales |>
    group_by(
        category.secondary,
        frame
    ) |>
    summarise(
        total_revenue = sum(price.ext)
    ) |>
    ungroup() |>
    mutate(
        total_revenue_pct = (total_revenue / sum(total_revenue)) * 100
    ) |>
    arrange(
        desc(total_revenue_pct)
    )

pct_revenue_by_cat2_frame |>
    datatable(
        colnames = c(
            "Category secondary",
            "Frame",
            "Total revenue",
            "Percentage"
        )
    ) |>
    formatRound(
        columns = 4,
        digits = 2
    )

pct_revenue_by_bikeshop <- bike_sales |>
    group_by(
        bikeshop.name
    ) |>
    summarise(
        total_revenue = sum(price.ext)
    ) |>
    mutate(
        pct_revenue = (total_revenue / sum(total_revenue)) * 100
    ) |>
    arrange(
        desc(pct_revenue)
    )

pct_revenue_by_bikeshop |>
    datatable(
        colnames = c(
            "Bikeshop",
            "Total revenue",
            "Percentage"
        )
    ) |>
    formatRound(
        columns = 3,
        digits = 2
    )

# Statisticial tests by group ----
## Chisq2 frame cat2 ----
table_cat2_frame <- bike_sales |>
    group_by(
        category.secondary,
        frame
    ) |>
    summarise(
        total_revenue = sum(price.ext)
    ) |>
    ungroup() |>
    mutate(
        total_revenue_pct = (total_revenue / sum(total_revenue) * 100)
    ) |>
    arrange(
        desc(total_revenue_pct)
    )

table_models <- table_cat2_frame |>
    mutate(
        model = str_c(
            category.secondary,
            frame,
            sep = " "
        )
    ) |>
    select(
        model,
        total_revenue
    )

deframe_table_models <- table_models |>
    deframe() |>
    chisq.test(
        p = rep.int(x = 1 / 13, times = 13)
    )

deframe_table_models |>
    tidy() |>
    datatable(
        colnames = c(
            "Statistics",
            "P-value",
            "Degrees of freedom",
            "Method"
        )
    ) |>
    formatRound(
        columns = 1,
        digits = 2
    )

## Chisq2 bikeshops ----
deframe_table_models2 <- bike_sales |>
    group_by(
        bikeshop.name
    ) |>
    summarise(
        total_revenue = sum(price.ext)
    ) |>
    mutate(
        total_revenue_pct = (total_revenue / sum(total_revenue)) * 100
    ) |>
    arrange(
        desc(total_revenue_pct)
    ) |>
    select(
        bikeshop.name,
        total_revenue
    ) |>
    deframe()

deframe_table_models2 |>
    chisq.test(
        p = rep.int(
            x = 1 / 30,
            times = 30
        )
    ) |>
    tidy() |>
    datatable(
        colnames = c(
            "Statistic",
            "P-value",
            "Degrees of freedom",
            "Method"
        )
    ) |>
    formatRound(
        columns = 1,
        digits = 2
    )

## T-test revenue mean by frame ----
alpha <- 0.05

bike_sales |>
    t_test(
        formula = price.ext ~ frame,
        alternative = "two-sided",
        order = c("Aluminum", "Carbon"),
        mu = 0,
        conf_level = 1 - alpha
    ) |>
    datatable(
        colnames = c(
            "Statistic",
            "Degrees of freedom",
            "P-value",
            "Method",
            "Estimate",
            "Lower interval value",
            "Upper interval value"
        )
    ) |>
    formatRound(
        columns = c(1, 2, 3, 5, 6, 7)
    )

## Anova ----
model1 <- aov(
    data = bike_sales,
    formula = price ~ category.secondary
) |>
    anova() |>
    tidy()

model1

model2 <- aov(
    data = bike_sales,
    formula = price ~ category.secondary + frame
) |>
    anova() |>
    tidy()

model2

model3 <- aov(
    data = bike_sales,
    formula = price ~ category.secondary + frame + category.secondary:frame
) |>
    anova() |>
    tidy()

model3 |>
    datatable(
      colnames = c(
        "Term",
        "Degrees of freedom",
        "Sum of squares",
        "Mean of squares",
        "Statistic",
        "P-value"
      )
    ) |> 
    formatRound(
      columns = c(3, 4, 5, 6)
    )


# model4 <- aov(
#   data = bike_sales,
#   formula = price ~ category.secondary*frame*bikeshop.state
# ) |>
# anova() |>
# tidy()
