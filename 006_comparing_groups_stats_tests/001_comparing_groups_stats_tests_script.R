# Load packages ----
library(tidyverse)
library(tidymodels)

# Import data ----
segmentation <- read_csv(
    file = "000_data/005_chapter_5.csv"
)

# Count ----
segmentation |>
    count(Segment)

segmentation |>
    count(
        subscribe,
        ownHome
    ) |>
    pivot_wider(
        names_from = ownHome,
        values_from = n
    )

# Statistical tests ----
## Chiq test proportions ----
chisq_test_prop <- segmentation |>
    chisq_test(
        response = Segment,
        p = c(1 / 4, 1 / 4, 1 / 4, 1 / 4)
    )

chisq_test_prop

alpha <- 0.05

pchisq(
    q = chisq_test_prop$statistic,
    df = chisq_test_prop$chisq_df,
    lower.tail = FALSE
)

qchisq(
    p = alpha,
    df = chisq_test_prop$chisq_df,
    lower.tail = FALSE
)

## Chiq test pearson ----
chisq_test_pearson <- segmentation |>
    chisq_test(
        formula = subscribe ~ ownHome,
        correct = FALSE
    )

chisq_test_pearson

pchisq(
    q = chisq_test_pearson$statistic,
    df = chisq_test_pearson$chisq_df,
    lower.tail = FALSE
)

chisq_test_pearson$p_value

## T-test medias ----
segmentation |>
    group_by(
        ownHome
    ) |>
    summarise(
        mean = mean(income),
        var = var(income),
        n = n()
    )

t_test_mean <- segmentation |>
    t_test(
        formula = income ~ ownHome,
        alternative = "two-sided",
        order = c("ownNo", "ownYes"),
        mu = 0,
        conf_level = 1 - alpha
    )

t_test_mean

pt(
    q = t_test_mean$statistic,
    df = t_test_mean$t_df
) *
    2

t_test_mean$p_value

## F-statistic anova ----
f_test_anova <- aov(
    data = segmentation,
    formula = income ~ Segment,
) |> 
  anova() |> 
  tidy()

alpha

f_test_anova

pf(
    q = f_test_anova$statistic[1],
    df1 = f_test_anova$df[1],
    df2 = f_test_anova$df[2],
    lower.tail = FALSE
)

f_test_anova_2 <- aov(
    data = segmentation,
    formula = income ~ Segment + ownHome,
) |> 
  anova() |> 
  tidy()

f_test_anova_3 <- aov(
    data = segmentation,
    formula = income ~ Segment + ownHome + Segment:ownHome,
) |> 
  anova() |> 
  tidy()