# Load libraries ----
library(tidyverse)
library(tidymodels)

# Import data ----
segmentation <- read_csv(file = "000_data/005_data_set_chapter-5.csv")

# Count ----
segmentation |> 
  count(Segment)

segmentation |> 
  count(subscribe, ownHome) |> 
  pivot_wider(
    id_cols = subscribe,
    names_from = ownHome,
    values_from = n
  )

# Chi2 test for given probabilities ----
chi_statistic <- table(
  segmentation$Segment
) |> 
  chisq.test(
    p = c(1/4, 1/4, 1/4, 1/4)
  )

chi_statistic
chi_statistic$p.value

alpha <- 0.05
alpha2 <- 0.01

qchisq(
  p = alpha, 
  df = 3,
  lower.tail = FALSE
)

qchisq(
  p = alpha2, 
  df = 3,
  lower.tail = FALSE
)

pchisq(
  q = chi_statistic$statistic,
  df = 3,
  lower.tail = FALSE
)

segmentation |> 
  chisq_test(
    response = Segment,
    p = c(1/4, 1/4, 1/4, 1/4)
  )

# Pearson chi-squared test ----
chi_statistic_pearson <- segmentation |> 
  chisq_test(
    formula = subscribe ~ ownHome,
    correct = FALSE
  )

chi_statistic_pearson

# T test differences in means ----
segmentation |> 
  group_by(ownHome) |> 
  summarise(
    mean_income = mean(income),
    var_income  = var(income),
    n           = n()
  )

alpha <- 0.05

t_test_statistic <- segmentation |> 
  t_test(
    formula = income ~ ownHome,
    alternative = "two-sided",
    order = c("ownNo", "ownYes"),
    mu = 0,
    conf_level =  1 - alpha
  )

t_test_statistic$p_value

qt(
  p = 0.025, 
  df = t_test_statistic$t_df,
  lower.tail = FALSE
)

pt(
  q = t_test_statistic$statistic,
  df = t_test_statistic$t_df,
  lower.tail = TRUE
) * 2

# Anova ----
segmentation |> 
  group_by(Segment) |> 
  summarise(
    mean_income = mean(income),
    n = n()
  )

segmentation |>  
  summarise(
    mean_income = mean(income),
    n = n()
  )

f_statistic <- aov(
  data = segmentation,
  formula = income ~ Segment 
) |> 
anova() |> 
tidy()

alpha <- 0.05

f_statistic$p.value[1]

pf(
  q = f_statistic$statistic[1],
  df1 = f_statistic$df[1],
  df2 = f_statistic$df[2],
  lower.tail = FALSE
)

qf(
  p = alpha,
  df1 = f_statistic$df[1],
  df2 = f_statistic$df[2],
  lower.tail = FALSE
)

## Anova with more than 2 variables
### income = f(Segment, ownHome)
model_aov <- aov(
  data = segmentation,
  formula = income ~ Segment + ownHome + Segment:ownHome  
)

model_aov_tidy <- model_aov |> 
  anova() |> 
  tidy()

model_aov_tidy$p.value |> 
  format(scientific = FALSE)

model_aov_tidy |> 
  filter(alpha > p.value)