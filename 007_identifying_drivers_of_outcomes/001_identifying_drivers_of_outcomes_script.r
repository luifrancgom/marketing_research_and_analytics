# Load libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(tidymodels)
library(performance)
library(DT)

# Import data ----
# "/data/data_sets_marketing/007_data_set_chapter-7.csv"
amusement_park <- read_csv(
  file = "000_data/007_data_set_chapter-7.csv"
)

# Inspect data ----
amusement_park |> 
  glimpse()

# Transform data ----
amusement_park <- amusement_park |> 
  mutate(
    weekend = factor(x = weekend, ordered = FALSE),
    num.child = as.integer(x = num.child),
    logdist = log(x = distance, base = exp(1))
  )

# Summarize data ----
amusement_park |>
  skim()

# Visualization ----
amusement_park |> 
  ggplot() + 
  geom_histogram(
    aes(x = distance),
    color = "black"
  ) + 
  scale_x_continuous(
    transform = "log"
  )

# Correlation matrix ----
correlation_matrix <- amusement_park |> 
  select(num.child, rides:logdist) |> 
  correlate()

correlation_matrix |> 
  autoplot(
    triangular = "lower"
  )

# Linear models ----
## Visualization ----
amusement_park |> 
  ggplot(
    aes(x = rides, y = overall)
  ) + 
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "blue"
  ) +
  labs(
    x = "Satisfaction with rides",
    y = "Overall satisfaction"
  )

## Estimation ----
# overall = f(rides)
# overall = b_0 + b_1*rides + error
model1 <- lm(
  formula = overall ~ rides,
  data = amusement_park
)

model1

model1_tidy <- model1 |> 
  tidy()

model1_est_b0 <- model1_tidy$estimate[1]
model1_est_b1 <- model1_tidy$estimate[2]

model2 <- lm(
  formula = overall ~ rides + games + wait + clean,
  data = amusement_park
)

model2_tidy <- model2 |> 
  tidy()

model2_tidy$p.value

format(2.186256e-04, scientific=FALSE)

model2_tidy |> 
  datatable(
    colnames = c(
      "Term",
      "Estimation",
      "Standard error",
      "T-statistic",
      "P-value"
    )
  ) |> 
  formatRound(
    columns = c(2, 3, 4, 5)
  )

### Checking assumptions ----
check_model(model1)
check_model(model2)

### Comparing models
anova_lm <- anova(
  model1,
  model2,
  test = "F"
)

anova_lm_tidy <- anova_lm |> 
  tidy()

alpha <- 0.05

anova_lm_tidy

## Predictios
model2_tidy

((model2_tidy$estimate) * c(1, 30, 10, 57, 90)) |> 
  sum()

((model2_tidy$estimate) * c(1, 6, 8, 10, 12)) |> 
  sum()

new_data <- tibble(
  rides = c(30, 70),
  games = c(10, 80),
  wait  = c(57, 60),
  clean = c(90, 93)
)

model2_new_data_pred <- predict(
  object = model2,
  newdata = new_data
) |> 
  enframe(
    name = "observation",
    value = "overall_pred"
  ) |> 
  bind_cols(new_data)

model2_new_data_pred |> 
  datatable(
    colnames = c(
      "Observation",
      "Overall prediction",
      "Rides",
      "Games",
      "Wait",
      "Clean"
    )
  ) |> 
  formatRound(
    columns = c(2),
    digits = 2
  )
