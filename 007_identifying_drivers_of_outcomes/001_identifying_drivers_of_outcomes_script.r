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
