# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(tidymodels)
library(performance)

# Import ----
# amusement_park <- read_csv(file = "http://goo.gl/HKnl74")
amusement_park <- read_csv(file = "000_data/007_chapter_7.csv")

# Inspect ----
amusement_park |> 
  glimpse()

# Transform ----
amusement_park <- amusement_park |> 
  mutate(
    weekend = factor(x = weekend,
                     ordered = FALSE),
    num.child = as.integer(x = num.child),
    logdist = log(x = distance, base = exp(1)),
    has.child = factor(x = num.child > 0, 
                       labels = c("no", 
                                  "yes"), 
                       ordered = FALSE)
  )
  
# Summarize ----
amusement_park |> 
  skim()

# Correlation matrix ----
correlation_matrix <- amusement_park |> 
  select(where(is.numeric)) |> 
  correlate(
    method = "pearson", 
    use = "pairwise.complete.obs" 
  )

correlation_matrix |> 
  autoplot(triangular = "lower")

# Visualization ----
amusement_park |> 
  ggplot(aes(x = rides, 
             y = overall)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    # Find code colors
    # https://www.color-hex.com/
    color = "red",
    se = FALSE
  ) +
  labs(
    x = "Satisfaction with rides [0, 100]",
    y = "Overall satisfaction [0, 100]"
  )
  
# Modeling ----
## Model 1 ----
amusement_park |> 
  select(rides, 
         overall)

model_1 <- lm(
  formula = overall ~ rides,
  data = amusement_park
)

model_1_tidy <- model_1 |> 
  tidy()

model_1 |> 
  augment()

### Manual estimation ----
model_1_tidy$estimate

-94.962246 + 1.703285*87
47 - (-94.962246 + 1.703285*87)

### Checking assumptions ----
check_model(x = model_1)

# https://github.com/easystats/performance/issues/638
check_model(x = model_1, 
            panel = FALSE) |> 
  plot()

## Model 2 ----
model_2 <- lm(
  formula = overall ~ rides + games + wait + clean,
  data = amusement_park
)

model_2_tidy <- model_2 |> 
  tidy()

model_2_tidy

### Checking assumptions ----
check_model(x = model_2)

check_model(x = model_2,
            panel = FALSE) |> 
  plot()

### Inference ----
alpha <- 0.05
model_2_tidy |> 
  filter(alpha > p.value)

model_2_tidy$std.error

# To obtain the degrees of freedom
## df.residual
model_2 |> 
  glance() |> 
  glimpse()

## Model 3 ----
model_3 <- lm( 
  formula = overall ~ rides + games + wait + clean + weekend + logdist + num.child,
  data = amusement_park
)

model_3 |> 
  tidy() |> 
  filter(alpha > p.value)

## Model 4 ----
model_4 <- lm(
  formula = overall ~ rides + games + wait + clean + logdist + has.child,
  data = amusement_park
)

model_4_tidy <- model_4 |> 
  tidy()

## Model 5 ----
model_5 <- lm(
  formula = overall ~ rides + games + wait + clean + logdist + has.child + weekend,
  data = amusement_park
)

model_5 |> 
  tidy() |> 
  filter(alpha > p.value)

## Comparing models ----
anova_lm <- anova(
  model_1,
  model_2,
  model_4,
  model_5,
  test = "F"
)

anova_lm |> 
  tidy() |> 
  filter(alpha > p.value)

# Prediction ----
new_data <- tibble(
  rides = c(30, 70),
  games = c(10, 80),
  wait =  c(57, 60),
  clean = c(90, 93),
  logdist = c(log(x = 10, base = exp(1)), 
              log(x = 100, base = exp(1))),
  has.child = c("yes", "no")
)

mode_4_pred <- predict(
  object = model_4,
  newdata = new_data
) |> 
enframe(
  name = "observation", 
  value = "overall_pred" 
) |> 
  bind_cols(
    new_data
  )
  