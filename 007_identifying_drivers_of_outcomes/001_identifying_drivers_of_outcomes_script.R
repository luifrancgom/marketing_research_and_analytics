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
    logdist = log(x = distance, base = exp(1))
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

