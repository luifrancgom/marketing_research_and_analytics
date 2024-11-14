# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(tidymodels)

# Import data ----
amusement_park <- read_csv(file = "000_data/007_data_chapter7.csv")
amusement_park

# Inspect data ----
amusement_park |> glimpse()

# Transform data ----
amusement_park |> 
  count(weekend)

amusement_park <- amusement_park |> 
  mutate(weekend   = factor(x = weekend, ordered = FALSE),
         num.child = as.integer(x = num.child),
         logdist   = log(x = distance))

# Summarize data ----
amusement_park |> 
  skim()

# Correlation ----
correlation_matrix <- amusement_park |> 
  select(where(fn = is.numeric)) |> 
  correlate()

correlation_matrix |> 
  autoplot(triangular = "lower")

# Data visualization ----
amusement_park |> 
  ggplot(aes(x = rides, y = overall)) +
  geom_point() +
              # lm: linear model
  geom_smooth(method = "lm", 
              se = FALSE,
              color = "red") + 
  labs(x = "Satisfaction with rides",
       y = "Overall satisfaction",
       title = "Linear model between satisfaction with rides and overall satisfaction") 

# Modeling
model1 <- lm(formula = overall ~ rides, 
             data = amusement_park)

model1_tidy <- model1 |> tidy()

coef <- model1_tidy |> 
  select(estimate)

coef$estimate[1]
coef$estimate[2]

## Forecast ----
coef$estimate[1] + coef$estimate[2]*95
coef$estimate[1] + coef$estimate[2]*50


