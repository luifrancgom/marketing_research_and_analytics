# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(tidymodels)

# Import data ----
## https://r-marketing.r-forge.r-project.org/ >
## Data >
## folder of all data files >
## rintro-chapter7.csv
# amusement_park <- read_csv("000_data/007_data_chapter7.csv")
amusement_park <- read_csv("http://goo.gl/HKnl74")

# Explore data ----
amusement_park
amusement_park |> glimpse()

# Transform data ----
amusement_park |> 
  ggplot() +
  geom_histogram(aes(x = distance),
                 color = "black")

amusement_park |> 
  ggplot() +
  geom_histogram(aes(x = distance),
                 color = "black") +
  scale_x_continuous(transform = "log")

amusement_park <- amusement_park |> 
  mutate(weekend = factor(x = weekend,
                          ordered = FALSE),
         num.child = as.integer(x = num.child),
         logdist = log(x = distance, 
                       base = exp(1)))

amusement_park

# Summary statistics ----
amusement_park |> 
  skim()

## Inspecting correlations ----
correlation_matrix <- amusement_park |> 
  select(num.child, rides:logdist) |> 
  correlate()

## Visualizing correlations ----
correlation_matrix |> 
  autoplot(triangular = "lower")

# Linear models ----

## Visualizing linear models
amusement_park |>
  ggplot(aes(x = rides, y = overall)) + 
  geom_point() +
  # https://ggplot2.tidyverse.org/reference/geom_smooth.html >
  # Examples
  geom_smooth(method = "lm",
              se = FALSE)

## Estimating linear models ----
model1 <- lm(formula = overall ~ rides,
             data = amusement_park)

model1

### Inspecting the model ----
str(model1)
glimpse(model1)

summary(model1)
tidy(model1)

## Making predictions 
model1$coefficients

### Assume we have a client that scores
### the rides with a value of 95
-94.962246 + 1.703285*95

summary(model1) |> 
  str()


## Confidence intervals ----
summary(model1)$coefficients[,2]
confint(model1, level = 0.95)

tidy(model1, conf.int = 0.95)

## Checking assumptions ----
par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))
