# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(tidymodels)
library(coefplot)

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

## Multiple linear regression ----
model2 <- lm(formula =  overall ~ rides + games + wait + clean,
             data = amusement_park)

model2

model3 <- lm(formula = overall ~ rides + games + wait + clean + weekend + logdist + num.child,
             data = amusement_park)

model3

### Checking the assumptions of the model ----
par(mfrow = c(2,2))
plot(model2)
par(mfrow = c(1,1))

### Inspecting the model ----
summary(model2)

model2 |> 
  vcov() |> 
  diag() |> 
  sqrt()

qt(p = 0.05/2, df = 495, lower.tail = TRUE)

### Confidence intervals ----
tidy(model2, conf.int = 0.95)

coefplot(model = model2,
         intercept = FALSE,
         lwdOuter = 1.5)

### Comparing models ----
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
summary(model3)$adj.r.squared

augment(model1)
augment(model2)

47 - 53.2

anova_lm <- anova(model1,
                  model2,
                  model3,
                  test = "F")

anova_lm

### Predictions ----
model2$coefficients

#### Manual ----
(-131.4091939 +
    # rides
    0.5290780*70 +
    # games
    0.1533361*60 +
    # wait
    0.5533264*40 +
    #clean
    0.9842126*90)

#### In a more automatic way
new_data <- tibble(rides = c(30, 70, 100),
                   games = c(10, 80, 100),
                   wait = c(57, 60, 100),
                   clean = c(90, 93, 100),
                   weekend = factor(c("yes", "no", "yes"), 
                                       ordered = FALSE),
                   logdist = c(5.6, 7.1, 1.3),
                   num.child = c(0, 2, 4))

new_data

predict(object = model3,
        newdata = new_data) |> 
  enframe(name = "observation", 
          value = "pred_overall") |> 
  bind_cols(new_data)
