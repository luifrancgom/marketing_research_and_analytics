# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(tidymodels)
library(DT)
library(coefplot)

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

# Modeling ----
## Model 1 ----
          # lm: linear model
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

## Error ----
### 2 client
overall_2 <- 65
overall_est_2 <- coef$estimate[1] + coef$estimate[2]*87
error_est_2 <- overall_2 - overall_est_2


## Verifying the assumptions of the model1 ----
par(mfrow = c(2,2))
plot(model1)
par(mfrow = c(1,1))

### Identifying influential observations ----
amusement_park |> 
  slice(c(57, 129, 295))

## Model 2 ----
model2 <- lm(formula = overall ~ rides + games + wait + clean,
             data = amusement_park)

model2

model2_tidy <- model2 |> 
  tidy()

model2_tidy

### Reading the result ----
alpha <- 0.05
model2_tidy |> 
  filter(p.value < 0.05)

#### beta_1 parameter (rides)
beta_1_est <- model2_tidy$estimate[2]
beta_1_est_sd <- model2_tidy$std.error[2]
t_statistic_beta_1_est <- (beta_1_est - 0) / beta_1_est_sd
t_statistic_beta_1_est
model2_tidy$statistic[2]
# Working with T-distribution: ?rt
pt(q = model2_tidy$statistic[2], 
   df = 500 - 5,
   lower.tail = FALSE)*2
model2_tidy$p.value[2]

## Verifying the assumptions of the model2 ----
par(mfrow = c(2,2))
plot(model2)
par(mfrow=c(1,1))

amusement_park |> 
  slice(c(59, 441, 475))

## Visualizing the T-test
coefplot(model = model2,
         intercept = FALSE,
         lwdOuter = 1.5)

## Categorical variables ----
model3 <- lm(formula = overall ~ rides + games + wait + clean + weekend,
             data = amusement_park)

model3

model3_tidy <- model3 |> 
  tidy()

model3_tidy

alpha <- 0.05

model3_tidy |> 
  filter(p.value < alpha)

### Understanding categorical values linear models ----
model3 |> 
  # How data is transform to apply a linear model
  model.matrix() |> 
  # Printing in a pretty way the data
  as_tibble()

amusement_park |> 
  select(rides, games, wait, clean, weekend)

## Visualizing model3 ----
coefplot(model = model3,
         intercept = FALSE)

## Prediction
model2_tidy

new_data <- tibble(rides = c(30, 70),
                   games = c(10, 80),
                   wait  = c(57, 60),
                   clean = c(90, 93))

new_data

prediction <- predict(object = model2,
        newdata = new_data) |> 
  enframe(name = "observation",
          value = "overall_predict") |> 
  bind_cols(new_data)

prediction
