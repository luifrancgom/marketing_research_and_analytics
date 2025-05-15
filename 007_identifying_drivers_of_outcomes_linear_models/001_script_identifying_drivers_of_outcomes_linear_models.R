# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(DT)
library(tidymodels)

# Import data ----
# amusement_park <- read_csv("http://goo.gl/HKnl74")
amusement_park <- read_csv(file = "000_data/007_data_chapter7.csv")
amusement_park

# Inspect ----
amusement_park |> 
  glimpse()

# Transform ----
amusement_park <- amusement_park |> 
  mutate(weekend = factor(x = weekend, 
                          ordered = FALSE),
         num.child = as.integer(x = num.child)) |> 
  mutate(logdist = log(x = distance)) |> 
  mutate(have_child = num.child > 0) |> 
  mutate(have_child = factor(x = have_child, 
                             labels = c("No", "Yes"),
                             ordered = FALSE))

## Parentheses
amusement_park |> 
  ggplot() +
  geom_histogram(aes(x = distance),
                 color = "black")

amusement_park |> 
  ggplot() +
  geom_histogram(aes(x = logdist),
                 color = "black")
  
# Summarize ----
amusement_park |> 
  skim()

## Correlation ----
correlation_matrix <- amusement_park |> 
  select(where(is.numeric), -c(distance)) |> 
  correlate(method = "pearson", 
            use = "pairwise.complete.obs")

correlation_matrix

correlation_matrix |> 
  datatable()

correlation_matrix |> 
  autoplot(triangular = "lower")

# Modeling ----
## Linear regression ----
amusement_park |> 
  ggplot(aes(x = rides,
             y = overall)) +
  geom_point() +
                       # lm
                       ## l: linear
                       ## m: model
  geom_smooth(method = "lm",
              se = FALSE,
              color = "red") +
  labs(x = "Satisfaction with rides",
       y = "Overall satisfaction")

alpha <- 0.05

### Model 1 ----
model1 <- lm(formula = overall ~ rides,
             data = amusement_park)

model1_tidy <- model1 |> 
  tidy()

model1 |> 
  glance()

(1.703285 - 0) / 0.1055462

?pt

pt(q = 16.13783, df = 498, lower.tail = FALSE)*2

model1_augment <- model1 |> 
  augment()

model1_augment

(-94.962246 + 1.703285*87)

47 - (-94.962246 + 1.703285*87)

#### Verifying the assumptions of the model ----
par(mfrow = c(2,2))
plot(model1)
par(mfrow = c(1,1))

#### Predictions ----
new_data_model1 <- tibble(rides = c(54, 80))

predict(object = model1,
        newdata = new_data_model1)


### Model 2 ----
model2 <- lm(formula = overall ~ rides + games + wait + clean,
             data = amusement_park)

mode2_tidy <- model2 |> 
  tidy() |> 
  mutate(decision_rule = alpha > p.value)

#### Predictions model 2 ----
new_data_model2 <- tibble(rides = c(30, 70),
                          games = c(10, 80),
                          wait  = c(57, 60),
                          clean = c(90, 93))

predict(object = model2,
        newdata = new_data_model2) |> 
  enframe(name = "observation",
          value = "pred_overall") |> 
  bind_cols(new_data_model2)


-131.4091939 + 0.5290780*30 + 0.1533361*10 + 0.5533264*57 + 0.9842126*90

### Model 3 ----
model3 <- lm(formula = overall ~ rides + games + wait + clean + have_child,
             data = amusement_park)

model3 |> 
  tidy() |> 
  mutate(decision_rule = alpha > p.value)

new_data_model3 <- tibble(rides = c(30, 70),
                          games = c(10, 80),
                          wait  = c(57, 60),
                          clean = c(90, 93),
                          have_child = c("Yes", "No"))

predict(object = model3,
        new_data_model3) |> 
  enframe(name = "observation",
          value = "pred_overall") |> 
  bind_cols(new_data_model3)
