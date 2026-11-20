# Load packages ----
library(tidyverse)
library(skimr)
library(corrr)
library(performance)
library(tidymodels)
library(coefplot)

# Import data ----
amusement_park <- read_csv(
    file = "000_data/007_chapter_7.csv"
)

# Transform ----
amusement_park <- amusement_park |>
    mutate(
        weekend = factor(
            x = weekend,
            ordered = FALSE
        ),
        num.child = as.integer(num.child),
        logdist = log(
            x = distance,
            base = exp(1)
        )
    )

amusement_park

# Inspect ----
amusement_park |>
    glimpse()

# Summary statistics ----
amusement_park |>
    skim()

# Correlation matrix ----
correlation_matrix <- amusement_park |>
    select(
        num.child,
        rides:logdist
    ) |> 
    correlate()

correlation_matrix |> 
    autoplot(
        triangular = "lower"
    )

# Visualization ----
amusement_park |> 
    ggplot(
        aes(
            x = rides,
            y = overall
        )
    ) +
    geom_point() +
    geom_smooth(
        method = "lm"
    )

# Linear models ----
model1 <- lm(
    data = amusement_park,
    formula = overall ~ rides
)

model1$coefficients

model1_b0 <- model1$coefficients[1]
model1_b1 <- model1$coefficients[2]

model1_b0 + model1_b1*95

model2 <- lm(
    data = amusement_park,
    formula = overall ~ rides + games + wait + clean
)

alpha <- 0.05

model2 |> 
    tidy(
       conf.int = TRUE,
       conf.level = 1 - alpha  
    )

model3 <- lm(
    data = amusement_park,
    formula = overall ~ rides + games + wait + clean + has.child
)

model3 |> 
    tidy()

## Checking assumptions ----
check_model(
    model = model1
)

## Statistical tests ----
coefplot(
    model = model2,
    intercept = FALSE,
    ylab = "Rating of Feature",
    xlab = "Association with Overall Satisfaction",
    lwdOuter = 1.5
)

## With categorical variables
amusement_park <- amusement_park |> 
    mutate(
        has.child = factor(
            x = num.child > 0,
            labels = c("no", "yes")
        )
    )

amusement_park |> 
    glimpse()

# Predictions ----
new_data <- tibble(
  rides = c(30, 70),
  games = c(10, 80),
  wait = c(57, 60),
  clean = c(90, 93),
  has.child = c("yes", "no")
)

new_data

predict(
    object = model3,
    newdata = new_data
) |> 
 enframe(
    name = "observation",
    value = "overall_pred"
 ) |> 
  bind_cols(
    new_data
  )
