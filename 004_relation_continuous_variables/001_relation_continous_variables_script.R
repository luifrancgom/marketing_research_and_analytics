# Load packages ----
library(tidyverse)
library(skimr)
library(corrr)

# Import data ----
customer <- read_csv(
    file = "000_data/004_chapter_4.csv"
)

customer

# Inspect ----
customer |>
    glimpse()

# Transform ----
customer <- customer |>
    mutate(
        cust.id = factor(x = cust.id, ordered = FALSE),
        email = factor(x = email, ordered = FALSE),
        online.visits = as.integer(x = online.visits),
        online.trans = as.integer(x = online.trans),
        store.trans = as.integer(x = store.trans),
        sat.service = factor(x = sat.service, ordered = TRUE),
        sat.selection = factor(x = sat.selection, ordered = TRUE)
    )

# Summarize ----
customer |>
    skim()

# Visualization
customer |>
    ggplot() +
    geom_point(
        aes(
            x = store.spend,
            y = online.spend
        )
    ) +
    scale_x_continuous(
        transform = "log1p"
    ) +
    scale_y_continuous(
        transform = "log1p"
    )

# Person correlation ----
age_mean <- mean(customer$age)
credit_score_mean <- mean(customer$credit.score)
numerator <- sum(
  (customer$age - age_mean) * (customer$credit.score - credit_score_mean)
)
denominator <- sqrt(sum((customer$age - age_mean)^2)) *
  sqrt(sum(((customer$credit.score - credit_score_mean)^2)))
pearson_corr <- numerator / denominator
pearson_corr

cor(
    x = customer$age,
    y = customer$credit.score
)

cor(
    x = customer$credit.score,
    y = customer$age
)

correlation_matrix <- customer |> 
    select(
        where(is.numeric)
    ) |> 
    correlate(
        use = "pairwise.complete.obs",
        method = "pearson",
        diagonal = NA
    )

correlation_matrix |> 
    autoplot(
        triangular = "lower"
    )

# Transformation variables ----
cor(
  x = customer$store.spend,
  y = customer$distance.to.store
)

cor(
  x = customer$store.spend,
  y = 1 / sqrt(customer$distance.to.store)
)

customer |>
  mutate(
    distance.to.store_trans = 1 / sqrt(distance.to.store)
  ) |>
  ggplot() +
  geom_point(
    aes(
      x = distance.to.store_trans,
      y = store.spend
    )
  )

# Association categorical variables ----
customer |> 
    ggplot() +
    geom_point(
        aes(
            x = sat.service,
            y = sat.selection
        ),
        position = position_jitter(
            width = 0.2,
            height = 0.2
        )
    )

customer |>
    count(
        sat.service,
        sat.selection
    ) |>
    ggplot() +
    geom_text(
        aes(
            x = sat.service,
            y = sat.selection,
            label = n
        )
    )    
