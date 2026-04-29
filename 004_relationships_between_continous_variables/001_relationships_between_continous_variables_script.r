# Load libraries ----
library(tidyverse)
library(skimr)
library(corrr)

# Import data ----
# read_csv(file = "http://goo.gl/PmPkaG")
# read_csv(file = "/data/data_sets_marketing/004_data_set_chapter-4.csv")
customer <- read_csv(file = "000_data/004_data_set_chapter-4.csv")

# Transform data ----
customer <- customer |> 
  mutate(
    cust.id = factor(x = cust.id, ordered = FALSE),
    email   = factor(x = email, ordered = FALSE),
    online.visits = as.integer(x = online.visits),
    online.trans = as.integer(x = online.trans),
    store.trans = as.integer(x = store.trans),
    sat.service = factor(x = sat.service, ordered = TRUE),
    sat.selection = factor(x = sat.selection, ordered = TRUE)
  )

# Inspect data ----
customer |> 
  glimpse()

# Summary statistics ----
customer |> 
  skim()

# Visualization ----
## online spend vs store spend ----
customer |> 
  ggplot() +
  geom_point(
    aes(
      x = store.spend, 
      y = online.spend,
      color = email
    )
  ) +
  scale_x_continuous(
    transform = "log1p",
    breaks = c(1, 2, 5, 10, 20, 50, 100, 500)
  ) +
  scale_y_continuous(
    transform = "log1p",
    breaks = c(1, 5, 50, 500)
  )

## Pearson correlation ----

cor(
  x = customer$online.spend,
  y = customer$store.spend,
  method = "pearson"
)

cor(
  x = log(customer$online.spend + 1),
  y = log(customer$store.spend + 1),
  method = "pearson"
)

## Pearson correlation matrix
correlation_matrix <- customer |> 
  select(where(is.numeric)) |> 
  correlate(
    use = "pairwise.complete.obs",
    method = "pearson",
    diagonal = NA
  )

correlation_matrix |> 
  autoplot(
   triangular = "lower" 
  )

## Categorical variables ----
customer |> 
  ggplot() +
  geom_point(
    aes(x = sat.service, y = sat.selection),
    position = position_jitter(width = 0.2, height = 0.2)
  )

count_sat_service_selection <- customer |> 
  count(
    sat.service,
    sat.selection
  )

count_sat_service_selection |> 
  ggplot() +
  geom_label(
    aes(
      x = sat.service,
      y = sat.selection,
      label = n
    )
  )
