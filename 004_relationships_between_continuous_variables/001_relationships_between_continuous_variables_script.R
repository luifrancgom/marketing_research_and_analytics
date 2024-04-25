# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)

# Import data ----
# customer <- read_csv(file = "http://goo.gl/PmPkaG")
customer <- read_csv(file = "000_data/004_data_chapter4.csv")

# Explore data ----
customer |> glimpse()

# Transform data ----
customer <- customer |>
  mutate(cust.id = factor(x = cust.id, ordered = FALSE),
         email = factor(x = email, ordered = FALSE),
         online.visits = as.integer(x = online.visits),
         online.trans = as.integer(x = online.trans),
         store.trans = as.integer(x = store.trans),
         sat.service = factor(x = sat.service, ordered = TRUE),
         sat.selection = factor(x = sat.selection, ordered = TRUE))

# Describing data ----
customer |> skim()

# Visualization -----

## Spending: online vs physical store ----
ggplot(customer) +
  geom_histogram(aes(x = store.spend),
                 color = "black")

ggplot(customer) +
  geom_point(aes(x = store.spend, y = online.spend,
                 color = email)) +
                     # See hex codes for colors
  scale_color_manual(values = c("red",  "#33ffe9")) +
  scale_x_continuous(breaks = c(1, 2, 5, 10, 20, 50, 100, 500),
                     transform = "log1p") +
  scale_y_continuous(breaks = c(1, 5, 50, 500),
                     transform = "log1p") +
  labs(x = "Spending in the physical store (dollars)",
       y = "Spending online (dollars)",
       title = "Spending in the physical store vs online",
       color = "Email")

## Age vs the credit score
ggplot(customer) +
  geom_point(aes(x = age, y = credit.score))

# Pearson correlation ----
cor(x = customer$age, y = customer$credit.score,
    method = "pearson")

## Correlation between variables ----
correlation_matrix <- customer |> 
  select(where(is.numeric)) |> 
  correlate(use = "pairwise.complete.obs")

correlation_matrix |> 
  autoplot(triangular = "lower")