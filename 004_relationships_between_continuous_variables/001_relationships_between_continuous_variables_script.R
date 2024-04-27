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

# Transform data 2 -----
customer |>
  mutate(log_store.spend = log(store.spend + 1)) |> 
  ggplot() +
  geom_histogram(aes(x = log_store.spend),
                 color = "black",
                 bins = 30)

customer |>
  ggplot() +
  geom_histogram(aes(x = distance.to.store),
                 color = "black",
                 bins = 30) +
  scale_x_continuous(transform = "log")

customer |> 
  ggplot() +
  geom_point(aes(x = distance.to.store,
                 y = store.spend))

## Improving correlation ----

cor(x = customer$store.spend, 
    y = customer$distance.to.store)

cor(x = customer$store.spend,
    y = 1 / customer$distance.to.store)

cor(x = customer$store.spend,
    y = 1 / sqrt(customer$distance.to.store))

# ggplot() +
#  geom_function(fun = log, 
#                xlim = c(-5, 5))

cor(x = log(customer$store.spend + 1),
    y = 1 / sqrt(customer$distance.to.store))

customer |> 
  mutate(distance.to.store_trans1 = 1 / distance.to.store) |> 
  ggplot() +
  geom_point(aes(x = distance.to.store_trans1, 
                 y = store.spend))

customer |> 
  mutate(distance.to.store_trans2 = 1 / sqrt(distance.to.store)) |> 
  ggplot() +
  geom_point(aes(x = distance.to.store_trans2, 
                 y = store.spend))

# Categorical ordinal variables
## You can do that but it is not
## conceptually correct
# cor(x = customer$sat.selection,
#    y = customer$sat.service)
customer |> 
  ggplot() +
  geom_point(aes(x = sat.service, y = sat.selection))

customer |> 
  ggplot() +
  geom_point(aes(x = sat.service, y = sat.selection),
             position = position_jitter(width = 0.2,
                                        height = 0.2))

# Alternative 1
## Using numbers
customer |> 
  count(sat.service, sat.selection) |> 
  ggplot() +
  ## Check out 
  ## https://ggplot2.tidyverse.org/reference/geom_text.html
  geom_text(aes(x = sat.service, y = sat.selection,
                label = n))

# Alternative 2
## Using colors
customer |> 
  count(sat.service, sat.selection) |> 
  ggplot() +
  ## Check out 
  ## https://ggplot2.tidyverse.org/reference/geom_tile.html
  geom_tile(aes(x = sat.service, y = sat.selection,
                fill = n),
            color = "black")  

## Improving colors
customer |> 
  count(sat.service, sat.selection) |> 
  ggplot() +
  geom_tile(aes(x = sat.service, y = sat.selection,
                fill = n),
            color = "black") +
  ## Check out 
  ## https://ggplot2.tidyverse.org/reference/scale_gradient.html
  scale_fill_gradient2()