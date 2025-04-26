# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)

# Import ----
# customer <- read_csv(file = "http://goo.gl/PmPkaG")
customer <- read_csv(file = "000_data/004_data_chapter4.csv")
customer

# Inspect ----
customer |> 
  glimpse()

# Transform ----
customer <- customer |> 
  mutate(cust.id = factor(x = cust.id, ordered = FALSE),
         email = factor(x = email, ordered = FALSE),
         online.visits = as.integer(x = online.visits),
         online.trans = as.integer(x = online.trans),
         store.trans = as.integer(x = store.trans),
         sat.service = factor(x = sat.service, 
                              ordered = TRUE),
         sat.selection = factor(x = sat.selection, 
                                ordered = TRUE))

customer

# Summarize ----
customer |> 
  select(-c(cust.id)) |> 
  skim()

# Visualization ----
## Spending ----
customer |> 
  ggplot() +
  geom_point(aes(x = online.spend, 
                 y = store.spend))

customer |> 
  ggplot() + 
  geom_point(aes(x = online.spend,
                 y = store.spend,
                 color = email)) +
  labs(x = "Amount spend online yearly (US dollars)",
       y = "Amount spend in the physical store (US dollars)",
       color = "Email",
       title = "Spend online vs physical store by customer")

customer |> 
  ggplot() + 
  geom_point(aes(x = online.spend,
                 y = store.spend,
                 color = email)) +
  scale_color_manual(values = c("#8e0c14", "#2c98af"),
                     labels = c("No", "Yes")) +
  labs(x = "Amount spend online yearly (US dollars)",
       y = "Amount spend in the physical store (US dollars)",
       color = "Email",
       title = "Spend online vs physical store by customer")

# Satisfaction ----
customer |> 
  ggplot() +
  geom_point(aes(x = sat.service, y = sat.selection),
             position = position_jitter(width = 0.2,
                                        height = 0.2))

customer |> 
  count(sat.service, sat.selection) |> 
  ggplot() +
  geom_label(aes(x = sat.service,
                 y = sat.selection,
                 label = n))
  
# Transforming the axes
customer |> 
  ggplot() +
  geom_point(aes(x = online.spend, 
                 y = store.spend,
                 color = email)) +
  # https://www.color-hex.com/color-palettes/
  scale_color_manual(values = c("#8e0c14", "#2c98af"),
                     labels = c("No", "Yes")) +
  # https://en.wikipedia.org/wiki/Natural_logarithm
  ## Understanding logarithms
  ### log1p: online.spend (take 0 values)
  scale_x_continuous(transform = "log1p", 
                     breaks = c(1, 2, 5, 10, 
                                20, 50, 100, 500,
                                5000)) +
  scale_y_continuous(transform = "log1p",
                     breaks = c(1, 5, 50, 500)) +
  labs(x = "Online spending (US Dollars)",
       y = "Store physical spending (US Dollars)",
       color = "Email",
       title = "Online vs store physical spending")

# Summary statistic ----
## Relation between variables ----
x <- 1:10
x
y <- 3*x
y
x_2 <- x^2 

tibble(x = x,
       y = y) |> 
  ggplot() +
  geom_point(aes(x = x, y = y))

w <- -100:100
w_2 <- w^2

tibble(x = w,
       y = w_2) |> 
  ggplot() +
  geom_point(aes(x = x, y = y))

## Pearson correlation
# https://en.wikipedia.org/wiki/Correlation
cor(x, y)
cor(w, w_2)
cor(w_2, w)

customer |> 
  summarise(cor = cor(store.spend, online.spend))

## Matrix correlation ----
customer |> glimpse()

customer |> 
  select(where(is.factor))

correlation_matrix <- customer |> 
  select(where(is.numeric)) |> 
  correlate(use = "pairwise.complete.obs", 
            method = "pearson")

correlation_matrix |> 
  autoplot(triangular = "lower")


