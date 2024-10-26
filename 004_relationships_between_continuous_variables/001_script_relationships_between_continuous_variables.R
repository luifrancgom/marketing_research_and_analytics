# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)

# Import data ----
customer <- read_csv(file = "000_data/004_data_chapter4.csv")
customer

# Inspect data ----
customer |> glimpse()

# Transform data ----
customer <- customer |>
  mutate(cust.id = factor(x = cust.id,
                          ordered = FALSE),
         email = factor(x = email,
                        ordered = FALSE),
         online.visits = as.integer(x = online.visits),
         online.trans = as.integer(x = online.trans),
         store.trans = as.integer(x = store.trans),
         sat.service = factor(x = sat.service,
                              ordered = TRUE),
         sat.selection = factor(x = sat.selection,
                                ordered = TRUE))

# Summarize data ----
customer |> skim()

# Visualize data ----
## Physical stores vs online: spending ----
customer |> 
  ggplot() +
  geom_point(aes(x = store.spend,
                 y = online.spend,
                 color = email)) +
  scale_color_manual(values = c("#ea5545",
                                "#b33dc6"), 
                     labels = c("Email in file: no",
                                "Email in file: yes")) +
  scale_x_continuous(transform = "log1p", 
                     breaks = c(1, 2, 5, 10, 20, 50, 100, 500)) +
  scale_y_continuous(transform = "log1p",
                     breaks = c(1, 5, 50, 500, 5000)) +
  labs(x = "Sales in the physical store (US dollars)",
       y = "Sales online store (US dollars)",
       title = "Yearly sales: physical store vs online",
       color = "Email")

# Summary statistics between continuous variables ----
## Pearson correlation ----

mean_store_spend <- mean(x = customer$store.spend) 
mean_online_spend <- mean(x = customer$online.spend)

numerator <- sum((customer$store.spend - mean_store_spend) * (customer$online.spend - mean_online_spend)) 
denominator <- sqrt(sum((customer$store.spend - mean_store_spend)^2)) * sqrt(sum((customer$online.spend - mean_online_spend)^2))

coef_pearson <- numerator / denominator

cor(x = customer$store.spend, 
    y = customer$online.spend, 
    method = "pearson")

## Correlation matrices ----
is.numeric(customer$age)
is.numeric(customer$email)

correlation_matrix <- customer |> 
  select(where(is.numeric)) |> 
  correlate(use = "pairwise.complete.obs",
            method = "pearson",
            diagonal = 1)

### Visualizing correlation matrix ---- 
correlation_matrix |> 
  autoplot(triangular = "lower")


## Transforming and visualizing data ----
customer |> 
  ggplot() +
  geom_histogram(aes(x = distance.to.store),
                 color = "black")

customer |> 
  ggplot() +
  geom_histogram(aes(x = distance.to.store_trans),
                 color = "black")

customer |> 
  ggplot() +
  geom_point(aes(x = distance.to.store,
                 y = store.spend))

### Prepare data
customer <- customer |> 
  mutate(distance.to.store_trans = 1/distance.to.store,
         distance.to.store_log = log(distance.to.store))

customer |> 
  ggplot() + 
  geom_point(aes(x = distance.to.store_trans,
                 y = store.spend))

customer |> 
  ggplot() + 
  geom_point(aes(x = distance.to.store_log,
                 y = store.spend))

cor(customer$distance.to.store,
    customer$store.spend)

cor(customer$distance.to.store_trans,
    customer$store.spend)

cor(customer$distance.to.store_log,
    customer$store.spend)

## visualizing categorical values
customer |> 
  ggplot() + 
  geom_point(aes(x = sat.service,
                 y = sat.selection),
             position = position_jitter(width = 0.2,
                                        height = 0.2))

customer_satisfaction <- customer |> 
  count(sat.service, sat.selection)

customer_satisfaction |> 
  ggplot() +
  geom_label(aes(x = sat.service,
                 y = sat.selection,
                 label = n))

  








