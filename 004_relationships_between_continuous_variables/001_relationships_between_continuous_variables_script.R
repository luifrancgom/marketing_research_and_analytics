# Libraries ----
library(tidyverse)
library(skimr)
library(corrr)
library(DT)

# Import ----
# customer <- read_csv(file = "http://goo.gl/PmPkaG")
customer <- read_csv(file = "000_data/004_chapter_4.csv")

# Transform
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

# Inspect ----
customer |> 
  glimpse()

# Summarize ----
customer |> 
  skim()
  
# Visualization ----
customer |> 
  ggplot() +
  geom_point(aes(x = store.spend,
                 y = online.spend,
                 colour = email)) +
  scale_x_continuous(transform = "log1p",
                     breaks = c(1, 2, 5, 
                                10, 20, 50, 
                                100, 500)) + 
  scale_y_continuous(transform = "log1p",
                     breaks = c(1, 5, 
                                50, 500))
  
# Correlation
## 2 variables
customer$store.spend
customer$online.spend

cor(x = customer$store.spend,
    y = customer$online.spend, 
    method = "pearson")

## Multiple variables
correlation_matrix <- customer |> 
  select(where(is.numeric)) |> 
  correlate(use = "pairwise.complete.obs")

correlation_matrix |> 
  datatable()

## Visualization
correlation_matrix |> 
  autoplot(triangular = "lower")

# Categorical variables
customer |> 
  ggplot() +
  geom_point(aes(x = sat.service,
                 y = sat.selection),
             position = position_jitter(width = 0.2,
                                        height = 0.2))
customer |> 
  count(sat.service,
        sat.selection) |> 
  ggplot() +
  geom_label(aes(x = sat.service,
                 y = sat.selection,
                 label = n))
  

