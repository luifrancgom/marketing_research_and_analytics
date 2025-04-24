# Libraries ----
library(tidyverse)
library(skimr)

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
  
