# Load packages ----
library(tidyverse)
library(skimr)

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
