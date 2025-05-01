# Libraries ----
library(tidyverse)
library(skimr)
library(DT)

# Import data ----
# segmetation <- read_csv(file = "http://goo.gl/qw303p")
segmentation <- read_csv(file = "000_data/005_data_chapter5.csv")
segmentation

# Inspect data ----
segmentation |> 
  glimpse()

# Transform data ----
segmentation <- segmentation |> 
  mutate(gender = factor(x = gender, ordered = FALSE),
         kids = as.integer(x = kids),
         ownHome = factor(x = ownHome, ordered = FALSE),
         subscribe = factor(x = subscribe, ordered = FALSE),
         Segment = factor(x = Segment, ordered = FALSE))

segmentation

# Summary statistics ----
segmentation |> 
  skim()

## Detecting problems ----
segmentation |> 
  filter(income < 0)

segmentation |> 
  filter(income >= 0)

# Descriptive statistics by group ----
segmentation |> 
  group_by(ownHome) |> 
  summarise(mean_income = mean(income))

segmentation |> 
  group_by(subscribe) |> 
  summarise(mean_income = mean(income))

segmentation |> 
  group_by(Segment, ownHome) |> 
  summarise(mean_income = mean(income)) |> 
  ungroup()

segmentation |> 
  group_by(Segment, ownHome) |> 
  summarise(sum_kids = sum(kids))
  
income_by_segment_ownhome <- segmentation |> 
  group_by(Segment, ownHome) |> 
  summarise(mean_income = mean(income)) |> 
  ungroup()

income_by_segment_ownhome |> 
  datatable(colnames = c("Own Home" = "ownHome",
                         "Mean Income" = "mean_income")) |> 
  formatRound(columns = "Mean Income", 
              digits = 2)

# Visualization ----
## Cuales la proporción de clientes 
## que se subscriben a mi producto
## por segment
pct_customers_subscribe <- segmentation |> 
  count(subscribe, Segment) |> 
  group_by(Segment) |> 
  mutate(pct_subscribers = (n / sum(n))*100) |> 
  ungroup() |> 
  filter(subscribe == "subYes") |> 
  mutate(Segment = fct_reorder(.f = Segment, 
                               .x = pct_subscribers))
  
pct_customers_subscribe |> 
  ggplot() +
  geom_col(aes(x = pct_subscribers,
               y = Segment)) +
  labs(x = "Percentage of subscribers",
       title = "Percentage of subscribers by segment")





