# Load libraries ----
library(tidyverse)
library(skimr)
library(DT)
library(scales)

# Import data ----
# read_csv(file = "http://goo.gl/qw303p")
# read_csv(file = "/data/data_sets_marketing/005_data_set_chapter-5.csv")
segmentation <- read_csv(
  file = "000_data/005_data_set_chapter-5.csv"
)

# Explore data ----
segmentation |> 
  glimpse()

# Count ----
segmentation |> 
  count(Segment, sort = TRUE)

segmentation |> 
  count(ownHome)

# Transform data ----
segmentation <- segmentation |> 
  mutate(
    gender = factor(x = gender, ordered = FALSE),
    kids = as.integer(x = kids),
    ownHome = factor(x = ownHome, ordered = FALSE),
    subscribe = factor(x = subscribe, ordered = FALSE),
    Segment = factor(x = Segment, ordered = FALSE)
  )

# Summarize data ----
segmentation |> 
  skim()

# Filter data ----
segmentation |> 
  filter(income < 0)

# Summary statistics by group ----
segmentation |> 
  count(ownHome, Segment)

mean_income_by_ownhome_segment <- segmentation |> 
  group_by(ownHome, Segment) |> 
  summarise(mean_income = mean(income)) |> 
  ungroup()

mean_income_by_ownhome_segment |> 
  datatable(
    colnames = c(
      "Home owner status",
      "Segment",
      "Mean Income"
    )
  ) |> 
  formatRound(
    columns = 3,
    digits = 2
  )

segmentation |> 
  group_by(ownHome, Segment) |> 
  summarise(total_kids = sum(kids))

# Visualizations ----
# Identify the percentage of subscribers by
# home owner status (ownHome) and Segment
## Prepare data ----
subscribers_by_ownhome_segment <- segmentation |>
  count(subscribe, ownHome, Segment) |> 
  group_by(ownHome, Segment) |> 
  mutate(n_pct = (n / sum(n))*100) |> 
  arrange(ownHome, Segment) |> 
  ungroup()

subscribers_by_ownhome_segment |> 
  ggplot() +
  geom_col(
    aes(x = subscribe, y = n_pct)
  ) +
  facet_wrap(
    facets = vars(ownHome, Segment),
    ncol = 4,
    nrow = 2
  )

# Check the percentages of subscribers 
# (subscribers = Yes)
# by segment
# Prepare data 2 ----
subscribers_yes_by_segment <- segmentation |> 
  count(subscribe, Segment) |> 
  group_by(Segment) |> 
  mutate(n_pct = (n / sum(n))*100) |> 
  filter(subscribe == "subYes") |> 
  ungroup() |> 
  mutate(Segment = fct_reorder(
    .f = Segment, .x = n_pct
  ))

subscribers_yes_by_segment |> 
  ggplot() +
  geom_col(
    aes(x = n_pct, y = Segment),
    color = "black"
  ) +
  scale_x_continuous(
    label = label_number(suffix = " %")
  ) +
  labs(
    x = "Percentage of subscribers (%)",
    y = "Segment"
  )

segmentation |> 
  ggplot() +
  geom_boxplot(
    aes(x = income, y = Segment)
  ) +
  scale_x_continuous(
    label = label_currency()
  ) + 
  facet_wrap(facet = vars(ownHome))
