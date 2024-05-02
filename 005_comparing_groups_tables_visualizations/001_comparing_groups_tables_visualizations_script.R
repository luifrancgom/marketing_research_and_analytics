# Libraries ----
library(tidyverse)
library(skimr)

# Import data ----
# segmentation <- read_csv(file = "http://goo.gl/qw303p")
segmentation <- read_csv(file = "000_data/005_data_chapter5.csv")
segmentation |> head(n=5)

# Transform data ----
segmentation <- segmentation |>
  mutate(gender = factor(gender, ordered = FALSE),
         kids = as.integer(kids),
         ownHome = factor(ownHome, ordered = FALSE),
         subscribe = factor(subscribe, ordered = FALSE),
         Segment = factor(Segment, ordered = FALSE))
segmentation |> head(n=5)

# Explore data
segmentation |> skim()

segmentation |> 
  filter(income <= 0)

# Creating groups ----
## Exploration 1
segmentation |> 
  group_by(Segment, ownHome) |> 
  summarise(mean_income = mean(income)) |> 
  arrange(desc(mean_income))

## Exploration 2
segmentation |> 
  group_by(Segment, ownHome) |> 
  summarise(mean_income = mean(income),
            # Adding the standard deviation
            # For more information use ?sd
            sd_income = sd(income)) |> 
  arrange(desc(mean_income))

## Exploration 3
segmentation |> 
  group_by(Segment, ownHome, subscribe) |> 
  summarise(mean_income = mean(income),
            sd_income = sd(income)) |> 
  arrange(desc(mean_income))

# Knowing the percentage of subscribers ----
## Prepare data
subscriber_by_segment_home_ownership <- segmentation |> 
  count(subscribe, Segment, ownHome) |> 
  group_by(Segment, ownHome) |> 
  mutate(percentage_n = (n / sum(n)) * 100) |> 
  ungroup()

subscriber_by_segment_home_ownership

## Visualizing data
subscriber_by_segment_home_ownership |> 
  ggplot() + 
  geom_col(aes(x = subscribe, y = percentage_n)) +
  facet_wrap(facets = vars(Segment, ownHome),
             nrow = 2, ncol = 4)