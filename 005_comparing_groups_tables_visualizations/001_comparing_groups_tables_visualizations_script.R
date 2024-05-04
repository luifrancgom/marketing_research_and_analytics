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

subscriber_by_segment_home_ownership |> 
  ggplot() + 
  geom_col(aes(x = subscribe, y = percentage_n)) +
                       # Notation formula
  facet_wrap(facets =  ~ Segment + ownHome,
             nrow = 2, ncol = 4)

# Knowing the percentage of clients by segment ---
## Prepare data
prop_table <- segmentation |> 
  count(subscribe, Segment) |> 
  group_by(Segment) |> 
  mutate(n_pct = (n / sum(n)) * 100) |> 
  filter(subscribe == "subYes")

## Visualization
prop_table |> 
  ggplot() +
  geom_col(aes(x = n_pct, 
               y = fct_reorder(Segment, n_pct)),
           # Selecting specific colors
           ## Hex colors
           ### https://color-hex.org/
           color = "black", fill = "#a3b8c8") +
  labs(x = "Percentage (%)",
       y = "",
       title = "Percentage of subscribers by segment")

# Knowing mean income by segment 
# and if you own a home

## Prepare
seg_income_ag <- segmentation |> 
  group_by(Segment, ownHome) |> 
  summarise(mean_income = mean(income)) |> 
  arrange(desc(mean_income))

## Visualize
seg_income_ag |> 
  ggplot() +
  geom_col(aes(x = mean_income, 
               y = fct_reorder(Segment, 
                               mean_income),
               fill = ownHome),
           position = position_dodge(),
           color = "black") +
  scale_fill_manual(values = c("#123782", "#d6912d"))
# Add labels with labs(x = , y = , title)

# Distribution of mean income by segment
# and own Home
seg_income_ag |> 
  ggplot() +
  geom_boxplot(aes(x = mean_income, 
                   y = Segment)) +
  facet_wrap(facets = vars(ownHome))

## With formula notation
seg_income_ag |> 
  ggplot() +
  geom_boxplot(aes(x = mean_income, 
                   y = Segment)) +
  facet_wrap(facets = ~ ownHome)

# Distribution of income by segment
# and own Home
segmentation |> 
  ggplot() +
  geom_boxplot(aes(x = income, 
                   y = fct_reorder(Segment,
                                   income)),
               fill = "#d7b4ae") +
  facet_wrap(facets = ~ ownHome) +
  labs(x = "Income (US dollars)",
       y = "",
       title = "Distribution of income by Segment and own home")