# Libraries ----
library(tidyverse)
library(skimr)
library(scales)


# Import data ----
segmentation <- read_csv(file = "000_data/005_data_chapter5.csv")
segmentation

# Inspect data ----
segmentation |> 
  glimpse()

# Transform data ----
segmentation <- segmentation |> 
  mutate(gender = factor(x = gender,
                         ordered = FALSE),
         kids = as.integer(kids),
         ownHome = factor(x = ownHome,
                          ordered = FALSE),
         subscribe = factor(x = subscribe,
                            ordered = FALSE),
         Segment = factor(x = Segment,
                          ordered = FALSE))
  
# Summarize data ----
segmentation |> 
  skim()

# Count data ----
segmentation |> 
  count(ownHome)

# Summary statistics by groups ----
## Question 1: Are there differences 
##             between segments in
##             relation to income?
segmentation |> 
  select(Segment, income) |> 
  group_by(Segment) |> 
  summarise(mean_income = mean(income))

segmentation |> 
  select(ownHome, income) |> 
  group_by(ownHome) |> 
  summarise(mean_income = mean(income))

segmentation |> 
  group_by(ownHome, Segment) |> 
  summarise(mean_income = mean(income)) |> 
  ungroup()

## Question 2: Are there differences 
##             between segments in
##             relation to the number
##             of kids?
segmentation |> 
  group_by(ownHome, Segment) |> 
  summarise(number_kids = sum(kids)) |> 
  ungroup()

## Question 3: What are the percentage
##             of subscribers by Segment
##             and by the status of owning
##             a house
subscriber_by_segment_home_ownership <- segmentation |> 
  count(subscribe, ownHome, Segment) |> 
  group_by(ownHome, Segment) |> 
  mutate(number_percentage = (n / sum(n))*100) |> 
  ungroup() |> 
  arrange(Segment, ownHome)

subscriber_by_segment_home_ownership

### Example how to arrange ----
df <- tibble(
  name = c("Alice", "Bob", "Charlie", "David"),
  age = c(25, 30, 22, 28)
)

df_sorted <- df |>  
  arrange(age)

## Visualization by groups ----
### Using facets ----
subscriber_by_segment_home_ownership |> 
  ggplot() +
  geom_col(aes(x = subscribe, 
               y = number_percentage)) +
  facet_wrap(facets = vars(ownHome, 
                           Segment), 
             nrow = 2, ncol = 4)

### Plotting proportions ----
#### Question 4: What proportion of clients 
####             are subscribers by segment?
prop_table <- segmentation |> 
  select(Segment, subscribe) |> 
  count(Segment, subscribe) |> 
  group_by(Segment) |> 
  mutate(pct_subscribers = n / sum(n)) |> 
  ungroup() |> 
  filter(subscribe == "subYes") |> 
  mutate(Segment = fct_reorder(.f = Segment,
                               .x = pct_subscribers))

prop_table |> 
  ggplot() + 
  geom_col(aes(x = pct_subscribers, 
               y = Segment),
           color = "black",
           fill = "#7ED4AD") + 
  labs(x = "Proportion of subscribers",
       y = "",
       title = "Proportions of subscribers by segment")

### Grouping with a continuous variable ----
#### Question 5: Are there differences between 
####             multiple categories(Segment, ownHome)
####             in relation to income

# Note: another option using the argument
# .group = "drop"
segmentation |> 
  select(income, ownHome, Segment) |> 
  group_by(Segment, ownHome) |> 
  summarise(mean_income = mean(income), 
            .groups = "drop")

mean_income_by_seg_ownhome <- segmentation |> 
  select(income, ownHome, Segment) |> 
  group_by(Segment, ownHome) |> 
  summarise(mean_income = mean(income)) |> 
  ungroup()

mean_income_by_seg_ownhome |> 
  ggplot() + 
  geom_col(aes(x = Segment, y = mean_income,
               fill = ownHome),
           position = position_dodge(),
           color = "black") + 
  scale_fill_manual(values = c("#7ED4AD", 
                               "#D76C82")) +
  labs(x = "",
       y = "Mean disposable income (US Dollars)",
       fill = "Owning home status",
       title = "Comparing of mean income by segment and owning home status")

### Grouping with a continuous variable and using facets ----  
#### Question 6: Distribution of income by more than
####             2 categories (ownHome, Segment)
income_distribution_by_seg_ownhome <- segmentation |> 
  select(income, ownHome, Segment) |>
  mutate(Segment = fct_reorder(.f = Segment,
                               .x = income))
  
income_distribution_by_seg_ownhome |>    
  ggplot() + 
  geom_boxplot(aes(x = income, y = Segment,
                   fill = ownHome)) + 
  scale_fill_manual(values = c("#7ED4AD", 
                               "#D76C82")) +
  scale_x_continuous(labels = label_dollar(scale = 0.01)) +
  facet_wrap(facets = vars(ownHome)) + 
  labs(x = "Disposable income in thousands (USD dollars)",
       y = "",
       fill = "Owning home status",
       title = "Distribution of income by owning home status and segment")
  


