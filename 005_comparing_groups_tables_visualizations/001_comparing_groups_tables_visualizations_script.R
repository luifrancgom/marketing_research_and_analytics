# Libraries ----
library(tidyverse)
library(skimr)
library(DT)

# Import ----
# segmentation <- read_csv(file = "http://goo.gl/qw303p")
segmentation <- read_csv(file = "000_data/005_chapter_5.csv")

# Inspect ----
segmentation |> 
  glimpse()

# Transform ----
segmentation <- segmentation |> 
  mutate(
    gender = factor(x = gender,
                    ordered = FALSE),
    kids = as.integer(x = kids),
    ownHome = factor(x = ownHome,
                     ordered = FALSE),
    subscribe = factor(x = subscribe,
                       ordered = FALSE),
    Segment = factor(x = Segment,
                     ordered = FALSE)
  )
  
# Summarize ----
segmentation |> 
  skim()

# Filter and counting ----
segmentation |> 
  count(ownHome)

segmentation |> 
  count(subscribe)

# Take into account that
segmentation |> 
  filter(income <= 0)

## Possible solution ----
segmentation_filter <- segmentation |> 
  filter(income > 0)
  
segmentation_filter |> 
  skim()

# Summary statistics ----
segmentation |> 
  summarise(mean_income = mean(income))

## Summary statistic by group ----
### Mean income ----
segmentation |> 
  group_by(ownHome) |> 
  summarise(mean_income = mean(income))

segmentation |> 
  group_by(Segment) |> 
  summarise(mean_income = mean(income))
  
segmentation_own_seg_income <- segmentation |> 
  group_by(ownHome, Segment) |> 
  summarise(mean_income = mean(income)) |> 
  ungroup()

segmentation_own_seg_income |> 
  datatable(
    colnames = c("Own home status",
                 "Segment",
                 "Mean Income")) |> 
  formatRound(columns = c("mean_income"), 
              digits = 2)
  
### Kids ----
segmentation_kids_income <- segmentation |> 
  group_by(ownHome, Segment) |> 
  summarise(total_kids = sum(kids)) |> 
  ungroup()
  
segmentation_kids_income |> 
  datatable(
    colnames = c("Own home status",
                 "Segment",
                 "Total kids") 
  )

# Visualization ----
# Colors: https://www.color-hex.com/color-palettes/
## Percentage of subscribers by ownHome, Segment
segmentation_subs_own_seg <- segmentation |> 
  count(subscribe, 
        ownHome, 
        Segment) |> 
  group_by(ownHome, Segment) |> 
  mutate(n_pct = (n / sum(n))*100) |> 
  ungroup() |> 
  arrange(ownHome, Segment)
  
segmentation_subs_own_seg |> 
  ggplot() +
  geom_col(aes(x = subscribe,
               y = n_pct,
               fill = subscribe),
           color = "black") +
  scale_fill_manual(values = c("#6aa84f",
                               "#f1c232")) +
  facet_wrap(vars(ownHome, Segment),
             nrow = 2,
             ncol = 4) +
  labs(x = "Subscribe status",
       y = "Percentage (%)",
       fill = "Subscription status")
  
## Mean income by segment, owhome
segmentation_own_seg_income <- segmentation |> 
  group_by(ownHome, Segment) |> 
  summarise(mean_income = mean(income)) |> 
  ungroup()

segmentation_own_seg_income |> 
  ggplot() + 
  geom_col(aes(x = Segment,
               y = mean_income,
               fill = ownHome),
           color = "black",
           position = position_dodge()) +
  scale_fill_manual(values = c("#6aa84f",
                               "#f1c232")) +
  labs(y = "Mean yearly disposable income (US Dollars)",
       fill = "Own home status")



