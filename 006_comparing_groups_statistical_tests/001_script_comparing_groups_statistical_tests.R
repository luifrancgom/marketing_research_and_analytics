# Libraries ----
library(tidyverse)
library(tidymodels)
library(DT)

# Import data ----
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

# Counting data ----
## Segment ----
count_segment <- segmentation |> 
  count(Segment) |> 
  arrange(desc(n)) |> 
  mutate(proportion = n / sum(n))

count_segment

## ownHome - subscribe ----
count_ownhome_subscribe <- segmentation |> 
  count(ownHome, subscribe) |> 
  pivot_wider(id_cols = subscribe, 
              names_from = ownHome, 
              values_from = n)

count_ownhome_subscribe

# Statistical tests ----
## Chi2 proportion ----
### Base R ----
chi_statistic <- table(segmentation$Segment) |> 
  chisq.test(p = c(1/4, 1/4, 1/4, 1/4))

chi_statistic

# In a manual way
sum((c(70, 100, 80, 50) - (300*(1/4)))^2 / (300*(1/4)))

pchisq(q = 17.333, 
       df =  4 - 1,
       # Is FALSE because P(X >= 17.333)
       lower.tail = FALSE)

### Tidymodels ----
segmentation |> 
  chisq_test(response = Segment,
             p = c(1/4, 1/4, 1/4, 1/4))

## Chi2 Pearson ----
count_ownhome_subscribe
### Base R ----
chi_statistic_pearson <- table(segmentation$ownHome,
                               segmentation$subscribe) |> 
  chisq.test(correct = FALSE)

chi_statistic_pearson
### Tidymodels ----
segmentation |> 
  chisq_test(formula = subscribe ~ ownHome,
             correct = FALSE) |> 
  datatable()

