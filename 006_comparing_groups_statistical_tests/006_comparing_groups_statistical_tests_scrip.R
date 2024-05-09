# Libraries -----
library(tidyverse)
library(tidymodels)

# Import data ----
## Importing data locally
# segmentation <- read_csv(file = "000_data/005_data_chapter5.csv")
segmentation <- read_csv(file = "http://goo.gl/qw303p")
segmentation |> head()

# Exploring data ----
segmentation |> glimpse()
segmentation |> count(Segment)

# Random variables
## Modeling a dice
sample(x = 1:6, size = 1, replace = TRUE)

# Chi squared test 1 ----
## For given probabilities
chi_statistic <- table(segmentation$Segment) |> 
  chisq.test(p = c(1/4, 1/4, 1/4, 1/4))

chi_statistic$parameter[1]

## Calculate the probability using the statistic
?qchisq
pchisq(q = 17.333, df = 3, lower.tail = FALSE)
qchisq(p = 0.05, df = 3, lower.tail = FALSE)

# Using tidymodels
segmentation |> 
  chisq_test(response = Segment,
             p = c(1/4, 1/4, 1/4, 1/4))

segmentation |> 
  chisq_test(response = Segment,
             p = c(1/4, 1/4, 1/4, 1/4)) |> 
  select(chisq_df)

# Chi squared test 2 ----
## To check independence between categories
segmentation |> 
  count(subscribe, ownHome)

# Independence
## What is the probability of 
## obtaining a 3 and a 6 when throwing a 
## dice
(1/6)*(1/6)

segmentation |>
  count(subscribe, ownHome) |>
  pivot_wider(id_cols = subscribe,
              names_from = ownHome,
              values_from = n)

(137 + 22) / (137 + 22 + 123 + 18)
(137 + 123) / (137 + 22 + 123 + 18)

(159 / 300) * (260 / 300)

## what I have
137

## What should be
300 * (159 / 300) * (260 / 300)

chi_statistic2 <- table(segmentation$subscribe,
                        segmentation$ownHome) |> 
  chisq.test(correct = FALSE)

?pchisq

pchisq(q = 0.074113, 
       df = 1, 
       lower.tail = FALSE)

