# Libraries ----
library(tidyverse)
library(skimr)

# Import ----
url <- "http://goo.gl/QPDdMl"
# In the case if the local file fails
# weekly_stores <- read_csv(file = url)
weekly_stores <- read_csv(file = "000_data/003_data_chapter3.csv")
weekly_stores |> head()

# Exploring data ----
weekly_stores |> glimpse()
weekly_stores |> View()

# Transforming data ----
weekly_stores <- weekly_stores |> 
  mutate(storeNum = factor(x = storeNum, 
                           ordered = FALSE),
         Year = factor(x = Year,
                       ordered = TRUE),
         Week = factor(x = Week, ordered = TRUE),
         p1prom = as.logical(x = p1prom),
         p2prom = as.logical(x = p2prom))

weekly_stores

# Summarizing data ----
weekly_stores |> summary()
weekly_stores |> skim()

weekly_stores |> count(country)
weekly_stores |> count(storeNum)

# Visualization data ----
weekly_stores |> 
  ggplot() +
  geom_histogram(aes(x = p1sales, y = after_stat(density)),
                 color = "black", fill="lightblue",
                 bins = 30) +
  geom_density(aes(x = p1sales),
               color = "darkred", 
               linetype = "solid",
               linewidth = 1) +
  scale_x_continuous(breaks = seq(from = 60, to = 300, by = 20)) +
  labs(x = "Sales of product 1 (units)",
       y = "Density",
       title = "Product 1: Distribution of sales in all stores")


