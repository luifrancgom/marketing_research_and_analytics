# Libraries ----
library(tidyverse)
library(skimr)

# Import data
## weekly_store <- read_csv(file = "http://goo.gl/QPDdMl")
weekly_store <- read_csv(file = "000_data/003_data_chapter3.csv")

# Inspect data ----
weekly_store |> glimpse()
weekly_store |> head(n = 3)

# Transform data ----
weekly_store <- weekly_store |> 
  mutate(storeNum = factor(x = storeNum, 
                           ordered = FALSE),
         Year     = factor(x = Year,
                           ordered = TRUE),
         Week     = factor(x = Week,
                           ordered = TRUE),
         p1prom   = as.logical(x = p1prom),
         p2prom   = as.logical(x = p2prom))

weekly_store

# Summary data ----
weekly_store |> skim()

# Count ----
weekly_store |> count(country)
weekly_store |> count(storeNum)

# Visualization data ----
## Histogram ----
weekly_store |> 
  ggplot() +
  geom_histogram(aes(x = p1sales),
                 color = "black",
                 bins = 20) +
  labs(x = "Unidades vendidas del producto 1",
       y = "Frecuencia",
       title = "Frecuencia de las ventas semanales del producto 1")

## Boxplot ----
weekly_store |> 
  ggplot() +
  geom_boxplot(aes(x = p2sales, 
                   y = p2prom)) +
  labs(x = "Unidades vendidas producto 2",
       y = "Se generó una promoción?",
       title = "Efectividad de las promociones sobre ventas semanales")

# Answering questions with data ----
## Preparing data
### In what countries the company sell 
### more units of product 2?
unit_sales_by_country <- weekly_store |> 
  select(country, p2sales) |> 
  group_by(country) |> 
  summarise(total_units = sum(p2sales)) |> 
  mutate(country = fct_reorder(.f = country,
                               .x = total_units))

unit_sales_by_country

## Visualize data ----
unit_sales_by_country |> 
  ggplot() +
  geom_col(aes(x = country, y = total_units))
  


