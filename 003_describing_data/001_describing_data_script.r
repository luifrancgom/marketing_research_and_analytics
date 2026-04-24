# Load libraries ----
library(tidyverse)
library(skimr)

# Importa data ----
# read_csv(file = "/data/data_sets_marketing/003_data_set_chapter-3.csv")
weekly_store <- read_csv(file = "000_data/003_data_set_chapter-3.csv")

# Exploring data ----
weekly_store |> 
  glimpse()

# Transform data ----
weekly_store <- weekly_store |> 
  mutate(
    storeNum = factor(x = storeNum, ordered = FALSE),
    Year     = factor(x = Year, levels = 1:2, ordered = TRUE),
    Week     = factor(x = Week, levels = 1:52, ordered = TRUE),
    p1prom   = as.logical(p1prom),
    p2prom   = as.logical(p2prom)
  )

weekly_store

# Summary data set ----
weekly_store |> 
  skim()

# Count data ----
weekly_store |>
  count(country)

weekly_store |> 
  count(storeNum)

# Visualization ----

## Histogram ----
weekly_store |> 
  ggplot() +
  geom_histogram(
    aes(x = p1sales),
    color = "black",
    fill  = "lightblue"
  ) +
  labs(
    x = "Sales in units of product 1",
    y = "Frequency",
    subtitle = "Distribution of sales of product 1"
  )

## Boxplot ----
weekly_store |> 
  ggplot() +
  geom_boxplot(
    aes(x = p2sales, y = p2prom)
  ) +
  scale_y_discrete(labels = c("No", "Yes")) +
  labs(
    x = "Sales in units of product 2",
    y = "Product 2 was promoted?",
    subtitle = "Distribution of sales of product 2 when it is promoted or not promoted"
  )

## Barplot ----
# In what countries the company sell more units of product 2?
### Prepare data ----
weekly_store_sales_by_country <- weekly_store |> 
  group_by(country) |> 
  summarise(sum_p2sales = sum(p2sales)) |> 
  mutate(country = fct_reorder(.f = country, .x = sum_p2sales))

weekly_store_sales_by_country |> 
  ggplot() +
  geom_col(
    aes(x = country, y = sum_p2sales),
    color = "black",
    fill = "steelblue"
  ) + 
  scale_x_discrete(
    labels = c(
      "Australia",
      "China",
      "Brazil",
      "United States",
      "Britain",
      "Japan",
      "Germany"
    )
  ) +
  labs(
    x = NULL,
    y = "Sales (Units)",
    subtitle = "Distribution of total sales of product 2 by country"
  )

