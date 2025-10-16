# Libraries ----
library(tidyverse)
library(skimr)

# Import ----
# weekly_store <- read_csv(file = "http://goo.gl/QPDdMl")
weekly_store <- read_csv(file = "000_data/003_chapter_3.csv")

# Inspect ----
weekly_store |> 
  head(n = 5)

weekly_store |> 
  glimpse()

# Transform ----
weekly_store <- weekly_store |> 
  mutate(
    storeNum = factor(x = storeNum, 
                         ordered = FALSE),
    Year = factor(x = Year, 
                     ordered = TRUE),
    Week = factor(x = Week,
                  ordered = TRUE),
    p1prom = as.logical(x = p1prom),
    p2prom = as.logical(x = p2prom)
  )

# Summarize ----
weekly_store |> 
  skim()

# Count ----
weekly_store |> 
  count(country)
# Australia - AU
# Brazil - BR
# China  - CN
# Germany - DE
# Great Britain - GB
# Japan - JP
# United States - US

weekly_store |> 
  count(storeNum)

weekly_store |> 
  count(Year)

weekly_store |> 
  count(p1sales, 
        sort = TRUE)

# Visualization ----
## Histogram ----
weekly_store |> 
  ggplot() +
  geom_histogram(aes(x = p1sales),
                 color = "black",
                 fill = "steelblue",
                 bins = 30) +
  labs(x = "Product 1 sales (units)",
       y = "Frequency",
       subtitle = "Distribution of product 1 sales")

## Boxplots ----
weekly_store |> 
  ggplot() +
  geom_boxplot(aes(x = p2sales,
                   y = p2prom),
               fill = "steelblue") +
  scale_y_discrete(labels = c("No", "Yes")) +
  labs(x = "Product 2 sales (units)",
       y = "Product 2 was promoted?",
       subtitle = "Distribution of product sales when product 2 was or was not promoted")
  
# Questions and answers with data ----
# In what countries the company sells 
# more units of product 2?

## Prepare data ----
weekly_store_sales_by_country <- weekly_store |> 
  group_by(country) |> 
  summarise(sum_p2sales = sum(p2sales)) |> 
  mutate(country = fct_reorder(.f = country, 
                               .x = sum_p2sales))

## Visualization sales
weekly_store_sales_by_country |> 
  ggplot() +
  geom_col(aes(x = country,
               y = sum_p2sales),
           color = "black",
           fill = "steelblue") +
  scale_x_discrete(labels = c("Australia",
                              "China",
                              "Brazil",
                              "United States",
                              "Great Britain",
                              "Japan",
                              "Germany")) +
  labs(x = "Country",
       y = "Total sales product 2 (units)",
       subtitle = "Total sales by country of product 2")



