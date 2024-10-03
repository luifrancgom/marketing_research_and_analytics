# Packages ----
library(tidyverse)
library(skimr)

# Import data ----
## Connecting to a server ----

# satisfaction_data <- read_csv(file = "http://goo.gl/UDv12g")
# satisfaction_data |> View()

## Locally
satisfaction_data <- read_csv(file = "000_data/002_data_chapter2.csv")
satisfaction_data

# Exploring data ----
satisfaction_data |> View()
satisfaction_data |> head(n = 5)

summary(object = satisfaction_data)

# Transforming data
## Ordinal variable
### https://es.wikipedia.org/wiki/Nivel_de_medida
satisfaction_data$Segment
satisfaction_data$Segment <- factor(x = satisfaction_data$Segment, 
                                    ordered = FALSE)

satisfaction_data <-  satisfaction_data |> 
  mutate(Segment = factor(x = Segment,
                          ordered = FALSE))
satisfaction_data

satisfaction_data |> skim()
