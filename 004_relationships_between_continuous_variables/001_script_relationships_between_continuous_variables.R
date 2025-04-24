# Libraries ----
library(tidyverse)
library(skimr)

# Import ----
# customer <- read_csv(file = "http://goo.gl/PmPkaG")
customer <- read_csv(file = "000_data/004_data_chapter4.csv")
customer

# Inspect ----
customer |> 
  glimpse()

# Transform ----
customer <- customer |> 
  mutate(cust.id = factor(x = cust.id, ordered = FALSE),
         email = factor(x = email, ordered = FALSE),
         online.visits = as.integer(x = online.visits),
         online.trans = as.integer(x = online.trans),
         store.trans = as.integer(x = store.trans),
         sat.service = factor(x = sat.service, 
                              ordered = TRUE),
         sat.selection = factor(x = sat.selection, 
                                ordered = TRUE))

customer

# Summarize ----
customer |> 
  skim()



