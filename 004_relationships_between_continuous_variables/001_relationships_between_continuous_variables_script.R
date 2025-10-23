# Libraries ----
library(tidyverse)
library(skimr)

# Import ----
# customer <- read_csv(file = "http://goo.gl/PmPkaG")
customer <- read_csv(file = "000_data/004_chapter_4.csv")

# Transform
customer <- customer |> 
  mutate(cust.id = factor(x = cust.id, 
                          ordered = FALSE),
         email = factor(x = email,
                        ordered = FALSE),
         online.visits = as.integer(x = online.visits),
         online.trans = as.integer(x = online.trans),
         store.trans = as.integer(x = store.trans),
         sat.service = factor(x = sat.service,
                              ordered = TRUE),
         sat.selection = factor(x = sat.selection,
                                ordered = TRUE))

# Inspect ----
customer |> 
  glimpse()

# Summarize ----
customer |> 
  skim()
  
