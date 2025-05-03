# Library ----
library(tidyverse)

# Data
tibble(x = c(0.05, 0.8, 0.45),
       y = c(1000, 3000, 5000)) |> 
  mutate(pct_x = scales::number(x = x, 
                                scale = 100, 
                                suffix = "%"),
         currency_y = scales::number(x = y,
                                     prefix = "$", 
                                     big.mark = ",",
                                     suffix = " COP"))
