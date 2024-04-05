# Libraries ----
library(tidyverse)
library(sweep)

# Exploring data base ----
?bike_sales
glimpse(bike_sales)

# Dividing it by tables ----
## We can recover the original tables in that
## way
closed_transactions <- bike_sales |> 
  select(order.date:price, customer.id,
         product.id)

products <- bike_sales |> 
  select(product.id:frame) |> 
  distinct()

customers <- bike_sales |> 
  select(customer.id:longitude) |> 
  distinct()

# Checking if bike_sales can be recover
closed_transactions |> 
  left_join(y = customers,
            by = join_by(customer.id)) |> 
  left_join(y = products,
            by = join_by(product.id)) |> 
  relocate(product.id, .after = longitude) |> 
  mutate( price.ext = quantity*price) |> 
  base::setequal(bike_sales)

# Importing data
closed_transactions |> 
  write_csv(file = "000_data/000_bike_sales/closed_transactions.csv")

products |> 
  write_csv(file = "000_data/000_bike_sales/products.csv")

customers |> 
  write_csv(file = "000_data/000_bike_sales/customers.csv")


bike_sales |> 
  View()