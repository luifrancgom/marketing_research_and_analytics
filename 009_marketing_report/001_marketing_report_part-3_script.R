# Load libraries ----
library(tidyverse)
library(scales)
library(sweep)
library(tidymodels)
library(DT)
library(tidyheatmaps)
library(parameters)

# Import data ----
bike_sales <- bike_sales |> 
  mutate(
    bikeshop.name      = factor(x = bikeshop.name, ordered = FALSE),
    bikeshop.city      = factor(x = bikeshop.city, ordered = FALSE),
    bikeshop.state     = factor(x = bikeshop.state, ordered = FALSE),
    model              = factor(x = model, ordered = FALSE),
    category.primary   = factor(x = category.primary, ordered = FALSE),
    category.secondary = factor(x = category.secondary, ordered = FALSE),
    frame              = factor(x = frame, ordered = FALSE),
    order.id           = factor(x = order.id, ordered = FALSE),
    order.line         = factor(x = order.line, ordered = TRUE),
    customer.id        = factor(x = customer.id, ordered = FALSE),
    product.id         = factor(x = product.id, ordered = FALSE)
  )

# Product gap ----
## Count data ----
bike_sales |> 
  count(model)

bike_sales |> 
  count(category.secondary)

bike_sales |> 
  count(bikeshop.state)

## Prepare data ----
total_sales_by__model_cat2_frame <- bike_sales |> 
  select(
    model,
    category.secondary,
    frame,
    price.ext
  ) |> 
  group_by(
    model, category.secondary, frame
  ) |> 
  summarise(
    total_revenue = sum(price.ext)
  ) |> 
  ungroup() |> 
  arrange(desc(total_revenue)) |> 
  mutate(category.secondary = fct_reorder(
    .f = category.secondary,
    .x = total_revenue
  )
)

total_sales_by__model_cat2_frame

## Visualize data ----
total_sales_by__model_cat2_frame |> 
  ggplot() + 
  geom_point(
    aes(x = frame, y = total_revenue),
    position = position_jitter(
      width = 0.2,
      height = 0.2
    )
  ) +
  scale_y_continuous(
    labels = label_currency()
  ) +
  facet_wrap(
    facets = vars(category.secondary)
  ) +
  labs(
    x = "Frame",
    y = "Total revenue (US dollars)"
  )

# Linear models ----
## Estimation ----
# Reference category: Cross Country Race
model1 <- lm(
  formula = price ~ category.secondary,
  data = bike_sales
)

model1_tidy <- model1 |> 
  tidy()

model1_tidy

model2 <- lm(
  formula = price ~ category.secondary + frame,
  data = bike_sales
)

# Select model
model2_tidy <- model2 |> 
  tidy()

model2_tidy |> 
  datatable(
    colnames = c(
      "Term",
      "Estimated parameters",
      "Standard error",
      "Statistic",
      "P-value"
    )
  ) |> 
  formatRound(
    columns = c(2, 3, 4, 5),
    digits = 3
  )

# Discard
model3 <- lm(
  formula = price ~ category.secondary + frame + category.secondary:frame,
  data = bike_sales
)

model3_tidy <- model3 |> 
  tidy()

model3_tidy

model4 <- lm(
  formula = price ~ category.secondary + frame + bikeshop.state,
  data = bike_sales
)

model4_tidy <- model4 |> 
  tidy()

model4_tidy |> 
  View()

## Prediction ----
new_data <- tibble(
  category.secondary = "Fat Bike",
  frame = "Carbon"
)

model2_new_data_pred <- predict(
  object = model2,
  newdata = new_data
) |> 
  enframe(
    name = "Observation",
    value = "Predicted Price"
  ) |> 
  bind_cols(new_data)

model2_new_data_pred |> 
  datatable(
    colnames = c(
      "Observation",
      "Predicted price",
      "Category secondary",
      "Frame"
    )
  ) |> 
  formatRound(
    columns = c(2),
    digits = 2
  )

# Segment bike shops with k-means clustering ----
## Bike shop revenue trends ----
bike_sales_total_revenue <- bike_sales |> 
  select(
    bikeshop.name,
    price.ext,
    model,
    category.primary,
    category.secondary,
    frame
  ) |> 
  group_by(
    bikeshop.name,
    model,
    category.primary,
    category.secondary,
    frame
  ) |> 
  summarise(total_revenue = sum(price.ext)) |> 
  ungroup()

## Prepare data ----
bike_sales_total_revenue_pct <- bike_sales_total_revenue |> 
  group_by(bikeshop.name) |> 
  mutate(total_revenue_pct = total_revenue / sum(total_revenue)) |> 
  ungroup()

bike_sales_total_revenue_pct

### Customer-product table
customer_product_table <- bike_sales_total_revenue_pct |> 
  select(
    bikeshop.name,
    model,
    total_revenue_pct
  ) |> 
  pivot_wider(
    id_cols = bikeshop.name,
    names_from = model,
    values_from = total_revenue_pct,
    values_fill = 0 
  )

customer_product_table

## Data visualization k-means and pca ----
### PCA ----
pca_object <- customer_product_table |> 
  select(-bikeshop.name) |> 
  prcomp(
    center = TRUE,
    scale. = FALSE
  )

pca_object |> 
  tidy(matrix = "eigenvalues")

pca_1_2 <- pca_object$x |> 
  as_tibble() |> 
  select(
    PC1,
    PC2
  )

pca_1_2 |> 
  ggplot() + 
  geom_point(
    aes(x = PC1, y = PC2)
  )

### Heatmap ----
customer_product_table_longer <- customer_product_table |> 
  pivot_longer(
    cols = -bikeshop.name,
    names_to = "model",
    values_to = "total_revenue_pct"
  )

customer_product_table_longer

tidyheatmap(
  df = customer_product_table_longer,
  rows = model,
  columns = bikeshop.name,
  values = total_revenue_pct,
  border_color = "black",
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  clustering_method = "complete"
)

## Sementation clustering kmeans ----
customer_product_table
### Choosing the number of clusters ----
n_clust_customer_product_table <- customer_product_table |> 
  select(-bikeshop.name) |> 
  n_clusters(
    standardize = FALSE, # The data is using the same scale
    nbclust_method = "kmeans",
    n_max = 10
  )

n_clust_customer_product_table |> 
  as_tibble() |> 
  mutate(n_Clusters = as.integer(n_Clusters)) |> 
  datatable(
    colnames = c(
      "Number of Clusters",
      "Method",
      "R package",
      "Duration (seconds)"
    )
  ) |> 
  formatRound(
    columns = c(4),
    digits = 2
  )

n_clust_customer_porduct_table |> 
  View()

n_clusters <- 4

### Applying k-means ----
set.seed(seed = 1234)
kmeans_object <- customer_product_table |> 
  select(-bikeshop.name) |> 
  kmeans(
    centers = n_clusters, 
    algorithm = "Hartigan-Wong"  
  )


kmeans_object$cluster
kmeans_object$centers |> 
  as_tibble()

### Extract clusters ----
clusters <- kmeans_object |> 
  augment(
    data = customer_product_table
  ) |> 
  select(
    bikeshop.name,
    .cluster
  )

clusters |> 
  arrange(.cluster) |> 
  datatable(
    colnames = c(
      "Bikeshop name",
      "Cluster"
    )
  )

### Cluster visualization ----
pca_1_2 |> 
  ggplot() +
  geom_point(
    aes(x = PC1, y = PC2)
  )

clusters_pca_1_2 <- pca_1_2 |> 
  bind_cols(clusters)

clusters_pca_1_2 |> 
  ggplot() + 
  geom_point(
    aes(
      x = PC1, y = PC2,
      color = .cluster
    )
  )

clusters_pca_1_2 |> 
  filter(PC1 < -0.1)
