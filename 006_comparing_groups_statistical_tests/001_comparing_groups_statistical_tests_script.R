# Libraries ----
library(tidyverse)
library(tidymodels)

# Import ----
segmentation <- read_csv(file = "000_data/005_chapter_5.csv")

# Count ----
seg_count <- segmentation |> 
  count(Segment) |> 
  mutate(pct = n / sum(n))

seg_count

status_sub_home <- segmentation |> 
  count(subscribe, ownHome) |> 
  pivot_wider(id_cols = subscribe, 
              names_from = ownHome, 
              values_from = n)
  
# Statistical tests ----
## Chi-test proportions ----
chi_prop_test <- segmentation |> 
  chisq_test(response = Segment, 
             p = c(1/4, 1/4, 1/4, 1/4))
  
chi_prop_test

(((seg_count$n - (300*(1/4)))^2) / (300*(1/4))) |> 
  sum()

pchisq(q = 17.3, 
       df = 3,
       lower.tail = FALSE)

alpha <- 0.05

qchisq(p = alpha,
       df = 3,
       lower.tail = FALSE)

## chi-test pearson ----
status_sub_home

chiq_person_test <- segmentation |> 
  chisq_test(formula = subscribe ~ ownHome, 
             correct = FALSE)
  
chiq_person_test

# t-test ----
# 1 var categorical: 2 values
# 1 variable continuous
segmentation |>
  group_by(ownHome) |> 
  summarise(mean = mean(income),
            var  = var(income),
            n    = n())

segmentation |> 
  t_test(
    formula = income ~ ownHome,
    alternative = "two-sided",
    order = c("ownNo", "ownYes"),
    mu = 0,
    conf_level = 1 - alpha 
  )

pt(q = -3.273094, 
   df = 285.2521,
   lower.tail = TRUE)*2 

47391 - 54935  

# Anova ----
# 1 var categorical: n >=2 values
# 1 variable continuous
segmentation |> 
  summarise(mean = mean(income))

seg_income <- segmentation |> 
  group_by(Segment) |> 
  summarise(
    mean = mean(income),
    var  = var(income),
    n    = n()
  )
  
seg_income  

anova_table <- aov(data = segmentation, 
                   formula = income ~ Segment) |> 
  anova() |> 
  tidy()

pf(q = 81.82841, 
   df1 = 3, df2 = 296, 
   lower.tail = FALSE)

qf(p = alpha, 
   df1 = 3, df2 = 296,
   lower.tail = FALSE)

# n var categorical: n >=2 values
# 1 variable continuous
seg_home_1 <- segmentation |> 
  aov(formula = income ~ Segment + ownHome) |> 
  anova() |> 
  tidy()

seg_home_2 <- segmentation |> 
  aov(formula = income ~ Segment + ownHome + Segment:ownHome) |> 
  anova() |> 
  tidy()

seg_home_1
# income = f(Segment, ownHome)

seg_home_2
