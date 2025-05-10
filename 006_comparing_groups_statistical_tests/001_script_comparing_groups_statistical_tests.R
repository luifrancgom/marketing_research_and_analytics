# Libraries ----
library(tidyverse)
library(tidymodels)

# Import ----
# segmentation <- read_csv(file = "http://goo.gl/qw303p")
# segmentation

segmentation <- read_csv(file = "000_data/005_data_chapter5.csv")
segmentation

# Count ----
segmentation |> 
  count(Segment)

segmentation |> 
  count(subscribe, ownHome) |> 
  pivot_wider(id_cols = subscribe, 
              names_from = ownHome, 
              values_from = n)

# Statistical tests ----
## Chi2 test for given probabilities ----
chi2_test <- table(segmentation$Segment) |> 
  chisq.test(p = c(1/4, 1/4, 1/4, 1/4))

chi2_test

(((c(70, 100, 80, 50) - (300*(1/4)))^2)/ (300*(1/4))) |> 
  sum()

# Chi2 distribution
## https://en.wikipedia.org/wiki/Chi_distribution
pchisq(q = 17.33333, 
       df = 4 - 1, 
       lower.tail = FALSE)

## alpha = 0.1, 0.05, 0.01
alpha <- 0.05
qchisq(p = alpha,
       df = 4 - 1,
       lower.tail = FALSE)

segmentation |> 
  chisq_test(response = Segment,
             p = c(1/4, 1/4, 1/4, 1/4))

## Chi2 pearson
1/2
(1/2)*(1/2)

chi2_person <- table(segmentation$subscribe,
                     segmentation$ownHome) |> 
  chisq.test(correct = FALSE)

chi2_person

alpha <- 0.05
qchisq(p = alpha, 
       df = 1, 
       lower.tail = FALSE)

## t test ----
segmentation |> 
  group_by(ownHome) |> 
  summarise(mean_income = mean(income))

segmentation |> 
  group_by(ownHome) |> 
  summarise(mean_income = mean(income),
            var_income  = var(income),
            n = n())

alpha <- 0.05

t_test <- t.test(income ~ ownHome,
                 data = segmentation, 
                 alternative = "two.sided",
                 mu = 0,
                 conf.level = 1 - alpha)
t_test

?qt

qt(p = alpha/2, 
   df = 285.25, 
   lower.tail = TRUE)

pt(q = -3.2731,
   df = 285.25,
   lower.tail = TRUE) * 2

## Anova ----
## Segment ----
segmentation |> 
  group_by(Segment) |> 
  summarise(mean_income = mean(income),
            var_income  = var(income),
            n           = n())

anova_table <- aov(data = segmentation,
                   formula = income ~ Segment) |> 
  anova()

anova_table

anova_table |> 
  tidy()

# 0.1, 0.05, 0.01
alpha <- 0.05

?df

qf(p = alpha, 
   df1 = 3, 
   df2 = 296,
   lower.tail = FALSE)

pf(q = 81.828, 
   df1 = 3,
   df2 = 296,
   lower.tail = FALSE)

## Segment, ownHome
anova_table2 <- aov(formula = income ~ Segment + ownHome,
                    data = segmentation) |> 
  anova()

anova_table2 |> 
  tidy()

anova_table3 <- aov(formula = income ~ Segment + ownHome + Segment:ownHome,
                    data = segmentation) |> 
  anova()

anova_table3 |> 
  tidy()

anova_table3

aov(formula = income ~ Segment*ownHome,
    data = segmentation) |> 
  anova()


