# Libraries -----
library(tidyverse)
library(tidymodels)

# Import data ----
## Importing data locally
# segmentation <- read_csv(file = "000_data/005_data_chapter5.csv")
segmentation <- read_csv(file = "http://goo.gl/qw303p")
segmentation |> head()

# Exploring data ----
segmentation |> glimpse()
segmentation |> count(Segment)

# Random variables
## Modeling a dice
sample(x = 1:6, size = 1, replace = TRUE)

# Chi squared test 1 ----
## For given probabilities
chi_statistic <- table(segmentation$Segment) |> 
  chisq.test(p = c(1/4, 1/4, 1/4, 1/4))

chi_statistic$parameter[1]

## Calculate the probability using the statistic
?qchisq
pchisq(q = 17.333, df = 3, lower.tail = FALSE)
qchisq(p = 0.05, df = 3, lower.tail = FALSE)

# Using tidymodels
segmentation |> 
  chisq_test(response = Segment,
             p = c(1/4, 1/4, 1/4, 1/4))

segmentation |> 
  chisq_test(response = Segment,
             p = c(1/4, 1/4, 1/4, 1/4)) |> 
  select(chisq_df)

# Chi squared test 2 ----
## To check independence between categories
segmentation |> 
  count(subscribe, ownHome)

# Independence
## What is the probability of 
## obtaining a 3 and a 6 when throwing a 
## dice
(1/6)*(1/6)

segmentation |>
  count(subscribe, ownHome) |>
  pivot_wider(id_cols = subscribe,
              names_from = ownHome,
              values_from = n)

(137 + 22) / (137 + 22 + 123 + 18)
(137 + 123) / (137 + 22 + 123 + 18)

(159 / 300) * (260 / 300)

## what I have
137

## What should be
300 * (159 / 300) * (260 / 300)

chi_statistic2 <- table(segmentation$subscribe,
                        segmentation$ownHome) |> 
  chisq.test(correct = FALSE)

?pchisq

pchisq(q = 0.074113, 
       df = 1, 
       lower.tail = FALSE)

# Distributions ---- 

## Binomial distribution ----
### https://en.wikipedia.org/wiki/Binomial_distribution
n <- 40
p <- 0.5

?dbinom
toss_coin <- tibble(x = 0:40) |> 
  mutate(f_x = dbinom(x = x,
                      size = n,
                      prob = p))

toss_coin |> 
  ggplot() + 
  geom_point(aes(x = x, y = f_x))

## Normal distribution ----
### https://en.wikipedia.org/wiki/Normal_distribution
?dnorm
toss_coin |> 
  ggplot() + 
  geom_point(aes(x = x, y = f_x)) + 
  geom_function(fun = dnorm,
                args = list(mean = n*p,
                            sd = sqrt(n*p*(1-p))),
                color = "red")

## Chi squared
### https://en.wikipedia.org/wiki/Chi-squared_distribution
?dchisq

tibble(z_1 = rnorm(n = 1000, mean = 0, sd = 1),
       z_2 = rnorm(n = 1000, mean = 0, sd = 1),
       z_3 = rnorm(n = 1000, mean = 0, sd = 1),
       z_1_2 = z_1^2,
       z_2_2 = z_1^2,
       z_3_2 = z_1^2,
       # In this case chi will have
       # 3 degrees of freedom
       ## https://en.wikipedia.org/wiki/Chi-squared_distribution
       chi = z_1_2 + z_2_2 + z_3_2) |> 
  ggplot() +
  geom_histogram(aes(x = chi,
                     y = after_stat(density)),
                 color = "black",
                 # Make histogram start at zero
                 boundary = 0) +
  geom_function(fun = dchisq,
                args = list(df = 3),
                color = "red",
                xlim = c(0, 30))

# t-test: mean difference ----
?t.test
segmentation |> 
  group_by(ownHome) |> 
  summarise(mean_income = mean(income),
            var_income = var(income),
            n = n())

qt(p = 0.025, df = 285.25, lower.tail = TRUE)

## Applying t-test
?t.test

t_test <- t.test(formula = income ~ ownHome,
                 alternative = "two.sided",
                 mu = 0,
                 # alpha = 1 - conf.level
                 ## 0.05
                 conf.level = 0.95,
                 data = segmentation)

t_test

# p-value = 0.001195
pt(q = -3.2731, df = 285.25) * 2

# F- test: difference between multiple means ----
segmentation |> 
  group_by(Segment) |> 
  summarise(mean_income = mean(income),
            var_income = var(income),
            n = n())

mean(segmentation$income)

?aov
?anova
f_test <- aov(formula = income ~ Segment,
              data = segmentation) 

anova_table <- f_test |> anova()

f_test
anova_table

# Multiple variables f-statistic
anova_table_2_or_more_var <- aov(formula = income ~ Segment + ownHome + Segment:ownHome,
              data = segmentation) |> 
  anova()

anova_table_2_or_more_var <- aov(formula = income ~ Segment*ownHome,
                                 data = segmentation) |> 
  anova()

anova_table_2_or_more_var