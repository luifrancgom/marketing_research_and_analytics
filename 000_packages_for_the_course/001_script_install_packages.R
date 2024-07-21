# Packages ----
packages <- c(
              ## Chapman & Feit (2019) ----
              ### Chapter 2 ----
              "lavaan", 
              "semPlot", 
              "corrplot", 
              "multcomp",
              ### Chapter 3 ----
              "car",
              "psych",
              "beanplot",
              ### Chapter 4 ----
              "gplots",
              ### Chapter 5 ----
              "lattice",
              ### Chapter 6 ----
              "binom",
              "multcomp",
              ### Chapter 7 ----
              "coefplot",
              ### Chapter 8 ----
              "RColorBrewer",
              "cluster",
              ## Tidyverse approach ----
              ### Data science ----
              "tidyverse",
              ### Modeling and machine learning ----
              "tidymodels",
              "corrr",
              ### Visualization ----
              "dotwhisker",
              "tidyheatmaps",
              "ggbiplot",
              ### Summary statistics ---- 
              "skimr",
              ### Prepare REPRoducible EXamples ----
              "reprex",
              ### Tables ----
              #### HTML tables ----
              "DT",
              ### Data: bike_sales 
              "sweep")

for (package in packages) {
  if (!(package %in% rownames(installed.packages()))) {
    install.packages(package)
  }
}