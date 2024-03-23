# Packages ----
packages <- c(# Packages associated to the book
              # R for Marketing Research and 
              # Analytics in relation to the 
              # chapters cover in the course
              "lavaan", 
              "semPlot", 
              "corrplot", 
              "multcomp",
              "car",
              "psych",
              "beanplot",
              "gplots",
              "lattice",
              "binom",
              "coefplot",
              "RColorBrewer",
              "cluster",
              # Packages using a tidyverse approach
              # and used in the different chapters
              # that are cover in the course but that
              # are not used constantly in the book
              # R for Marketing Research and Analytics 
              "tidyverse",
              "skimr",
              "tidymodels",
              "corrr",
              "dotwhisker",
              "tidyheatmaps",
              "ggbiplot",
              "sweep")

for (package in packages) {
  if (!(package %in% rownames(installed.packages()))) {
    install.packages(package)
  }
}