# Network and SSL configuration  ----
# We use 'wget' with the '--no-check-certificate' flag to bypass SSL handshake 
# errors and handle redirects to the binary package servers reliably on RHEL 10.
options(download.file.method = "wget")
options(download.file.extra  = "--no-check-certificate")

# Configuration ----
# Check R.home() to obtain R_HOME and
# R.home(component = 'bin') to obtain
# the executable path
# <executable-path> -e ".libPaths()"
target_lib <- "/opt/R/4.5.2/lib/R/library"
# Check the SETUP in https://packagemanager.posit.co
# for the specific Operating System, Linux Distribution
# and environment
my_repo <- "https://packagemanager.posit.co/cran/__linux__/rhel10/latest"

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
  ### Chapter 7 ----
  "coefplot",
  ### Chapter 8 ----
  "RColorBrewer",
  "cluster",
  ## Tidyverse approach ----
  ### Data science ----
  "tidyverse",
  ### Tidying ----
  "janitor",
  ### Modeling and machine learning ----
  "tidymodels",
  "corrr",
  "hardhat",
  #### For using n_clusters
  "easystats", # for checking linear model assumptions
  "NbClust",
  "mclust",
  "factoextra",
  ### Visualization ----
  "dotwhisker",
  "tidyheatmaps",
  "ggbiplot",
  "ggforce",
  "ggrepel",
  "patchwork",
  ### Summary statistics ----
  "skimr",
  ### Prepare REPRoducible EXamples ----
  "reprex",
  ### Tables ----
  #### HTML tables ----
  "DT",
  "gt",
  ### Data: bike_sales
  "sweep",
  ### Images ----
  "imager",
  ### Quarto ----
  "knitr",
  "rmarkdown",
  ### Maps ----
  "tigris",
  ### LaTeX ----
  "latex2exp",
  "tinytex"
)

# Installation Logic ----
# We check against the specific global library path
installed <- rownames(installed.packages(lib.loc = target_lib))

for (package in packages) {
  if (!(package %in% installed)) {
    message(paste("Installing:", package))
    install.packages(
      package,
      lib = target_lib,
      repos = my_repo,
      dependencies = TRUE
    )
  } else {
    message(paste("Skipping:", package, "(already installed globally)"))
  }
}
