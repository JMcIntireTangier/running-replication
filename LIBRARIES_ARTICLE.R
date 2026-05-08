# ===== LIBRARIES_ARTICLE.R =====
# Silent package loader with version and dependency checks

# Check R version
r_version_required <- "4.6.0"
current_version <- paste0(R.version$major, ".", R.version$minor)

if (compareVersion(as.character(current_version), r_version_required) < 0) {
  stop(
    "This code requires R version ", r_version_required, " or higher.\n",
    "You are running R version ", current_version, ".\n",
    "Please update R from https://cran.r-project.org/"
  )
}

# List required packages
required_packages <- c(
  "tidyverse",
  "fixest",
  "modelsummary",
  "flextable",
  "knitr",
  "rmarkdown",
  "here",
  "janitor",
  "scales",
  "ggpubr",
  "viridis",
  "ggplot2",
  "officer",
  "officedown",
  "ggtext",
  "broom",
  "estimatr",
  "sandwich",
  "lmtest",
  "patchwork",
  "kableExtra",
  "gt",
  "gtsummary"
)

# Check and install missing packages
packages_to_install <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(packages_to_install) > 0) {
  message("The following packages are missing and will be installed:\n",
          paste(packages_to_install, collapse = ", "))
  install.packages(packages_to_install, dependencies = TRUE)
}

# Load all packages silently
suppressPackageStartupMessages({
  for (pkg in required_packages) {
    library(pkg, character.only = TRUE)
  }
})

# Set options
options(tibble.print.max = 10000)
options(timeout = 300)
options(dplyr.summarise.inform = FALSE)
options(tidyverse.quiet = TRUE)


# Confirmation message (optional)
message("All required packages loaded successfully. R version ", 
        current_version, " (required: ", r_version_required, "+)")

# List all required packages
required_packages <- c(
# Data manipulation and analysis
  
  "tidyverse", # includes dplyr, tidyr, purrr, ggplot2, etc.
  "lubridate",

# Modeling and statistics
  
  "estimatr", # lm_robust for clustered/robust SEs
  "broom",
  "modelsummary",
  "lmtest",
  "sandwich",
  "fixest",
  "e1071",

# Output and reporting

  "flextable",
  "officer",
  "officedown",
  "knitr",
  "rmarkdown",
  
# Visualization

  "ggplot2",
  "ggpubr",
  "ggtext",
  "viridis",
  "ggthemes",
  "scales",

# Data cleaning and preparation

  "stringr",
  "forcats",
  "here",
  "readxl")

# Load all packages silently
suppressPackageStartupMessages({
  for (pkg in required_packages) {
    library(pkg, character.only = TRUE)}})

# Optional: Print confirmation of what was loaded
# cat("Loaded", length(required_packages), "packages\n")

options(tibble.print.max = 10000)
options(timeout = 300)

# Optional — keep only if used
# library(geosphere)    # for distance calculations
# library(circular)     # for circular stats
# library(rvest)        # for web scraping
# library(tidymodels)   # not needed for OLS
# library(sensemakr)    # not in your Rmd shown
# library(stargazer)    # you use modelsummary instead
# library(ggbeeswarm)   # only if you make specific plots
# library(lemon)        # only if you need it
# library(doBy)         # base R and dplyr can do this
# data.table            # not used in this project
# skimr                 # not used in this project