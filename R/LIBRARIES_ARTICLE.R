# Core data manipulation and analysis
library(tidyverse)      # includes dplyr, tidyr, purrr, ggplot2, etc.
library(lubridate)      # dates and times
library(data.table)     # keep only if needed for specific merges; use data.table::

# Modeling and statistics
library(estimatr)       # lm_robust for clustered/robust SEs
library(broom)          # tidying model output
library(modelsummary)   # model summaries
library(lmtest)         # additional diagnostic tests
library(sandwich)       # robust covariance matrices
library(fixest)
library(e1071)

# Output and reporting
library(flextable)      # tables in Word
library(officer)        # Word document manipulation
library(officedown)     # Rmd to Word with custom templates
library(knitr)          # knitting options
library(rmarkdown)      # rendering (knitr already loads this, but fine)

# Visualization
library(ggplot2)
library(ggpubr)         # ggarrange
library(ggtext)         # rich text in ggplot
library(viridis)        # color scales
library(ggthemes)       # additional themes
library(scales)

# Data cleaning and preparation
library(janitor)        # clean_names(), etc.
library(stringr)        # string manipulation
library(forcats)        # factor manipulation
library(here)           # relative paths
library(skimr)          # quick data summaries
library(readxl)         # if you read Excel files

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

# Options

options(tibble.print.max = 10000)
options(timeout = 300)

