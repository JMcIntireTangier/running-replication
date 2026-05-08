
# 1. Load needed packages
if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  install.packages("rmarkdown")
}
if (!requireNamespace("fs", quietly = TRUE)) {
  install.packages("fs")
}

library(fs)

# Define paths
proj_root   <- getwd()  # run this script from the project root
rmd_file    <- path(proj_root, "MARKDOWN_RUNNING_CA.Rmd")
rep_dir     <- path(proj_root, "replication_package")

# Create replication package directory structure
dir_create(rep_dir)

# Copy code and data files that the Rmd sources
code_data_files <- c(
  "MAKE_REPLIC_PACKAGE.R",
  "MARKDOWN_RUNNING_CA.Rmd",
  
  "RUN_F_WP_V2.RDS",
  "RUN_M_WP_V2.RDS",
  "RUN_F_MTSAC_V2.RDS",
  "RUN_M_MTSAC_V2.RDS",
  "LIST_REG_DND.RDS",
 
  "SEND_MTSAC_AQI_PM25_DAILY.RDS",
  "SEND_MTSAC_OZONE_HOURLY_LT.RDS",
  "SEND_MTSAC_TEMPER_HOURLY_LT.RDS",
  
  "SEND_WP_AQI_PM25_DAILY.RDS",
  "SEND_WP_OZONE_HOURLY_LT.RDS",
  "SEND_WP_TEMPER_HOURLY_LT.RDS",
  
  "LIBRARIES_ARTICLE.R",
  "PALETTES_ARTICLE.R",
  
  "0A_FUNCTIONS_WP.R",
  "0B_FUNCTIONS_MTSAC.R",
  "0C_SETUP_AIR_QUALITY.R",
  "0D_MATCH_WP_MTSAC.R",
  
  "1A_RUN_FEMALE_WP.R",
  "1B_RUN_MALE_WP.R",
  "1C_TEXT_INSERT_WP.R",
  "2A_RUN_FEMALE_MTSAC.R",
  "2B_RUN_MALE_MTSAC.R",
  "2C_TEXT_INSERT_MTSAC.R",
  
  "3A_COMPARE_WP_MTSAC.R",
  
  "4A_RENDER_TABLES.R",
  "4B_RENDER_FIGURES.R",
  
  "TEST_ESTIMATION.R",
  "README.txt",

  "running_template_A.docx",      # Word template
  "nature-communications.csl",    # Citation style
  "running_references.bib")        # Bibliography

# Copy all files to root of replication package
file_copy(path(proj_root, code_data_files), 
          rep_dir, overwrite = TRUE)



# Save session info with timestamp
sink(path(rep_dir, "session_info.txt"))
cat("Replication package built on:", 
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
print(sessionInfo())
sink()
