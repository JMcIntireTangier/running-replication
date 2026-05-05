# REGRESSIONS FOR Mt SAC MALES, 2000 - 2023 ==============

df_M_MTSAC_readin <- readRDS("RUN_M_MTSAC_V2.RDS")

count_original_sample_M_MTSAC <- dim(df_M_MTSAC_readin)[1]


df_M_MTSAC <- func_filters_run_MTSAC(df_M_MTSAC_readin)


(count_valid_sample_M_MTSAC <- dim(df_M_MTSAC)[1])

(sample_erosion_M_MTSAC <- count_original_sample_M_MTSAC - count_valid_sample_M_MTSAC)

# ============ D1_D2, male  ========

# Calculate the 80th percentile threshold

df_M_MTSAC_D1D2 <- df_M_MTSAC %>% filter(division == "D1_D2")

threshold_M_MTSAC_D1D2_Q5  <- quantile(df_M_MTSAC_D1D2$finish_time_seconds, 
                                       probs = 0.2, na.rm = TRUE)

# Slice the dataframe to get rows in the top quintile

dfqnt_M_MTSAC_D1D2_Q5 <- df_M_MTSAC_D1D2[df_M_MTSAC_D1D2$finish_time_seconds
                                         <= threshold_M_MTSAC_D1D2_Q5, ]

# division D3, male

# Calculate the 80th percentile threshold

df_M_MTSAC_D3D3 <- df_M_MTSAC %>% filter(division == "D3")

threshold_M_MTSAC_D3D3_Q5  <- quantile(df_M_MTSAC_D3D3$finish_time_seconds, 
                                       probs = 0.2, na.rm = TRUE)

# Slice the data frame to get rows above the threshold
dfqnt_M_MTSAC_D3D3_Q5 <- df_M_MTSAC_D3D3[df_M_MTSAC_D3D3$finish_time_seconds <= threshold_M_MTSAC_D3D3_Q5, ]

# ===== division D4_D5, male =====

# Calculate the 80th percentile threshold

df_M_MTSAC_D4D5 <- df_M_MTSAC %>% filter(division == "D4_D5")

threshold_M_MTSAC_D4D5_Q5  <- quantile(df_M_MTSAC_D4D5$finish_time_seconds, 
                                       probs = 0.2, na.rm = TRUE)

lsdf_M_MTSAC_D <- mget(ls(pattern = "df_M_MTSAC_D\\d{1}D\\d{1}$"))

# Slice the dataframe to get rows above the threshold
dfqnt_M_MTSAC_D4D5_Q5 <- 
     df_M_MTSAC_D4D5[df_M_MTSAC_D4D5$finish_time_seconds <= threshold_M_MTSAC_D4D5_Q5, ]

# do the division regressions

reg_M_MTSAC <- feols(finish_time_seconds ~ 
    year_count + 
    ozone_racetime + 
    temper_racetime + 
#    PM25_daily_on_race_day +
#    linear_school_AQI_14 +
    program_score + 
    grade + 
    division, 
    cluster = ~ race_name + runner_id,
    data = df_M_MTSAC)

func_M_MTSAC_D <- function(df){
     feols(finish_time_seconds ~ 
      year_count + 
      ozone_racetime + 
      temper_racetime + 
#      PM25_daily_on_race_day +
#      linear_school_AQI_14 +
      program_score +
      grade, 
      cluster = ~ race_name + runner_id,
      data = df)}

reg_M_MTSAC_D1D2 <- func_M_MTSAC_D(df_M_MTSAC_D1D2)
reg_M_MTSAC_D3D3 <- func_M_MTSAC_D(df_M_MTSAC_D3D3)
reg_M_MTSAC_D4D5 <- func_M_MTSAC_D(df_M_MTSAC_D4D5)

lsreg_M_MTSAC_divs <- mget(ls(pattern = "reg_M_MTSAC_D\\d{1}D\\d{1}$"))

# top male quintile in D1D2

reg_M_MTSAC_D1D2_Q5 <- feols(finish_time_seconds ~
  year_count +
  ozone_racetime +
  temper_racetime +
#  PM25_daily_on_race_day +
#  linear_school_AQI_14 +
  program_score +
  grade,
  cluster = ~ race_name + runner_id,
  data = dfqnt_M_MTSAC_D1D2_Q5)
