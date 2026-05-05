# REGRESSIONS FOR Mt SAC FEMALES, 2000 - 2023 ==============

# temporary function to print changes in sample sizes
# after each filter

df_F_MTSAC_readin <- readRDS("RUN_F_MTSAC_V2.RDS")

(count_original_sample_F_MTSAC <- dim(df_F_MTSAC_readin)[1])

df_F_MTSAC <- func_filters_run_MTSAC(df_F_MTSAC_readin)

         
(count_valid_sample_F_MTSAC <- dim(df_F_MTSAC)[1])

(sample_erosion_F_MTSAC <- count_original_sample_F_MTSAC - count_valid_sample_F_MTSAC)


# ============ D1_D2, female  ========

# Calculate the 80th percentile threshold

df_F_MTSAC_D1D2 <- df_F_MTSAC %>% filter(division == "D1_D2")

threshold_F_MTSAC_D1D2_Q5  <- quantile(df_F_MTSAC_D1D2$finish_time_seconds, 
     probs = 0.2, na.rm = TRUE)

# Slice the dataframe to get rows in the top quintile

dfqnt_F_MTSAC_D1D2_Q5 <- df_F_MTSAC_D1D2[df_F_MTSAC_D1D2$finish_time_seconds
     <= threshold_F_MTSAC_D1D2_Q5, ]

# division D3, female

# Calculate the 80th percentile threshold

df_F_MTSAC_D3D3 <- df_F_MTSAC %>% filter(division == "D3")

threshold_F_MTSAC_D3D3_Q5  <- quantile(df_F_MTSAC_D3D3$finish_time_seconds, 
     probs = 0.2, na.rm = TRUE)

# Slice the data frame to get rows above the threshold
dfqnt_F_MTSAC_D3D3_Q5 <- df_F_MTSAC_D3D3[df_F_MTSAC_D3D3$finish_time_seconds <= threshold_F_MTSAC_D3D3_Q5, ]

# ===== division D4_D5, female =====

# Calculate the 80th percentile threshold

df_F_MTSAC_D4D5 <- df_F_MTSAC %>% filter(division == "D4_D5")

threshold_F_MTSAC_D4D5_Q5  <- quantile(df_F_MTSAC_D4D5$finish_time_seconds, 
     probs = 0.2, na.rm = TRUE)

lsdf_F_MTSAC_D <- mget(ls(pattern = "df_F_MTSAC_D\\d{1}D\\d{1}$"))

# Slice the dataframe to get rows above the threshold
dfqnt_F_MTSAC_D4D5_Q5 <- 
     df_F_MTSAC_D4D5[df_F_MTSAC_D4D5$finish_time_seconds <= threshold_F_MTSAC_D4D5_Q5, ]

# do the division regressions

reg_F_MTSAC <- feols(finish_time_seconds ~ 
     year_count + 
     ozone_racetime + 
     temper_racetime + 
#     PM25_daily_on_race_day +
#     linear_school_AQI_14 +
     program_score + 
     grade + 
     division, 
     cluster = ~ race_name + runner_id,
     data = df_F_MTSAC)

func_F_MTSAC_D <- function(df){
     feols(finish_time_seconds ~ 
        year_count + 
             ozone_racetime + 
             temper_racetime + 
#             PM25_daily_on_race_day +
#             linear_school_AQI_14 +
             program_score +
             grade, 
             cluster = ~ race_name + runner_id,
             data = df)}

reg_F_MTSAC_D1D2 <- func_F_MTSAC_D(df_F_MTSAC_D1D2)
reg_F_MTSAC_D3D3 <- func_F_MTSAC_D(df_F_MTSAC_D3D3)
reg_F_MTSAC_D4D5 <- func_F_MTSAC_D(df_F_MTSAC_D4D5)

lsreg_F_MTSAC_divs <- mget(ls(pattern = "reg_F_MTSAC_D\\d{1}D\\d{1}$"))

# top female quintile in D1D2

reg_F_MTSAC_D1D2_Q5 <- feols(finish_time_seconds ~
     year_count +
     ozone_racetime +
     temper_racetime +
#     PM25_daily_on_race_day +
#     linear_school_AQI_14 +
     program_score +
     grade,
     cluster = ~ race_name + runner_id,
     data = dfqnt_F_MTSAC_D1D2_Q5)

