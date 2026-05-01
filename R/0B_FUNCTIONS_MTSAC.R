# ===== FUNCTIONS TO SETUP MTSAC ANALYSIS =====

func_sample_size_MTSAC <- function(df_F,df_M) {
     counts_MTSAC <- data.frame(
     F_MTSAC = length(df_F$runner_name),
     F_races_MTSAC = pull(count(distinct(df_F,race_name))),
     F_unique_MTSAC = pull(count(distinct(df_F,runner_name))),
     M_MTSAC = length(df_M$runner_name),
     M_races_MTSAC = pull(count(distinct(df_M,race_name))),
     M_unique_MTSAC = pull(count(distinct(df_M,runner_name))))
     
     return(counts_MTSAC)
     
}


func_summary_MTSAC_data <- function(dfx) {
  
  dfx <- dfx %>%
    
    reframe(
      mean_FTS = mean(finish_time_seconds,na.rm = T),
      median_FTS = median(finish_time_seconds,na.rm = T),
      mean_AQI = mean(AQI_daily_on_race_day,na.rm = T),
      mean_AQI_14 = mean(linear_school_AQI_14,na.rm = T),
      mean_C = mean(temper_racetime,na.rm = T),
      mean_PM25 = mean(PM25_daily_on_race_day,na.rm = T))
  
  return(dfx)
  
}

func_site_10AM_MTSAC <- function(df_F_or_M) {
     df_10AM <- df_F_or_M %>%
          filter(ref_hour == 10) %>%
          reframe(ozone_10AM = mean(ozone_racetime, na.rm = T),
                  temper_10AM = mean(temper_racetime, na.rm = T))
          
     message("mean 10 AM ozone (PPM) = ", round(df_10AM$ozone_10AM,4))
     message("mean 10 AM temper (C) = ", round(df_10AM$temper_10AM,1))

}

func_filters_run_MTSAC <- function(df_F_or_M) {
     
  # define factors    
     df_F_or_M$grade <- as.factor(df_F_or_M$grade)
     
     df_F_or_M$state <- as.factor(df_F_or_M$state)
     
     df_F_or_M$course <- as.factor(df_F_or_M$course)
     
     df_F_or_M$weekday <- as.factor(df_F_or_M$weekday)
     
     df_F_or_M$gender <- as.factor(df_F_or_M$gender)
     
     df_F_or_M$start_time <- as.character(df_F_or_M$start_time)
     
     df_F_or_M <- df_F_or_M %>%
          mutate(division = fct_relevel(division,"D1_D2","D3","D4_D5"))
     
     df_F_or_M <- df_F_or_M %>% mutate(grade = fct_relevel(grade,"9","10","11","12"))
     
     df_F_or_M <- df_F_or_M %>% mutate(section = fct_relevel(section,
          "Southern","Central Coast","Central","Los Angeles","North Coast",
          "Northern","Oakland","San Diego","San Francisco","Sac Joaquin"))
     
  # define year_count   
  
     df_F_or_M <- df_F_or_M %>% mutate(year_count = year - min(year) + 1)
     
     
# drop missing race AQI_daily     

df_F_or_M <- df_F_or_M %>% filter(!is.na(AQI_daily_on_race_day))

# drop finish times > 1800 seconds

# set finish_time_seconds <= 30 * 60
max_time <- 30 * 60

df_F_or_M <- df_F_or_M %>% filter(finish_time_seconds <= max_time) 

# convert ozone_racetime from PPM to PPB
# create log values of ozone,temperature, PM25, and AQI_daily
# 

df_F_or_M <- filter(df_F_or_M,ozone_racetime > 0)
(length(df_F_or_M$race_name))

return(df_F_or_M)
                      
}

func_table_year_weekday_divison_MTSAC <- function(df_F_or_M) {
     
     gender <- as.character(df_F_or_M$gender[1])
     
     course <- as.character("MTSAC")
     
     table_YWD <- df_F_or_M %>% group_by(year,weekday,division) %>%
          summarise(n_of_runners = length(finish_time_seconds),
               mean_time = round(mean(finish_time_seconds,na.rm = T),1),
               sd_time = round(sd(finish_time_seconds,na.rm = T),1),
               mean_AQI = mean(AQI_daily_on_race_day,na.rm = T))
     
     gtable_YWD <- gt(table_YWD) %>%
          tab_caption(caption = paste0("Summary of ",gender," results at ",course)) %>%
          cols_label(
               year = "year",
               division = "division",
               n_of_runners = "Number<br>of runners",
               mean_time = "Mean time<br>(seconds)",
               sd_time = "Standard deviation<br>of time (seconds)",
               mean_AQI = "AQI<br>daily mean",
               .fn = md)
     
     return(gtable_YWD)

}

# Get coefficients and their standard errors

func_coefs_MTSAC <- function(reg_F,reg_M,yr,class) {
     coef_M <- coef(reg_M)
     coef_F <- coef(reg_F)
     se_M <- sqrt(diag(vcov(reg_M)))
     se_F <- sqrt(diag(vcov(reg_F)))
     
     # Calculate z-statistics for difference between coefficients
     z_stats <- (coef_F - coef_M) / sqrt(se_F^2 + se_M^2)
     
     # Calculate p-values
     p_values <- 2 * (1 - pnorm(abs(z_stats)))
     
     # Calculate difference and its standard error
     diff_FM <- coef_F - coef_M
     se_diff <- sqrt(diag(vcov(reg_F)) + diag(vcov(reg_M)))
     
     # Create comparison dataframe with p-values
     df_FM_MTSAC <- data.frame(
          Coefficient = names(coef_M),
          Male = coef_M,
          SE_M = se_M,
          Female = coef_F,
          SE_F = se_F,
          diff_FM,
          se_diff,
          Z_stat = z_stats,
          P_value = p_values)
     
     return(df_FM_MTSAC)}
     
     
func_stats_reg_MTSAC <- function(df_data,sry_reg) {
  
  text_stats <- df_data %>%
    reframe(mean_AQI = mean(AQI_daily_on_race_day,na.rm = T),
            sd_AQI = sd(AQI_daily_on_race_day,na.rm = T),
            One_SD_AQI = sd_AQI * coefficients(sry_reg)[3])
  
  return(text_stats)
  
}

func_sry_reg_F_MTSAC <- function(df_data,sry_reg) {
  
  sry_F_MTSAC_all <- df_data %>%
    group_by(year,division) %>%
    reframe(mean_finish_time = mean(finish_time_seconds,na.rm = T),
            SD_finish_time = sd(finish_time_seconds,na.rm = T),
            mean_AQI = mean(AQI_daily_on_race_day,na.rm = ),
            N_F_runners_MTSAC= n())
  
  return(sry_F_MTSAC_all)
  
}

func_sry_reg_M_MTSAC <- function(df_data,sry_reg) {
  
  sry_M_MTSAC_all <- df_data %>%
    group_by(year,division) %>%
    reframe(mean_finish_time = mean(finish_time_seconds,na.rm = T),
            SD_finish_time = sd(finish_time_seconds,na.rm = T),
            mean_AQI = mean(AQI_daily_on_race_day,na.rm = ),
            N_M_runners_MTSAC = n())
  
  return(sry_M_MTSAC_all)
  
}

func_AQ_table_MTSAC <- function(reg_F,reg_M,df_F, df_M) {
  
  # PM25
  sd_M_PM25_daily_mean <- sd(df_M$PM25_daily_on_race_day,na.rm = T)
  avg_M_PM25_daily_mean <- mean(df_M$PM25_daily_on_race_day,na.rm = T)
#  coef_M_PM25_daily_mean <- coef(reg_M)["PM25_daily_on_race_day"]
#  standard_M_PM25_effect <- sd_M_PM25_daily_mean * coef_M_PM25_daily_mean
  
  sd_F_PM25_daily_mean <- sd(df_F$PM25_daily_on_race_day,na.rm = T)
  avg_F_PM25_daily_mean <- mean(df_F$PM25_daily_on_race_day,na.rm = T)
#  coef_F_PM25_daily_mean <- coef(reg_F)["PM25_daily_on_race_day"]
#  standard_F_PM25_effect <- sd_F_PM25_daily_mean * coef_F_PM25_daily_mean
  
  # Ozone
  sd_M_ozone <- sd(df_M$ozone_racetime,na.rm = T)
  avg_M_ozone <- mean(df_M$ozone_racetime,na.rm = T)
  coef_M_ozone <- coef(reg_M)["ozone_racetime"]
  standard_M_ozone_effect <- sd_M_ozone * coef_M_ozone
  
  sd_F_ozone <- sd(df_F$ozone_racetime,na.rm = T)
  avg_F_ozone <- mean(df_F$ozone_racetime,na.rm = T)
  coef_F_ozone <- coef(reg_F)["ozone_racetime"]
  standard_F_ozone_effect <- sd_F_ozone * coef_F_ozone
  
  # Temper
  sd_M_temper <- sd(df_M$temper_racetime,na.rm = T)
  avg_M_temper <- mean(df_M$temper_racetime,na.rm = T)
  coef_M_temper <- coef(reg_M)["temper_racetime"]
  standard_M_temper_effect <- sd_M_temper * coef_M_temper
  
  sd_F_temper <- sd(df_F$temper_racetime,na.rm = T)
  avg_F_temper <- mean(df_F$temper_racetime,na.rm = T)
  coef_F_temper <- coef(reg_F)["temper_racetime"]
  standard_F_temper_effect <- sd_F_temper * coef_F_temper
  
  compare_F_M_AQ <- data.frame(
    
    SD_M_PM25 = round(sd_M_PM25_daily_mean,2),
    MN_M_PM25 = round(avg_M_PM25_daily_mean,1),
    
#    CF_M_PM25 = round(coef_F_PM25_daily_mean,1),
#    EF_M_PM25 = round(standard_M_PM25_effect,1),
    
    SD_F_PM25 = round(sd_F_PM25_daily_mean,2),
    MN_F_PM25 = round(avg_F_PM25_daily_mean,1),

#    CF_F_PM25 = round(coef_F_PM25_daily_mean,1),
#    EF_F_PM25 = round(standard_F_PM25_effect,1),
    
    SD_M_ozone = round(sd_M_ozone,5),
    MN_M_ozone = round(avg_M_ozone,5),
    CF_M_ozone = round(coef_M_ozone,1),
    EF_M_ozone = round(standard_M_ozone_effect,1),
    SD_F_ozone = round(sd_F_ozone,5),
    MN_F_ozone = round(avg_F_ozone,5),
    CF_F_ozone = round(coef_F_ozone,1),
    EF_F_ozone = round(standard_F_ozone_effect,1),
    
    SD_M_temper = round(sd_M_temper,2),
    MN_M_temper = round(avg_M_temper,1),
    CF_M_temper = round(coef_M_temper,1),
    EF_M_temper = round(standard_M_temper_effect,1),
    SD_F_temper = round(sd_F_temper,5),
    MN_F_temper = round(avg_F_temper,5),
    CF_F_temper = round(coef_F_temper,1),
    EF_F_temper = round(standard_F_temper_effect,1))
  
  return(compare_F_M_AQ)}
