# ===== TEXT INSERTIONS FOR MTSAC =====
# tables of sample sizes

counts_MTSAC <- func_sample_size_MTSAC(df_F_MTSAC, df_M_MTSAC)

counts_F_all = counts_MTSAC[1]

counts_M_all = counts_MTSAC[4]

counts_FM_MTSAC_all <- format(counts_MTSAC[1] + counts_MTSAC[4],big.mark = ",")

counts_MTSAC_D1D2_Q5 <- func_sample_size_MTSAC(dfqnt_F_MTSAC_D1D2_Q5,
     dfqnt_M_MTSAC_D1D2_Q5)


# ===== MTSAC 2002 - 2023 =====
# data summaries

sry_F_MTSAC <- func_summary_MTSAC_data(df_F_MTSAC)
sry_M_MTSAC <- func_summary_MTSAC_data(df_M_MTSAC)

sry_F_MTSAC_D1D2_Q5 <- func_summary_MTSAC_data(dfqnt_F_MTSAC_D1D2_Q5)
sry_M_MTSAC_D1D2_Q5 <- func_summary_MTSAC_data(dfqnt_M_MTSAC_D1D2_Q5)

# Get coefficients and their standard errors
coef_M_MTSAC <- coef(reg_M_MTSAC)
coef_F_MTSAC <- coef(reg_F_MTSAC)
se_M_MTSAC <- sqrt(diag(vcov(reg_M_MTSAC)))
se_F_MTSAC <- sqrt(diag(vcov(reg_F_MTSAC)))

coef_M_MTSAC_D1D2_Q5 <- coef(reg_M_MTSAC_D1D2_Q5)
coef_F_MTSAC_D1D2_Q5 <- coef(reg_F_MTSAC_D1D2_Q5)
se_M_MTSAC_D1D2_Q5 <- sqrt(diag(vcov(reg_M_MTSAC_D1D2_Q5)))
se_F_MTSAC_D1D2_Q5 <- sqrt(diag(vcov(reg_F_MTSAC_D1D2_Q5)))

# Calculate z-statistics for difference between coefficients

z_stats_MTSAC <- (coef_M_MTSAC - coef_F_MTSAC) / sqrt(se_M_MTSAC^2 + se_F_MTSAC^2)
z_stats_MTSAC_D1D2_Q5 <- (coef_M_MTSAC_D1D2_Q5 - coef_F_MTSAC_D1D2_Q5) / sqrt(se_M_MTSAC_D1D2_Q5^2 + se_F_MTSAC_D1D2_Q5^2)

# Calculate p-values

p_values_MTSAC <- 2 * (1 - pnorm(abs(z_stats_MTSAC)))
p_values_MTSAC_D1D2_Q5 <- 2 * (1 - pnorm(abs(z_stats_MTSAC_D1D2_Q5)))

# Calculate difference and its standard error

diff_FM_MTSAC <- coef_F_MTSAC - coef_M_MTSAC
se_diff_MTSAC <- sqrt(diag(vcov(reg_M_MTSAC)) + diag(vcov(reg_F_MTSAC)))

diff_FM_MTSAC_D1D2_Q5 <- coef_F_MTSAC_D1D2_Q5 - coef_M_MTSAC_D1D2_Q5
se_diff_MTSAC_D1D2_Q5 <- sqrt(diag(vcov(reg_M_MTSAC_D1D2_Q5)) + diag(vcov(reg_F_MTSAC_D1D2_Q5)))

# Create FM comparison dataframe with p-values

df_cmp_FM_MTSAC <- data.frame(
  Coefficient = names(coef_M_MTSAC),
  Male = round(coef_M_MTSAC,3),
  Male_SE = round(se_M_MTSAC,3),
  Female = round(coef_F_MTSAC,3),
  Female_SE = round(se_F_MTSAC,3),
  Z_stat = round(z_stats_MTSAC,3),
  P_value = round(p_values_MTSAC,3),
  diff_FM = round(diff_FM_MTSAC,3),
  se_diff = round(se_diff_MTSAC,3))

df_cmp_FM_MTSAC_D1D2_Q5 <- data.frame(
  Coefficient = names(coef_M_MTSAC_D1D2_Q5),
  Male = round(coef_M_MTSAC_D1D2_Q5,3),
  Male_SE = round(se_M_MTSAC_D1D2_Q5,3),
  Female = round(coef_F_MTSAC_D1D2_Q5,3),
  Female_SE = round(se_F_MTSAC_D1D2_Q5,3),
  Z_stat = round(z_stats_MTSAC_D1D2_Q5,3),
  P_value = round(p_values_MTSAC_D1D2_Q5,3),
  diff_FM = round(diff_FM_MTSAC_D1D2_Q5,2),
  se_diff = round(se_diff_MTSAC_D1D2_Q5,3))


# Format the table with kable, including p-value formatting
# NOT NEEDED NOW
# kbl_cmp_FM_MTSAC <- knitr::kable(df_cmp_FM_MTSAC, 
#   caption = paste("Regression coefs MTSAC, 2000 - 2023, all sample"),
#   row.names = FALSE,
#   format = "simple",
#   digits = c(0, 2, 3, 2,3,3,3,2,3),
#   format.args = list(scientific = FALSE))
# 

AQ_FM_MTSAC <- func_AQ_table_MTSAC(reg_F_MTSAC,reg_M_MTSAC,
                              df_F_MTSAC,df_M_MTSAC) %>% as_tibble()

AQ_FM_MTSAC_D1D2_Q5 <- 
    func_AQ_table_MTSAC(reg_F_MTSAC_D1D2_Q5,reg_M_MTSAC_D1D2_Q5,
  df_F_MTSAC,df_M_MTSAC) %>% as_tibble()

# get the all quintile regression coefficients
# to insert into the MTSAC figures

func_bar_Qall_MTSAC <- function(reg_FM,df_data,gnr,cf,tr) {
  # filter on a coefficient
  
 #  dfn <- deparse(substitute(df_data))
 #  
 # dfy <- as.numeric(str_extract_all(dfn, "\\d{4}")[[1]])
 #  
  
df_FM <- tidy(reg_FM,conf.int = TRUE) %>%
    
    filter(term == cf) %>%
    
    mutate(
      gender = gnr, 
      est_name = "2002 - 2023",
      type_row = tr,
      elapsed_years = ifelse(str_detect(tr,"2023"),21,1),
      statistic = statistic,
      stat_lo = (conf.low / std.error),
      stat_hi = (conf.high / std.error)) %>%
    
    select(gender,est_name,elapsed_years,type_row, term, 
           estimate,std.error,statistic,stat_lo, stat_hi)
  
  return(df_FM)
  
}     


# MTSAC, 2002-2023 with hourly values for ozone and temperature
#female

df_F_MTSAC_put_yr <- func_bar_Qall_MTSAC(reg_F_MTSAC,
        df_F_MTSAC,"F","year_count","delta 1 year")

df_F_MTSAC_put_ozone <- func_bar_Qall_MTSAC(reg_F_MTSAC,
           df_F_MTSAC,"F","ozone_racetime", "Ozone")

df_F_MTSAC_put_temper <- func_bar_Qall_MTSAC(reg_F_MTSAC,
            df_F_MTSAC,"F","temper_racetime","Temperature")

# df_F_MTSAC_put_PM25 <- func_bar_Qall_MTSAC(reg_F_MTSAC,
#           df_F_MTSAC,"F","PM25_daily_on_race_day","PM 2.5")

#
# df_F_MTSAC_put_AQI_14 <- func_bar_Qall_MTSAC(reg_F_MTSAC,
#           df_F_MTSAC,"F","linear_school_AQI_14","School AQI")

df_F_MTSAC_put_program <- func_bar_Qall_MTSAC(reg_F_MTSAC,
    df_F_MTSAC,"F","program_score","Program score")

df_F_MTSAC_put_grade <- func_bar_Qall_MTSAC(reg_F_MTSAC,
           df_F_MTSAC,"F","grade12", "delta grade 9-12")

df_F_MTSAC_put_div <- func_bar_Qall_MTSAC(reg_F_MTSAC,
         df_F_MTSAC,"F","divisionD4_D5","Small-large school")

#male

df_M_MTSAC_put_yr <- func_bar_Qall_MTSAC(reg_M_MTSAC,
        df_M_MTSAC,"M","year_count","delta 1 year")

df_M_MTSAC_put_ozone <- func_bar_Qall_MTSAC(reg_M_MTSAC,
           df_M_MTSAC,"M","ozone_racetime", "Ozone")

df_M_MTSAC_put_temper <- func_bar_Qall_MTSAC(reg_M_MTSAC,
            df_M_MTSAC,"M","temper_racetime","Temperature")

# df_M_MTSAC_put_PM25 <- func_bar_Qall_MTSAC(reg_M_MTSAC,
#           df_M_MTSAC,"M","PM25_daily_on_race_day","PM 2.5")

# df_M_MTSAC_put_AQI_14 <- func_bar_Qall_MTSAC(reg_M_MTSAC,
#           df_M_MTSAC,"M","linear_school_AQI_14","School AQI")

df_M_MTSAC_put_program <- func_bar_Qall_MTSAC(reg_M_MTSAC,
  df_M_MTSAC,"M","program_score","Program score")

df_M_MTSAC_put_grade <- func_bar_Qall_MTSAC(reg_M_MTSAC,
           df_M_MTSAC,"M","grade12", "delta grade 9-12")

df_M_MTSAC_put_div <- func_bar_Qall_MTSAC(reg_M_MTSAC,
         df_M_MTSAC,"M","divisionD4_D5","Small-large school")

df_bar_MTSAC_all_put <- rbind(
  
  df_F_MTSAC_put_yr,
  df_F_MTSAC_put_ozone,
  df_F_MTSAC_put_temper,
#  df_F_MTSAC_put_PM25,
#  df_F_MTSAC_put_AQI_14,
  df_F_MTSAC_put_program,
  df_F_MTSAC_put_grade,
  df_F_MTSAC_put_div,
  
  df_M_MTSAC_put_yr,
  df_M_MTSAC_put_ozone,
  df_M_MTSAC_put_temper,
#  df_M_MTSAC_put_PM25,
#  df_M_MTSAC_put_AQI_14,
  df_M_MTSAC_put_program,
  df_M_MTSAC_put_grade,
  df_M_MTSAC_put_div)

df_bar_MTSAC_put <- df_bar_MTSAC_all_put %>%
  pivot_longer(statistic:stat_hi,
               names_to = "type_stat",
               values_to = "value")

df_bar_MTSAC_put <- df_bar_MTSAC_put %>% 
  mutate(est_name = factor(est_name),
         term = factor(term),
         estimate = factor(estimate))

df_bar_MTSAC_put <- df_bar_MTSAC_put %>%
  filter(est_name == "2002 - 2023")

df_bar_MTSAC_put <- df_bar_MTSAC_put %>%
  mutate(type_row = fct_relevel(type_row,
"delta 1 year",
"Small-large school",
"delta grade 9-12",
"Ozone",
"Temperature",
"Program score"))

# write a function to get the regression coefficients
# from D1D2_Q5 data subset at MTSAC and at MTSAC

func_bar_Q5_MTSAC <- function(reg_FM,df_data,gnr,cf,tr) {
  # filter on a coefficient
  
  
  df_FM <- tidy(reg_FM,conf.int = TRUE) %>%
    
    filter(term == cf) %>%
    
    mutate(
      gender = gnr, 
      est_name = "D1 & D2, Q5",
      type_row = tr,
      elapsed_years = ifelse(str_detect(tr,"2023"),21,1),
      stat_lo = conf.low / std.error,
      stat_hi = conf.high / std.error) %>%
    select(gender, est_name, 
           elapsed_years, type_row, term, 
           estimate, std.error,
           statistic,stat_lo, stat_hi)
  
  return(df_FM)
  
}     

# MTSAC 2002 - 2023, D1 and D2, Q5
#female races
df_F_MTSAC_put_yr_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_F_MTSAC_D1D2_Q5,
              df_F_MTSAC_D1D2,"F","year_count","delta 1 year")

df_F_MTSAC_put_ozone_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_F_MTSAC_D1D2_Q5,
 df_F_MTSAC_D1D2,"F","ozone_racetime", "Ozone")

df_F_MTSAC_put_temper_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_F_MTSAC_D1D2_Q5,
  df_F_MTSAC_D1D2,"F","temper_racetime","Temperature")

# df_F_MTSAC_put_PM25_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_F_MTSAC_D1D2_Q5,
# df_F_MTSAC_D1D2,"F","PM25_daily_on_race_day","PM 2.5")

# df_F_MTSAC_put_school14_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_F_MTSAC_D1D2_Q5,
#     df_F_MTSAC_D1D2,"F","linear_school_AQI_14","School AQI")

df_F_MTSAC_put_program_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_F_MTSAC_D1D2_Q5,
  df_F_MTSAC_D1D2,"F","program_score","Program score")

df_F_MTSAC_put_grade_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_F_MTSAC_D1D2_Q5,
 df_F_MTSAC_D1D2,"F","grade12", "delta grade 9-12")

#male races
df_M_MTSAC_put_yr_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_M_MTSAC_D1D2_Q5,
              df_M_MTSAC_D1D2,"M","year_count","delta 1 year")

df_M_MTSAC_put_ozone_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_M_MTSAC_D1D2_Q5,
 df_M_MTSAC_D1D2,"M","ozone_racetime","Ozone")

df_M_MTSAC_put_temper_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_M_MTSAC_D1D2_Q5,
  df_M_MTSAC_D1D2,"M","temper_racetime","Temperature")

# df_M_MTSAC_put_PM25_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_M_MTSAC_D1D2_Q5,
# df_M_MTSAC_D1D2,"M","PM25_daily_on_race_day","PM 2.5")

# df_M_MTSAC_put_school14_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_M_MTSAC_D1D2_Q5,
#     df_M_MTSAC_D1D2,"M","linear_school_AQI_14","School AQI")

df_M_MTSAC_put_program_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_M_MTSAC_D1D2_Q5,
  df_M_MTSAC_D1D2,"M","program_score","Program score")

df_M_MTSAC_put_grade_D1D2_Q5 <- func_bar_Q5_MTSAC(reg_M_MTSAC_D1D2_Q5,
 df_M_MTSAC_D1D2,"M","grade12","delta grade 9-12")

df_bar_MTSAC_put_D1D2_Q5 <- rbind(
  df_F_MTSAC_put_yr_D1D2_Q5,
  df_F_MTSAC_put_ozone_D1D2_Q5,
  df_F_MTSAC_put_temper_D1D2_Q5,
#  df_F_MTSAC_put_PM25_D1D2_Q5,
#  df_F_MTSAC_put_school14_D1D2_Q5,
  df_F_MTSAC_put_program_D1D2_Q5,
  df_F_MTSAC_put_grade_D1D2_Q5,
  
  df_M_MTSAC_put_yr_D1D2_Q5,
  df_M_MTSAC_put_ozone_D1D2_Q5,
  df_M_MTSAC_put_temper_D1D2_Q5,
#  df_M_MTSAC_put_PM25_D1D2_Q5,
#  df_M_MTSAC_put_school14_D1D2_Q5,
  df_M_MTSAC_put_program_D1D2_Q5,
  df_M_MTSAC_put_grade_D1D2_Q5)

df_bar_MTSAC_put_D1D2_Q5 <- df_bar_MTSAC_put_D1D2_Q5 %>%
  pivot_longer(statistic:stat_hi,
               names_to = "type_stat",
               values_to = "value")

df_bar_MTSAC_put_D1D2_Q5 <- df_bar_MTSAC_put_D1D2_Q5 %>% 
  mutate(est_name = factor(est_name),
         term = factor(term))

df_bar_MTSAC_put_D1D2_Q5 <- df_bar_MTSAC_put_D1D2_Q5 %>%
  filter(est_name == "D1 & D2, Q5")

df_bar_MTSAC_put_D1D2_Q5 <- df_bar_MTSAC_put_D1D2_Q5 %>%
  mutate(type_row = factor(type_row))

df_bar_MTSAC_put_D1D2_Q5 <- df_bar_MTSAC_put_D1D2_Q5 %>%
  mutate(type_row = fct_relevel(type_row,
  "delta 1 year",
  "delta grade 9-12",   
  "Ozone",
  "Temperature",
  "Program score"))

df_F_MTSAC %>%
  group_by(division, grade) %>%
  summarise(
    mean_time = mean(finish_time_seconds, na.rm = TRUE),
    se_coef = sd(finish_time_seconds, na.rm = TRUE),
    n = n())

df_M_MTSAC %>%
  group_by(division, grade) %>%
  summarise(
    mean_time = mean(finish_time_seconds, na.rm = TRUE),
    se_coef = sd(finish_time_seconds, na.rm = TRUE),
    n = n())

list_main_dfs_MTSAC <- list(
  df_F_MTSAC   = df_F_MTSAC,
  df_M_MTSAC   = df_M_MTSAC)

#return from lapply should all be ungrouped
lapply(list_main_dfs_MTSAC,class)


# ========== test for effects of missing values on PM25_daily_on_race_day ==========

dfs_MTSAC_fit_PM25 <- list(
                        F_MTSAC = df_F_MTSAC,
                        F_MTSAC_D1D2_Q5 = dfqnt_F_MTSAC_D1D2_Q5,
                        
                        M_MTSAC = df_M_MTSAC,
                        M_MTSAC_D1D2_Q5 = dfqnt_M_MTSAC_D1D2_Q5)

func_fit_MTSAC_PM25 <- function(dfm, dfm_name) {
  
  fit_without_PM25 <- feols(
    finish_time_seconds ~ year_count +
      ozone_racetime +
      temper_racetime +
      grade,
    cluster = ~ race_name + runner_id,
    data     = dfm)
  
  fit_with_PM25 <- feols(
    finish_time_seconds ~ year_count +
      ozone_racetime +
      temper_racetime +
      PM25_daily_on_race_day +
      grade,
    cluster = ~ race_name + runner_id,
    data     = dfm)
  
  n_without <- nobs(fit_without_PM25)
  n_with    <- nobs(fit_with_PM25)
  adj_R2_without = summary(fit_without_PM25)[["adj.r.squared"]]
  adj_R2_with = summary(fit_with_PM25)[["adj.r.squared"]]
  
  PM25_fit_table <- tibble(
    
    dataset = dfm_name,
    
    model  = c("without PM25", "with PM25"),
    
    n      = c(n_without, n_with),
    
    R2 = c(adj_R2_without,adj_R2_with),
    
    
    cf_year = c(coef(fit_without_PM25)[["year_count"]],
                coef(fit_with_PM25)[["year_count"]]),
    
    se_year = c(
      sqrt(diag(vcov(fit_without_PM25)))["year_count"],
      sqrt(diag(vcov(fit_with_PM25)))["year_count"]),
    
    p_year = c(fit_without_PM25$p.value[["year_count"]],
               fit_with_PM25$p.value[["year_count"]]),
    
    cf_temp = c(coef(fit_without_PM25)[["temper_racetime"]],
                coef(fit_with_PM25)[["temper_racetime"]]),
    
    se_temp = c(
      sqrt(diag(vcov(fit_without_PM25)))["temper_racetime"],
      sqrt(diag(vcov(fit_with_PM25)))["temper_racetime"]),
    
    p_temp = c(fit_without_PM25$p.value[["temper_racetime"]],
               fit_with_PM25$p.value[["temper_racetime"]]),
    
    cf_ozone = c(coef(fit_without_PM25)[["ozone_racetime"]], coef(fit_with_PM25)[["ozone_racetime"]]),
    
    se_ozone = c(
      sqrt(diag(vcov(fit_without_PM25)))["ozone_racetime"],
      sqrt(diag(vcov(fit_with_PM25)))["ozone_racetime"]),
    
    p_ozone = c(fit_without_PM25$p.value[["ozone_racetime"]],
                fit_with_PM25$p.value[["ozone_racetime"]]),
    
    cf_PM25 = c(NA_real_, coef(fit_with_PM25)[["PM25_daily_on_race_day"]]),
    
    se_PM25 = c(NA_real_,
                sqrt(diag(vcov(fit_with_PM25)))[["PM25_daily_on_race_day"]]),
    
    p_PM25 = c(NA_real_,
               fit_with_PM25$p.value[["PM25_daily_on_race_day"]]))}


sry_fit_MTSAC_PM25 <- lapply(names(dfs_MTSAC_fit_PM25), function(nm) {
  func_fit_MTSAC_PM25(dfs_MTSAC_fit_PM25[[nm]], dfm_name = nm)})

# Bind the results 

(PM25_fit_MTSAC <- as_tibble(do.call(rbind, sry_fit_MTSAC_PM25)))

PM25_fit_MTSAC <- PM25_fit_MTSAC %>% 
  mutate(
    # explicit ordering of datasets; adjust if you prefer another order
    dataset = factor(
      dataset,
      levels = c(
        "F_MTSAC",
        "F_MTSAC_D1D2_Q5",
        
        "M_MTSAC",
        "M_MTSAC_D1D2_Q5")))

PM25_fit_MTSAC <- PM25_fit_MTSAC %>%
  mutate(
    gender = factor(case_when(
      grepl("F_", dataset) ~ "Female",
      grepl("M_", dataset) ~ "Male",
      TRUE ~ NA_character_)))

PM25_fit_MTSAC <- PM25_fit_MTSAC %>%
  select(dataset,gender,model,everything())

