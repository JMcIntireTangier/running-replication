#  ===== TEXT INSERTIONS FOR WP =====
# ===== Woodward Park 1987 - 2023 =====


# show sample sizes

counts_WP <- func_sample_size_WP(df_F_WP_1987_2023, df_M_WP_1987_2023)

counts_FM_WP_all = format(counts_WP[1] + counts_WP[4],big.mark = ",")

# top quintile
 
counts_WP_Q5 <- func_sample_size_WP(dfqnt_F_WP_1987_2023_D1D2_Q5, 
     dfqnt_M_WP_1987_2023_D1D2_Q5)


# sample size by division
 
counts_WP_divison <- func_sample_size_WP_by_division(df_F_WP_1987_2023,
                    df_M_WP_1987_2023)

# derive summary values for all sample and D1D2_Q5

sry_F_WP_1987_2023 <- func_summary_WP_data_1987_2023(df_F_WP_1987_2023)
sry_F_WP_1987_2023_D1D2_Q5 <- func_summary_WP_data_1987_2023(dfqnt_F_WP_1987_2023_D1D2_Q5)

sry_M_WP_1987_2023 <- func_summary_WP_data_1987_2023(df_M_WP_1987_2023)
sry_M_WP_1987_2023_D1D2_Q5 <- func_summary_WP_data_1987_2023(dfqnt_M_WP_1987_2023_D1D2_Q5)

# Get coefficients and their standard errors

coef_F_WP_1987_2023 <- coef(reg_F_WP_1987_2023)
se_F_WP_1987_2023 <- sqrt(diag(vcov(reg_F_WP_1987_2023)))

coef_F_WP_1987_2023_D1D2_Q5 <- coef(reg_F_WP_1987_2023_D1D2_Q5)
se_F_WP_1987_2023_D1D2_Q5 <- sqrt(diag(vcov(reg_F_WP_1987_2023_D1D2_Q5)))

coef_M_WP_1987_2023 <- coef(reg_M_WP_1987_2023)
se_M_WP_1987_2023 <- sqrt(diag(vcov(reg_M_WP_1987_2023)))

coef_M_WP_1987_2023_D1D2_Q5 <- coef(reg_M_WP_1987_2023_D1D2_Q5)
se_M_WP_1987_2023_D1D2_Q5 <- sqrt(diag(vcov(reg_M_WP_1987_2023_D1D2_Q5)))

# Calculate z-statistics for difference between coefficients

z_stats_WP_1987_2023 <- (coef_M_WP_1987_2023 - coef_F_WP_1987_2023) / 
     sqrt(se_M_WP_1987_2023^2 + se_F_WP_1987_2023^2)

z_stats_WP_1987_2023_D1D2_Q5 <- (coef_M_WP_1987_2023_D1D2_Q5 - coef_F_WP_1987_2023_D1D2_Q5) / 
     sqrt(se_M_WP_1987_2023_D1D2_Q5^2 + se_F_WP_1987_2023_D1D2_Q5^2)

# Calculate p-values

p_values_WP_1987_2023 <- 2 * (1 - pnorm(abs(z_stats_WP_1987_2023)))
p_values_WP_1987_2023_D1D2_Q5 <- 2 * (1 - pnorm(abs(z_stats_WP_1987_2023_D1D2_Q5)))

# Calculate difference and its standard error

diff_FM_WP_1987_2023 <- coef_F_WP_1987_2023 - coef_M_WP_1987_2023
se_diff_WP_1987_2023 <- sqrt(diag(vcov(reg_M_WP_1987_2023)) + diag(vcov(reg_F_WP_1987_2023)))

diff_FM_WP_1987_2023_D1D2_Q5 <- coef_F_WP_1987_2023_D1D2_Q5 - coef_M_WP_1987_2023_D1D2_Q5
se_diff_WP_1987_2023_D1D2_Q5 <- sqrt(diag(vcov(reg_M_WP_1987_2023_D1D2_Q5)) + diag(vcov(reg_F_WP_1987_2023_D1D2_Q5)))

# Create comparison dfs with p-values

df_cmp_FM_WP_1987_2023 <- data.frame(
     Coefficient = names(coef_M_WP_1987_2023),
     Male = round(coef_M_WP_1987_2023,2),
     Male_SE = round(se_M_WP_1987_2023,3),
     Female = round(coef_F_WP_1987_2023,2),
     Female_SE = round(se_F_WP_1987_2023,3),
     Z_stat = round(z_stats_WP_1987_2023,3),
     P_value = round(p_values_WP_1987_2023,3),
     diff_FM = round(diff_FM_WP_1987_2023,2),
     se_diff = round(se_diff_WP_1987_2023,3))

df_cmp_FM_WP_1987_2023_D1D2_Q5 <- data.frame(
     Coefficient = names(coef_M_WP_1987_2023_D1D2_Q5),
     Male = round(coef_M_WP_1987_2023_D1D2_Q5,2),
     Male_SE = round(se_M_WP_1987_2023_D1D2_Q5,3),
     Female = round(coef_F_WP_1987_2023_D1D2_Q5,2),
     Female_SE = round(se_F_WP_1987_2023_D1D2_Q5,3),
     Z_stat = round(z_stats_WP_1987_2023_D1D2_Q5,3),
     P_value = round(p_values_WP_1987_2023_D1D2_Q5,3),
     diff_FM = round(diff_FM_WP_1987_2023_D1D2_Q5,2),
     se_diff = round(se_diff_WP_1987_2023_D1D2_Q5,3))

# Format comparison df with kable for presentation
# not needed for 
# Format the table with kable, including p-value formatting
# kbl_cmp_FM_WP_1987_2023 <- kable(df_cmp_FM_WP_1987_2023, 
#                                   caption = paste("Regression coefs FM at WP,1987 - 2023, all sample"),
#                                   row.names = FALSE,
#                                   format = "simple",
#                                   digits = c(0, 2, 3, 2,3,3,3,2,3),
#                                   format.args = list(scientific = FALSE))



# ===== Woodward Park 2000 - 2023 =====

# sample sizes
 
counts_WP_2000 <- func_sample_size_WP(df_F_WP_2000_2023, df_M_WP_2000_2023)
counts_WP_2000_D1D2_Q5 <- func_sample_size_WP(dfqnt_F_WP_2000_2023_D1D2_Q5,
     dfqnt_M_WP_2000_2023_D1D2_Q5)

sry_F_WP_2000_2023 <- func_summary_WP_data_2000_2023(df_F_WP_2000_2023)
sry_M_WP_2000_2023 <- func_summary_WP_data_2000_2023(df_M_WP_2000_2023)

sry_F_WP_2000_2023_D1D2_Q5 <- func_summary_WP_data_2000_2023(dfqnt_F_WP_2000_2023_D1D2_Q5)
sry_M_WP_2000_2023_D1D2_Q5 <- func_summary_WP_data_2000_2023(dfqnt_M_WP_2000_2023_D1D2_Q5)

# Get coefficients and their standard errors
 
coef_M_WP_2000_2023 <- coef(reg_M_WP_2000_2023)
coef_F_WP_2000_2023 <- coef(reg_F_WP_2000_2023)
se_M_WP_2000_2023 <- sqrt(diag(vcov(reg_M_WP_2000_2023)))
se_F_WP_2000_2023 <- sqrt(diag(vcov(reg_F_WP_2000_2023)))

coef_M_WP_2000_2023_D1D2_Q5 <- coef(reg_M_WP_2000_2023_D1D2_Q5)
coef_F_WP_2000_2023_D1D2_Q5 <- coef(reg_F_WP_2000_2023_D1D2_Q5)
se_M_WP_2000_2023_D1D2_Q5 <- sqrt(diag(vcov(reg_M_WP_2000_2023_D1D2_Q5)))
se_F_WP_2000_2023_D1D2_Q5 <- sqrt(diag(vcov(reg_F_WP_2000_2023_D1D2_Q5)))

# Calculate z-statistics for difference between coefficients

z_stats_WP_2000_2023 <- (coef_M_WP_2000_2023 - coef_F_WP_2000_2023) / sqrt(se_M_WP_2000_2023^2 + se_F_WP_2000_2023^2)
z_stats_WP_2000_2023_D1D2_Q5 <- (coef_M_WP_2000_2023_D1D2_Q5 - coef_F_WP_2000_2023_D1D2_Q5) / sqrt(se_M_WP_2000_2023_D1D2_Q5^2 + se_F_WP_2000_2023_D1D2_Q5^2)

# Calculate p-values

p_values_WP_2000_2023 <- 2 * (1 - pnorm(abs(z_stats_WP_2000_2023)))
p_values_WP_2000_2023_D1D2_Q5 <- 2 * (1 - pnorm(abs(z_stats_WP_2000_2023_D1D2_Q5)))

# Calculate difference and its standard error

diff_FM_WP_2000_2023 <- coef_F_WP_2000_2023 - coef_M_WP_2000_2023
se_diff_WP_2000_2023 <- sqrt(diag(vcov(reg_M_WP_2000_2023)) + diag(vcov(reg_F_WP_2000_2023)))

diff_FM_WP_2000_2023_D1D2_Q5 <- coef_F_WP_2000_2023_D1D2_Q5 - coef_M_WP_2000_2023_D1D2_Q5
se_diff_WP_2000_2023_D1D2_Q5 <- sqrt(diag(vcov(reg_M_WP_2000_2023_D1D2_Q5)) + diag(vcov(reg_F_WP_2000_2023_D1D2_Q5)))

# Create FM comparison df for WP_2000_2023

df_cmp_FM_WP_2000_2023 <- data.frame(
     Coefficient = names(coef_M_WP_2000_2023),
     Male = round(coef_M_WP_2000_2023,1),
     Male_SE = round(se_M_WP_2000_2023,2),
     Female = round(coef_F_WP_2000_2023,1),
     Female_SE = round(se_F_WP_2000_2023,2),
     Z_stat = round(z_stats_WP_2000_2023,2),
     P_value = round(p_values_WP_2000_2023,3),
     diff_FM = round(diff_FM_WP_2000_2023,2),
     se_diff = round(se_diff_WP_2000_2023,3))

# Format the df with kable
# # not needed for now

# kbl_FM_WP_2000_2023 <- knitr::kable(df_cmp_FM_WP_2000_2023, 
#   caption = paste("Regression coefs FM at WP,2000 - 2023, all sample"),
#   row.names = FALSE,
#   format = "simple",
#   digits = c(0, 2, 3, 2,3,3,3,2,3),
#   format.args = list(scientific = FALSE))

AQ_FM_WP_2000_2023 <- func_AQ_table_WP(reg_F_WP_2000_2023,reg_M_WP_2000_2023,
      df_F_WP_2000_2023,df_M_WP_2000_2023) %>% as.data.frame()

# Create FM comparison df for WP_2000_2023_D1D2_Q5

df_FM_WP_2000_2023_D1D2_Q5 <- data.frame(
  Coefficient = names(coef_M_WP_2000_2023_D1D2_Q5),
  Male = round(coef_M_WP_2000_2023_D1D2_Q5,1),
  Male_SE = round(se_M_WP_2000_2023_D1D2_Q5,2),
  Female = round(coef_F_WP_2000_2023_D1D2_Q5,1),
  Female_SE = round(se_F_WP_2000_2023_D1D2_Q5,2),
  Z_stat = round(z_stats_WP_2000_2023_D1D2_Q5,2),
  P_value = round(p_values_WP_2000_2023_D1D2_Q5,3),
  diff_FM = round(diff_FM_WP_2000_2023_D1D2_Q5,2),
  se_diff = round(se_diff_WP_2000_2023_D1D2_Q5,3))

# Format the table with kable for presentations
# NOT NEEDED FOR NOW

# kbl_FM_WP_2000_2023_D1D2_Q5 <- knitr::kable(df_FM_WP_2000_2023_D1D2_Q5, 
#   caption = paste("Regression coefs FM at WP, 2000 - 2023, D1D2, Q5"),
#   row.names = FALSE,
#   format = "simple",
#   digits = c(0, 2, 3, 2,3,3,3,2,3),
#   format.args = list(scientific = FALSE))

AQ_FM_WP_2000_2023_D1D2_Q5 <- 
          func_AQ_table_WP(reg_F_WP_2000_2023_D1D2_Q5,reg_M_WP_2000_2023_D1D2_Q5,
                        df_F_WP_2000_2023,df_M_WP_2000_2023) %>% as.data.frame()

# get the all quintile regression coefficients
# to insert into the WP figures

func_bar_Qall_WP <- function(reg_FM,df_data,gnr,cf,tr) {
     # filter on a coefficient
     
     dfn <- deparse(substitute(df_data))
     
     dfy <- as.numeric(str_extract_all(dfn, "\\d{4}")[[1]])
     
     df_FM <- tidy(reg_FM,conf.int = TRUE) %>%
          
          filter(term == cf) %>%
          
          mutate(
               gender = gnr, 
               est_name = paste0(as.character(dfy[1]),"_",as.character(dfy[2])),
               type_row = tr,
               elapsed_years = ifelse(str_detect(tr,"2023"),dfy[2] - dfy[1],1),
               statistic = statistic,
               stat_lo = (conf.low / std.error),
               stat_hi = (conf.high / std.error)) %>%
          
      select(gender, est_name, elapsed_years,type_row, term, 
                 estimate,std.error,statistic,stat_lo, stat_hi)
     
     return(df_FM)
     
}     

# WP, 1987-2023 with daily values
#female

df_F_WP_1987_put_yr <- func_bar_Qall_WP(reg_F_WP_1987_2023,
     df_F_WP_1987_2023,"F","year_count","delta 1 year")

df_F_WP_1987_put_AQI <- func_bar_Qall_WP(reg_F_WP_1987_2023,
     df_F_WP_1987_2023,"F","AQI_daily_on_race_day","Daily AQI")

# df_F_WP_1987_put_AQI_14 <- func_bar_Qall_WP(reg_F_WP_1987_2023,
#     df_F_WP_1987_2023,"F","linear_school_AQI_14","School AQI")

df_F_WP_1987_put_grade <- func_bar_Qall_WP(reg_F_WP_1987_2023,
     df_F_WP_1987_2023,"F","grade12","delta grade 9-12")

df_F_WP_1987_put_div <- func_bar_Qall_WP(reg_F_WP_1987_2023,
     df_F_WP_1987_2023,"F","divisionD5","Small-large school")

#male
df_M_WP_1987_put_yr <- func_bar_Qall_WP(reg_M_WP_1987_2023,
     df_M_WP_1987_2023,"M","year_count","delta 1 year")

df_M_WP_1987_put_AQI <- func_bar_Qall_WP(reg_M_WP_1987_2023,
     df_M_WP_1987_2023,"M","AQI_daily_on_race_day","Daily AQI")

# df_M_WP_1987_put_AQI_14 <- func_bar_Qall_WP(reg_M_WP_1987_2023,
#   df_M_WP_1987_2023,"M","linear_school_AQI_14","School AQI")

df_M_WP_1987_put_grade <- func_bar_Qall_WP(reg_M_WP_1987_2023,
     df_M_WP_1987_2023,"M","grade12","delta grade 9-12")

df_M_WP_1987_put_div <- func_bar_Qall_WP(reg_M_WP_1987_2023,
     df_M_WP_1987_2023,"M","divisionD5","Small-large school")

# WP, 2000-2023 with hourly values for ozone and temperature
#  and with daily values for PM25

#female
df_F_WP_2000_put_yr <- func_bar_Qall_WP(reg_F_WP_2000_2023,
     df_F_WP_2000_2023,"F","year_count","delta 1 year")

df_F_WP_2000_put_ozone <- func_bar_Qall_WP(reg_F_WP_2000_2023,
     df_F_WP_2000_2023,"F","ozone_racetime", "Ozone")

df_F_WP_2000_put_temper <- func_bar_Qall_WP(reg_F_WP_2000_2023,
     df_F_WP_2000_2023,"F","temper_racetime","Temperature")

df_F_WP_2000_put_PM25 <- func_bar_Qall_WP(reg_F_WP_2000_2023,
      df_F_WP_2000_2023,"F","PM25_daily_on_race_day","PM 2.5")

#
# df_F_WP_2000_put_AQI_14 <- func_bar_Qall_WP(reg_F_WP_2000_2023,
#     df_F_WP_2000_2023,"F","linear_school_AQI_14","School AQI")

df_F_WP_2000_put_grade <- func_bar_Qall_WP(reg_F_WP_2000_2023,
     df_F_WP_2000_2023,"F","grade12", "delta grade 9-12")

df_F_WP_2000_put_div <- func_bar_Qall_WP(reg_F_WP_2000_2023,
     df_F_WP_2000_2023,"F","divisionD5","Small-large school")

#male
df_M_WP_2000_put_yr <- func_bar_Qall_WP(reg_M_WP_2000_2023,
  df_M_WP_2000_2023,"M","year_count","delta 1 year")

df_M_WP_2000_put_ozone <- func_bar_Qall_WP(reg_M_WP_2000_2023,
  df_M_WP_2000_2023,"M","ozone_racetime", "Ozone")

df_M_WP_2000_put_temper <- func_bar_Qall_WP(reg_M_WP_2000_2023,
  df_M_WP_2000_2023,"M","temper_racetime","Temperature")


df_M_WP_2000_put_PM25 <- func_bar_Qall_WP(reg_M_WP_2000_2023,
   df_M_WP_2000_2023,"M","PM25_daily_on_race_day","PM 2.5")

# df_M_WP_2000_put_AQI_14 <- func_bar_Qall_WP(reg_M_WP_2000_2023,
#   df_M_WP_2000_2023,"M","linear_school_AQI_14","School AQI")

df_M_WP_2000_put_grade <- func_bar_Qall_WP(reg_M_WP_2000_2023,
  df_M_WP_2000_2023,"M","grade12", "delta grade 9-12")

df_M_WP_2000_put_div <- func_bar_Qall_WP(reg_M_WP_2000_2023,
  df_M_WP_2000_2023,"M","divisionD5","Small-large school")

df_bar_WP_all_put <- rbind(
     df_F_WP_1987_put_yr,
     df_F_WP_1987_put_AQI,
#     df_F_WP_1987_put_AQI_14,
     df_F_WP_1987_put_grade,
     df_F_WP_1987_put_div,
     
     df_M_WP_1987_put_yr,
     df_M_WP_1987_put_AQI,
#     df_M_WP_1987_put_AQI_14,
     df_M_WP_1987_put_grade,
     df_M_WP_1987_put_div,
     
     df_F_WP_2000_put_yr,
     df_F_WP_2000_put_ozone,
     df_F_WP_2000_put_temper,
#     df_F_WP_2000_put_AQI_14,
     df_F_WP_2000_put_grade,
     df_F_WP_2000_put_div,
     
     df_M_WP_2000_put_yr,
     df_M_WP_2000_put_ozone,
     df_M_WP_2000_put_temper,
#     df_F_WP_2000_put_AQI_14,
     df_M_WP_2000_put_grade,
     df_M_WP_2000_put_div)

df_bar_WP_1987_put <- df_bar_WP_all_put %>%
     filter(est_name == "1987_2023")

df_bar_WP_1987_put <- df_bar_WP_1987_put %>%
     pivot_longer(statistic:stat_hi,
                  names_to = "type_stat",
                  values_to = "value")

df_bar_WP_1987_put <- df_bar_WP_1987_put %>% 
     mutate(est_name = factor(est_name),
            term = factor(term))

df_bar_WP_1987_put <- df_bar_WP_1987_put %>%
     mutate(type_row = fct_relevel(type_row,
     "delta 1 year",
     "Small-large school",
     "delta grade 9-12",
     "Daily AQI"))

df_bar_WP_2000_put <- df_bar_WP_all_put %>%
     pivot_longer(statistic:stat_hi,
                  names_to = "type_stat",
                  values_to = "value")

df_bar_WP_2000_put <- df_bar_WP_2000_put %>% 
     mutate(est_name = factor(est_name),
            term = factor(term),
            estimate = factor(estimate))

df_bar_WP_2000_put <- df_bar_WP_2000_put %>%
     filter(est_name == "2000_2023")

df_bar_WP_2000_put <- df_bar_WP_2000_put %>%
     mutate(type_row = fct_relevel(type_row,
          "delta 1 year",
          "Small-large school",
          "delta grade 9-12",
          "Ozone",
          "Temperature"))

# write a function to get the regression coefficients
# from D1D2_Q5 data subset at WP and at MTSAC

func_bar_Q5_WP <- function(reg_FM,df_data,gnr,cf,tr) {
     # filter on a coefficient
     
     dfn <- deparse(substitute(df_data))
     
     dfy <- as.numeric(str_extract_all(dfn, "\\d{4}")[[1]])
     
     df_FM <- tidy(reg_FM,conf.int = TRUE) %>%
          
          filter(term == cf) %>%
          
          mutate(
               gender = gnr, 
               est_name = paste0(as.character(dfy[1]),"_",as.character(dfy[2])),
               type_row = tr,
               elapsed_years = ifelse(str_detect(tr,"2023"),dfy[2] - dfy[1],1),
               stat_lo = conf.low / std.error,
               stat_hi = conf.high / std.error) %>%
          select(gender, est_name, 
                 elapsed_years, type_row, term, 
                 estimate, std.error,
                 statistic,stat_lo, stat_hi)
     
     return(df_FM)
     
}     

# WP 1987 - 2023

df_F_WP_1987_put_yr_D1D2_Q5 <- func_bar_Q5_WP(reg_F_WP_1987_2023_D1D2_Q5,
     df_F_WP_1987_2023_D1D2,"F","year_count","delta 1 year")

df_F_WP_1987_put_AQI_D1D2_Q5 <- func_bar_Q5_WP(reg_F_WP_1987_2023_D1D2_Q5,
     df_F_WP_1987_2023_D1D2,"F","AQI_daily_on_race_day","Daily AQI")

# df_F_WP_1987_put_AQI14_D1D2_Q5 <- func_bar_Q5_WP(reg_F_WP_1987_2023_D1D2_Q5,
#      df_F_WP_1987_2023_D1D2,"F","linear_school_AQI_14","School AQI")

df_F_WP_1987_put_grade_D1D2_Q5 <- func_bar_Q5_WP(reg_F_WP_1987_2023_D1D2_Q5,
     df_F_WP_1987_2023_D1D2,"F","grade12","delta grade 9-12")


df_M_WP_1987_put_yr_D1D2_Q5 <- func_bar_Q5_WP(reg_M_WP_1987_2023_D1D2_Q5,
     df_M_WP_1987_2023_D1D2,"M","year_count","delta 1 year")

df_M_WP_1987_put_AQI_D1D2_Q5 <- func_bar_Q5_WP(reg_M_WP_1987_2023_D1D2_Q5,
     df_M_WP_1987_2023_D1D2,"M","AQI_daily_on_race_day","Daily AQI")

# df_M_WP_1987_put_AQI14_D1D2_Q5 <- func_bar_Q5_WP(reg_M_WP_1987_2023_D1D2_Q5,
#      df_M_WP_1987_2023_D1D2,"M","linear_school_AQI_14","School AQI")

df_M_WP_1987_put_grade_D1D2_Q5 <- func_bar_Q5_WP(reg_M_WP_1987_2023_D1D2_Q5,
     df_M_WP_1987_2023_D1D2,"M","grade12","delta grade 9-12")

# WP 2000-2023
# females
df_F_WP_2000_put_yr_D1D2_Q5 <- func_bar_Q5_WP(reg_F_WP_2000_2023_D1D2_Q5,
     df_F_WP_2000_2023_D1D2,"F","year_count","delta 1 year")

df_F_WP_2000_put_ozone_D1D2_Q5 <- func_bar_Q5_WP(reg_F_WP_2000_2023_D1D2_Q5,
     df_F_WP_2000_2023_D1D2,"F","ozone_racetime", "Ozone")

df_F_WP_2000_put_temper_D1D2_Q5 <- func_bar_Q5_WP(reg_F_WP_2000_2023_D1D2_Q5,
     df_F_WP_2000_2023_D1D2,"F","temper_racetime","Temperature")

df_F_WP_2000_put_PM25_D1D2_Q5 <- func_bar_Q5_WP(reg_F_WP_2000_2023_D1D2_Q5,
   df_F_WP_2000_2023_D1D2,"F","PM25_daily_on_race_day","PM 2.5")

# df_F_WP_2000_put_school14_D1D2_Q5 <- func_bar_Q5_WP(reg_F_WP_2000_2023_D1D2_Q5,
#   df_F_WP_2000_2023_D1D2,"F","linear_school_AQI_14","School AQI")

df_F_WP_2000_put_grade_D1D2_Q5 <- func_bar_Q5_WP(reg_F_WP_2000_2023_D1D2_Q5,
     df_F_WP_2000_2023_D1D2,"F","grade12", "delta grade 9-12")

#males
df_M_WP_2000_put_yr_D1D2_Q5 <- func_bar_Q5_WP(reg_M_WP_2000_2023_D1D2_Q5,
     df_M_WP_2000_2023_D1D2,"M","year_count","delta 1 year")

df_M_WP_2000_put_ozone_D1D2_Q5 <- func_bar_Q5_WP(reg_M_WP_2000_2023_D1D2_Q5,
     df_M_WP_2000_2023_D1D2,"M","ozone_racetime","Ozone")

df_M_WP_2000_put_temper_D1D2_Q5 <- func_bar_Q5_WP(reg_M_WP_2000_2023_D1D2_Q5,
     df_M_WP_2000_2023_D1D2,"M","temper_racetime","Temperature")

df_M_WP_2000_put_PM25_D1D2_Q5 <- func_bar_Q5_WP(reg_M_WP_2000_2023_D1D2_Q5,
   df_M_WP_2000_2023_D1D2,"M","PM25_daily_on_race_day","PM 2.5")

# df_M_WP_2000_put_school14_D1D2_Q5 <- func_bar_Q5_WP(reg_M_WP_2000_2023_D1D2_Q5,
#   df_M_WP_2000_2023_D1D2,"M","linear_school_AQI_14","School AQI")

df_M_WP_2000_put_grade_D1D2_Q5 <- func_bar_Q5_WP(reg_M_WP_2000_2023_D1D2_Q5,
     df_M_WP_2000_2023_D1D2,"M","grade12","delta grade 9-12")

df_bar_WP_put_D1D2_Q5 <- rbind(
    df_F_WP_1987_put_yr_D1D2_Q5,
    df_F_WP_1987_put_AQI_D1D2_Q5,
#    df_F_WP_1987_put_AQI14_D1D2_Q5,
    df_F_WP_1987_put_grade_D1D2_Q5,
                           
    df_M_WP_1987_put_yr_D1D2_Q5,
    df_M_WP_1987_put_AQI_D1D2_Q5,
#    df_M_WP_1987_put_AQI14_D1D2_Q5,
    df_M_WP_1987_put_grade_D1D2_Q5,
                           
    df_F_WP_2000_put_yr_D1D2_Q5,
    df_F_WP_2000_put_ozone_D1D2_Q5,
    df_F_WP_2000_put_temper_D1D2_Q5,
    df_F_WP_2000_put_PM25_D1D2_Q5,
#    df_F_WP_2000_put_school14_D1D2_Q5,
    df_F_WP_2000_put_grade_D1D2_Q5,
                           
    df_M_WP_2000_put_yr_D1D2_Q5,
    df_M_WP_2000_put_ozone_D1D2_Q5,
    df_M_WP_2000_put_temper_D1D2_Q5,
    df_M_WP_2000_put_PM25_D1D2_Q5,
#    df_M_WP_2000_put_school14_D1D2_Q5,
    df_M_WP_2000_put_grade_D1D2_Q5)

df_bar_WP_put_D1D2_Q5 <- df_bar_WP_put_D1D2_Q5 %>%
     pivot_longer(statistic:stat_hi,
                  names_to = "type_stat",
                  values_to = "value")

df_bar_WP_put_D1D2_Q5 <- df_bar_WP_put_D1D2_Q5 %>% 
     mutate(est_name = factor(est_name),
            term = factor(term))

df_bar_WP_1987_put_D1D2_Q5 <- df_bar_WP_put_D1D2_Q5 %>%
     filter(est_name == "1987_2023")

df_bar_WP_1987_put_D1D2_Q5 <- df_bar_WP_1987_put_D1D2_Q5 %>%
     mutate(type_row = fct_relevel(type_row,
        "delta 1 year",
          "delta grade 9-12",
        "Daily AQI"))

df_bar_WP_2000_put_D1D2_Q5 <- df_bar_WP_put_D1D2_Q5 %>%
     filter(est_name == "2000_2023")

df_bar_WP_2000_put_D1D2_Q5 <- df_bar_WP_2000_put_D1D2_Q5 %>%
     mutate(type_row = fct_relevel(type_row,
    "delta 1 year",
    "delta grade 9-12",                                   
    "Ozone",
    "Temperature",
    "PM 2.5"))


df_F_WP_1987_2023 %>%
    group_by(division, grade) %>%
     summarise(
          mean_time = mean(finish_time_seconds, na.rm = TRUE),
          se_coef = sd(finish_time_seconds, na.rm = TRUE),
          n = n())

df_M_WP_1987_2023 %>%
     group_by(division, grade) %>%
     summarise(
          mean_time = mean(finish_time_seconds, na.rm = TRUE),
          se_coef = sd(finish_time_seconds, na.rm = TRUE),
          n = n())

#
# ========== check group or not group structure for WP ==========
# 

list_main_dfs_WP <- list(
  df_F_WP_1987_2023   = df_F_WP_1987_2023,
  df_F_WP_2000_2023   = df_F_WP_2000_2023,
  df_M_WP_1987_2023   = df_M_WP_1987_2023,
  df_M_WP_2000_2023   = df_M_WP_2000_2023)

#return from lapply should all be ungrouped
lapply(list_main_dfs_WP,class)

# ========== test for effects of missing values on PM25_daily_on_race_day ==========

dfs_WP_fit_PM25 <- list(F_WP_2000 = df_F_WP_2000_2023,
                        F_WP_2000_Q5 = dfqnt_F_WP_2000_2023_D1D2_Q5,
                      
                        M_WP_2000 = df_M_WP_2000_2023,
                        M_WP_2000_Q5 = dfqnt_M_WP_2000_2023_D1D2_Q5)

func_fit_WP_PM25 <- function(dfm, dfm_name) {
  
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
  
PM25_WP_fit_table <- tibble(
    
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

    cf_ozone = c(coef(fit_without_PM25)[["ozone_racetime"]], 
                 coef(fit_with_PM25)[["ozone_racetime"]]),
    
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


sry_fit_WP_PM25 <- lapply(names(dfs_WP_fit_PM25), function(nm) {
  func_fit_WP_PM25(dfs_WP_fit_PM25[[nm]], dfm_name = nm)})

# Bind the results 

PM25_fit_WP <- as_tibble(do.call(rbind, sry_fit_WP_PM25))

PM25_fit_WP <- PM25_fit_WP %>% 
  mutate(
    # explicit ordering of datasets; adjust if you prefer another order
    dataset = factor(
      dataset,
      levels = c(
        "F_WP_1987",
        "F_WP_1987_Q5",
        "F_WP_2000",
        "F_WP_2000_Q5",
        
        "M_WP_1987",
        "M_WP_1987_Q5",
        "M_WP_2000",
        "M_WP_2000_Q5")))

PM25_fit_WP <- PM25_fit_WP %>%
  mutate(
    gender = factor(case_when(
      grepl("F_", dataset) ~ "Female",
      grepl("M_", dataset) ~ "Male",
      TRUE ~ NA_character_)))


PM25_fit_WP <- PM25_fit_WP %>%
  select(dataset,gender,model,everything())

