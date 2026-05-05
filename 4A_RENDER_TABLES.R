# 4A RENDER_TABLES

# Get tidy output with confidence intervals
# WP female 
# 

#first create a flextable function

func_format_flextable <- function(ft) {
  ft %>%
    line_spacing(space = 0.9, part = "all") %>%
    padding(padding.top = 3, padding.bottom = 0, part = "all") %>%
    set_table_properties(layout = "autofit", width = 1)
}

df_F_WP_1987_put_yr <- tidy(reg_F_WP_1987_2023, conf.int = TRUE) %>%
     # Get only the year coefficient row
     filter(term == "year_count") %>%
     # Get median of dependent variable
     mutate(
          gender = "F", est_name = "1987_2023",
          type_row = "delta 1987 - 2023",
          elapsed_years = 2023 - 1987,
          median_time = median(df_F_WP_1987_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (elapsed_years * estimate) / median_time,
          est_lo = (elapsed_years * conf.low) / median_time,
          est_hi = (elapsed_years * conf.high) / median_time) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_F_WP_1987_put_AQI <- tidy(reg_F_WP_1987_2023, conf.int = TRUE) %>%
     # Get only the AQI daily coefficient row
     filter(term == "AQI_daily_on_race_day") %>%
     # Get median of dependent variable
     mutate(
          gender = "F", est_name = "1987_2023",
          type_row = "Daily AQI",
          median_time = median(df_F_WP_1987_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_F_WP_1987_put_AQI_14 <- tidy(reg_F_WP_1987_2023, conf.int = TRUE) %>%
  # Get the school AQI coefficient row
  filter(term == "linear_school_AQI_14") %>%
  # Get median of dependent variable
  mutate(
    gender = "F", est_name = "1987_2023",
    type_row = "School AQI",
    median_time = median(df_F_WP_1987_2023$finish_time_seconds),
    # Normalize estimates and CIs by median
    est = (estimate) / median_time,
    est_lo = (conf.low) / median_time,
    est_hi = (conf.high) / median_time
  ) %>%
  select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_F_WP_1987_put_gr <- tidy(reg_F_WP_1987_2023, conf.int = TRUE) %>%
     # Get only the grade 12 coefficient row
     filter(term == "grade12") %>%
     # Get median of dependent variable
     mutate(
          gender = "F", est_name = "1987_2023",
          type_row = "grade 12 - grade 9",
          median_time = median(df_F_WP_1987_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_F_WP_1987_put_div <- tidy(reg_F_WP_1987_2023, conf.int = TRUE) %>%
     # Get only the division 5 coefficient row
     filter(term == "divisionD5") %>%
     # Get median of dependent variable
     mutate(
          gender = "F", est_name = "1987_2023",
          type_row = "Large-small school",
          median_time = median(df_F_WP_1987_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

# Get tidy output with confidence intervals
# now do MALE

df_M_WP_1987_put_yr <- tidy(reg_M_WP_1987_2023, conf.int = TRUE) %>%
     # Get only the year coefficient row
     filter(term == "year_count") %>%
     # Get median of dependent variable
     mutate(
          gender = "M", est_name = "1987_2023",
          type_row = "delta 1987 - 2023",
          elapsed_years = 2023 - 1987,
          median_time = median(df_M_WP_1987_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (elapsed_years * estimate) / median_time,
          est_lo = (elapsed_years * conf.low) / median_time,
          est_hi = (elapsed_years * conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_M_WP_1987_put_AQI <- tidy(reg_M_WP_1987_2023, conf.int = TRUE) %>%
     # Get only the AQI daily coefficient row
     filter(term == "AQI_daily_on_race_day") %>%
     # Get median of dependent variable
     mutate(
          gender = "M", est_name = "1987_2023",
          type_row = "Daily AQI",
          median_time = median(df_M_WP_1987_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_M_WP_1987_put_AQI_14 <- tidy(reg_M_WP_1987_2023, conf.int = TRUE) %>%
  # Get the school AQI coefficient row
  filter(term == "linear_school_AQI_14") %>%
  # Get median of dependent variable
  mutate(
    gender = "M", est_name = "1987_2023",
    type_row = "School AQI",
    median_time = median(df_M_WP_1987_2023$finish_time_seconds),
    # Normalize estimates and CIs by median
    est = (estimate) / median_time,
    est_lo = (conf.low) / median_time,
    est_hi = (conf.high) / median_time
  ) %>%
  select(gender, est_name, type_row, term, est, est_lo, est_hi)

# Get tidy output with confidence intervals
df_M_WP_1987_put_gr <- tidy(reg_M_WP_1987_2023, conf.int = TRUE) %>%
     # Get only the grade 12 coefficient row
     filter(term == "grade12") %>%
     # Get median of dependent variable
     mutate(
          gender = "M", est_name = "1987_2023",
          type_row = "grade 12 - grade 9",
          median_time = median(df_M_WP_1987_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_M_WP_1987_put_div <- tidy(reg_M_WP_1987_2023, conf.int = TRUE) %>%
     # Get only the division 5 coefficient row
     filter(term == "divisionD5") %>%
     # Get median of dependent variable
     mutate(
          gender = "M", est_name = "1987_2023",
          type_row = "Large-small school",
          median_time = median(df_M_WP_1987_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

# now do 2000 - 2023
# now do F

df_F_WP_2000_put_yr <- tidy(reg_F_WP_2000_2023, conf.int = TRUE) %>%
     # Get only the year coefficient row
     filter(term == "year_count") %>%
     # Get median of dependent variable
     mutate(
          gender = "F", est_name = "2000_2023",
          type_row = "delta 2000 - 2023",
          elapsed_years = 2023 - 2000,
          median_time = median(df_F_WP_2000_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (elapsed_years * estimate) / median_time,
          est_lo = (elapsed_years * conf.low) / median_time,
          est_hi = (elapsed_years * conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_F_WP_2000_put_ozone <- tidy(reg_F_WP_2000_2023, conf.int = TRUE) %>%
     # Get only the ozone coefficient row
     filter(term == "ozone_racetime") %>%
     # Get median of dependent variable
     mutate(
          gender = "F", est_name = "2000_2023",
          type_row = "Ozone effect",
          median_time = median(df_F_WP_2000_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_F_WP_2000_put_temper <- tidy(reg_F_WP_2000_2023, conf.int = TRUE) %>%
     # Get only the temperature coefficient row
     filter(term == "temper_racetime") %>%
     # Get median of dependent variable
     mutate(
          gender = "F", est_name = "2000_2023",
          type_row = "Temperature effect",
          median_time = median(df_F_WP_2000_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_F_WP_2000_put_PM25 <- tidy(reg_F_WP_2000_2023, conf.int = TRUE) %>%
  # Get only the PM2.5 coefficient row
  filter(term == "PM25_daily_on_race_day") %>%
  # Get median of dependent variable
  mutate(
    gender = "F", est_name = "2000_2023",
    type_row = "PM25 effect",
    median_time = median(df_F_WP_2000_2023$finish_time_seconds),
    # Normalize estimates and CIs by median
    est = (estimate) / median_time,
    est_lo = (conf.low) / median_time,
    est_hi = (conf.high) / median_time
  ) %>%
  select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_F_WP_2000_put_AQI_14 <- tidy(reg_F_WP_2000_2023, conf.int = TRUE) %>%
  # Get only the school AQI 14 row
  filter(term == "linear_school_AQI_14") %>%
  # Get median of dependent variable
  mutate(
    gender = "F", est_name = "2000_2023",
    type_row = "School AQI",
    median_time = median(df_F_WP_2000_2023$finish_time_seconds),
    # Normalize estimates and CIs by median
    est = (estimate) / median_time,
    est_lo = (conf.low) / median_time,
    est_hi = (conf.high) / median_time
  ) %>%
  select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_F_WP_2000_put_gr <- tidy(reg_F_WP_2000_2023, conf.int = TRUE) %>%
     # Get only the grade 12 coefficient row
     filter(term == "grade12") %>%
     # Get median of dependent variable
     mutate(
          gender = "F", est_name = "2000_2023",
          type_row = "grade 12 - grade 9",
          median_time = median(df_F_WP_2000_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_F_WP_2000_put_div <- tidy(reg_F_WP_2000_2023, conf.int = TRUE) %>%
     # Get only the divison 5 coefficient row
     filter(term == "divisionD5") %>%
     # Get median of dependent variable
     mutate(
          gender = "F", est_name = "2000_2023",
          type_row = "Large-small school",
          median_time = median(df_F_WP_2000_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

#males
df_M_WP_2000_put_yr <- tidy(reg_M_WP_2000_2023, conf.int = TRUE) %>%
     # Get only the year coefficient row
     filter(term == "year_count") %>%
     # Get median of dependent variable
     mutate(
          gender = "M", est_name = "2000_2023",
          type_row = "delta 2000 - 2023",
          elapsed_years = 2023 - 2000,
          median_time = median(df_M_WP_2000_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (elapsed_years * estimate) / median_time,
          est_lo = (elapsed_years * conf.low) / median_time,
          est_hi = (elapsed_years * conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_M_WP_2000_put_ozone <- tidy(reg_M_WP_2000_2023, conf.int = TRUE) %>%
     # Get only the ozone coefficient row
     filter(term == "ozone_racetime") %>%
     # Get median of dependent variable
     mutate(
          gender = "M", est_name = "2000_2023",
          type_row = "Ozone effect",
          median_time = median(df_M_WP_2000_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_M_WP_2000_put_temper <- tidy(reg_M_WP_2000_2023, conf.int = TRUE) %>%
     # Get only the temperature coefficient row
     filter(term == "temper_racetime") %>%
     # Get median of dependent variable
     mutate(
          gender = "M", est_name = "2000_2023",
          type_row = "Temperature effect",
          median_time = median(df_M_WP_2000_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_M_WP_2000_put_PM25 <- tidy(reg_M_WP_2000_2023, conf.int = TRUE) %>%
  # Get only the PM2.5 coefficient row
  filter(term == "PM25_daily_on_race_day") %>%
  # Get median of dependent variable
  mutate(
    gender = "M", est_name = "2000_2023",
    type_row = "PM25 effect",
    median_time = median(df_M_WP_2000_2023$finish_time_seconds),
    # Normalize estimates and CIs by median
    est = (estimate) / median_time,
    est_lo = (conf.low) / median_time,
    est_hi = (conf.high) / median_time
  ) %>%
  select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_M_WP_2000_put_AQI_14 <- tidy(reg_M_WP_2000_2023, conf.int = TRUE) %>%
  # Get only the school AQI 14 row
  filter(term == "linear_school_AQI_14") %>%
  # Get median of dependent variable
  mutate(
    gender = "M", est_name = "2000_2023",
    type_row = "School AQI",
    median_time = median(df_M_WP_2000_2023$finish_time_seconds),
    # Normalize estimates and CIs by median
    est = (estimate) / median_time,
    est_lo = (conf.low) / median_time,
    est_hi = (conf.high) / median_time
  ) %>%
  select(gender, est_name, type_row, term, est, est_lo, est_hi)

# Get tidy output with confidence intervals
df_M_WP_2000_put_gr <- tidy(reg_M_WP_2000_2023, conf.int = TRUE) %>%
     # Get only the grade 12 coefficient row
     filter(term == "grade12") %>%
     # Get median of dependent variable
     mutate(
          gender = "M", est_name = "2000_2023",
          type_row = "grade 12 - grade 9",
          median_time = median(df_M_WP_2000_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_M_WP_2000_put_div <- tidy(reg_M_WP_2000_2023, conf.int = TRUE) %>%
     # Get only the divison 5 coefficient row
     filter(term == "divisionD5") %>%
     # Get median of dependent variable
     mutate(
          gender = "M", est_name = "2000_2023",
          type_row = "Large-small school",
          median_time = median(df_M_WP_2000_2023$finish_time_seconds),
          # Normalize estimates and CIs by median
          est = (estimate) / median_time,
          est_lo = (conf.low) / median_time,
          est_hi = (conf.high) / median_time
     ) %>%
     select(gender, est_name, type_row, term, est, est_lo, est_hi)

df_bar_WP <- rbind(
  df_F_WP_1987_put_yr,
  df_F_WP_1987_put_AQI,
  df_F_WP_1987_put_AQI_14,
  df_F_WP_1987_put_gr,
  df_F_WP_1987_put_div,
  
  df_M_WP_1987_put_yr,
  df_M_WP_1987_put_AQI,
  df_M_WP_1987_put_AQI_14,
  df_M_WP_1987_put_gr,
  df_M_WP_1987_put_div,
  
  df_F_WP_2000_put_yr,
  df_F_WP_2000_put_ozone,
  df_F_WP_2000_put_temper,
  df_F_WP_2000_put_PM25,
  df_F_WP_2000_put_AQI_14,
  df_F_WP_2000_put_gr,
  df_F_WP_2000_put_div,
  
  df_M_WP_2000_put_yr,
  df_M_WP_2000_put_ozone,
  df_M_WP_2000_put_temper,
  df_M_WP_2000_put_PM25,
  df_M_WP_2000_put_AQI_14,
  df_M_WP_2000_put_gr,
  df_M_WP_2000_put_div)
  
df_bar_WP <- df_bar_WP %>%
     pivot_longer(est:est_hi,
                  names_to = "estimate",
                  values_to = "value")

df_bar_WP <- df_bar_WP %>% mutate(est_name = factor(est_name),
     term = factor(term),
     estimate = factor(estimate))

df_bar_WP_1987_2023 <- df_bar_WP %>%
     filter(est_name == "1987_2023")

df_bar_WP_1987_2023 <- df_bar_WP_1987_2023 %>%
     mutate(type_row = fct_relevel(type_row,
      "delta 1987 - 2023",
        "Daily AQI",
#      "School AQI",
          "grade 12 - grade 9",
          "Large-small school"))

df_bar_WP_2000_2023 <- df_bar_WP %>%
     filter(est_name == "2000_2023")

df_bar_WP_2000_2023 <- df_bar_WP_2000_2023 %>%
     mutate(type_row = fct_relevel(type_row,
  "delta 2000 - 2023",
    "Ozone effect",
    "Temperature effect",
#  "PM25 effect",
#  "School AQI",
     "grade 12 - grade 9",
     "Large-small school"))

# df_bar_WP_2000_2023 <- df_bar_WP_2000_2023 %>%
#      mutate(effect_scale = case_when(type_row %in%
#           c("Ozone effect","Temperature effect") ~ "Smaller effect",
#           TRUE ~ "Larger effect"))


# ===== MAKE TABLES AND LIST THEM

list_table_1 <- list(
  `Female pooled divisions` = reg_F_WP_1987_2023,
  `Male pooled divisions`   = reg_M_WP_1987_2023)

summary_table_1 <- modelsummary(
  list_table_1,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("year_count" = "Year",
                  "AQI_daily_on_race_day" = "Daily AQI",
                  "linear_school_AQI_14" = "School AQI",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12",
                  "divisionD2" = "Division 2",
                  "divisionD3" = "Division 3",
                  "divisionD4" = "Division 4",
                  "divisionD5" = "Division 5"),
  title = "Table 1: Female times converged to male times at about 1.2 second yr^-1^, 
  Woodward Park, 1987 - 2023",
  
  notes = list(
    "Standard errors clustered by race identifier + runner identifier.",
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

list_table_2a <- list(
  `Division 1` = reg_F_WP_1987_2023_D1,
  `Division 2` = reg_F_WP_1987_2023_D2,
  `Division 3` = reg_F_WP_1987_2023_D3,
  `Division 4` = reg_F_WP_1987_2023_D4,
  `Division 5` = reg_F_WP_1987_2023_D5)

summary_table_2a <- modelsummary(
  list_table_2a,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("year_count" = "Year",
                  "AQI_daily_on_race_day" = "Daily AQI",
#                  "linear_school_AQI_14" = "School AQI",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12"),
  title = "Table 2a: Female times became faster, Woodward Park, 1987 - 2023",

notes = list(
  "Standard errors clustered by race identifier + runner identifier.",
  "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))


list_table_2b <- list(
  `Division 1` = reg_M_WP_1987_2023_D1,
  `Division 2` = reg_M_WP_1987_2023_D2,
  `Division 3` = reg_M_WP_1987_2023_D3,
  `Division 4` = reg_M_WP_1987_2023_D4,
  `Division 5` = reg_M_WP_1987_2023_D5)

summary_table_2b <- modelsummary(
  list_table_2b,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("year_count" = "Year",
                  "AQI_daily_on_race_day" = "Daily AQI",
#                  "linear_school_AQI_14" = "School AQI",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12"),
  title = "Table 2b: Male times became faster, Woodward Park, 1987 - 2023",

notes = list(
  "Standard errors clustered by race identifier + runner identifier.",
  "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

list_table_3 <- list(
  `Female pooled divisions` = reg_F_WP_2000_2023,
  `Male pooled divisions` = reg_M_WP_2000_2023)

summary_table_3 <- modelsummary(
  list_table_3,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("year_count" = "Year",
                  "ozone_racetime" = "Hourly ozone",
                  "temper_racetime" = "Hourly temperature",
                  "PM25_daily_on_race_day" = "Daily PM 2.5",
#                  "linear_school_AQI_14" = "School AQI",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12",
                  "divisionD2" = "Division 2",
                  "divisionD3" = "Division 3",
                  "divisionD4" = "Division 4",
                  "divisionD5" = "Division 5"),
  title = "Table 3: Female times converged to male times by 
about 0.07 second yr^-1^, Woodward Park 2000 - 2023",

notes = list(
  "Standard errors clustered by race identifier + runner identifier.",
  "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

list_table_4a <- list(
  `Division 1` = reg_F_WP_2000_2023_D1,
  `Division 2` = reg_F_WP_2000_2023_D2,
  `Division 3` = reg_F_WP_2000_2023_D3,
  `Division 4` = reg_F_WP_2000_2023_D4,
  `Division 5` = reg_F_WP_2000_2023_D5)

min_P_PM25_F_WP_2000 <- c(
  `SE_D1` = fixest::pvalue(reg_F_WP_2000_2023_D1)["PM25_daily_on_race_day"],
  `SE_D2` = fixest::pvalue(reg_F_WP_2000_2023_D2)["PM25_daily_on_race_day"],
  `SE_D3` = fixest::pvalue(reg_F_WP_2000_2023_D3)["PM25_daily_on_race_day"],
  `SE_D4` = fixest::pvalue(reg_F_WP_2000_2023_D4)["PM25_daily_on_race_day"],
  `SE_D5` = fixest::pvalue(reg_F_WP_2000_2023_D5)["PM25_daily_on_race_day"]) %>% 
  min()

summary_table_4a <- modelsummary(
  list_table_4a,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("year_count" = "Year",
                  "ozone_racetime" = "Hourly ozone",
                  "temper_racetime" = "Hourly temperature",
                  "PM25_daily_on_race_day" = "Daily PM 2.5",
 #                 "linear_school_AQI_14" = "School AQI",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12",
                  "divisionD2" = "Division 2",
                  "divisionD3" = "Division 3",
                  "divisionD4" = "Division 4",
                  "divisionD5" = "Division 5"),
  title = "Table 4a: Female times quickened with no environmental effects,
Woodward Park, 2000 - 2023",
 
 notes = list(
   "Standard errors clustered by race identifier + runner identifier.",
   "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

list_table_4b <- list(
  `Division 1` = reg_M_WP_2000_2023_D1,
  `Division 2` = reg_M_WP_2000_2023_D2,
  `Division 3` = reg_M_WP_2000_2023_D3,
  `Division 4` = reg_M_WP_2000_2023_D4,
  `Division 5` = reg_M_WP_2000_2023_D5)

min_P_PM25_M_WP_2000 <- c(
  `SE_D1` = fixest::pvalue(reg_M_WP_2000_2023_D1)["PM25_daily_on_race_day"],
  `SE_D2` = fixest::pvalue(reg_M_WP_2000_2023_D2)["PM25_daily_on_race_day"],
  `SE_D3` = fixest::pvalue(reg_M_WP_2000_2023_D3)["PM25_daily_on_race_day"],
  `SE_D4` = fixest::pvalue(reg_M_WP_2000_2023_D4)["PM25_daily_on_race_day"],
  `SE_D5` = fixest::pvalue(reg_M_WP_2000_2023_D5)["PM25_daily_on_race_day"]) %>% 
  min()


summary_table_4b <- modelsummary(
  list_table_4b,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("year_count" = "Year",
                  "ozone_racetime" = "Hourly ozone",
                  "temper_racetime" = "Hourly temperature",
                  "PM25_daily_on_race_day" = "Daily PM 2.5",
#                  "linear_school_AQI_14" = "School AQI",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12"),
 
  title = "Table 4b: Male times quickened with some slowing effect of temperature,
Woodward Park, 2000 - 2023",

notes = list(
  "Standard errors clustered by race identifier + runner identifier.",
  "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

list_table_4c <- list(
  `Female, 1987-2023` = reg_F_WP_1987_2023_D1D2_Q5,
  `Male, 1987-2023` =   reg_M_WP_1987_2023_D1D2_Q5,
  `Female, 2000-2023` = reg_F_WP_2000_2023_D1D2_Q5,
  `Male, 2000-2023` = reg_M_WP_2000_2023_D1D2_Q5)


summary_table_4c <- modelsummary(
  list_table_4c,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("year_count" = "Year",
                  "AQI_daily_on_race_day" = "Daily AQI",
                  "ozone_racetime" = "Hourly ozone",
                  "temper_racetime" = "Hourly temperature",
                  "PM25_daily_on_race_day" = "Daily PM 2.5",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12",
                  "divisionD2" = "Division 2"),
  title = "Table 4c:  Top quintile times quickened more among females than males,
D1 & D2, Woodward Park, 1987 - 2023",
  
  notes = list(
    "Standard errors clustered by race identifier + runner identifier.",
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

list_table_5a <- list(
  `Pooled divisions` = reg_F_MTSAC,
  `Divisions 1 & 2` = reg_F_MTSAC_D1D2,
  `Division 3` = reg_F_MTSAC_D3D3,
  `Divisions 4 & 5` = reg_F_MTSAC_D4D5)

summary_table_5a <- modelsummary(
  list_table_5a,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("year_count" = "Year",
                  "ozone_racetime" = "Hourly ozone",
                  "temper_racetime" = "Hourly temperature",
                  "PM25_daily_on_race_day" = "Daily PM 2.5",
#                  "linear_school_AQI_14" = "School AQI",
                  "program_score" = "Program",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12",
                  "divisionD3" = "Division 3",
                  "divisionD4_D5" = "Divisions 4 & 5"),
  title = "Table 5a: Female times generally became faster, Mt SAC, 2002 - 2023",

notes = list(
  "Standard errors clustered by race identifier + runner identifier.",
  "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

list_table_5b <- list(
  `Pooled divisions` = reg_M_MTSAC,
  `Divisions 1 & 2` = reg_M_MTSAC_D1D2,
  `Division 3` = reg_M_MTSAC_D3D3,
  `Division 4 & 5` = reg_M_MTSAC_D4D5)

summary_table_5b <- modelsummary(
  list_table_5b,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("year_count" = "Year",
                  "ozone_racetime" = "Hourly ozone",
                  "temper_racetime" = "Hourly temperature",
                  "PM25_daily_on_race_day" = "Daily PM 2.5",
#                  "linear_school_AQI_14" = "School AQI",
                  "program_score" = "Program",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12",
                  "divisionD3" = "Division 3",
                  "divisionD4_D5" = "Divisions 4 & 5"),
  title = "Table 5b: Male times quickened generally with some slowing effects of 
higher ozone levels, Mt SAC, 2002 - 2023",

notes = list(
  "Standard errors clustered by race identifier + runner identifier.",
  "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

list_table_6 <- list(
  `Female` = reg_F_MTSAC_D1D2_Q5,
  `Male`   = reg_M_MTSAC_D1D2_Q5)

summary_table_6 <- modelsummary(
  list_table_6,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("year_count" = "Year",
                  "ozone_racetime" = "Hourly ozone",
                  "temper_racetime" = "Hourly temperature",
                  "PM25_daily_on_race_day" = "Daily PM 2.5",
 #                 "linear_school_AQI_14" = "School AQI",
                  "program_score" = "Program",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12"),
  title = "Table 6: Top quintile times among females and males were largely unaffected
by year, ozone and temperature levels, D1 & D2, Mt SAC, 2002 - 2023",
 
 notes = list(
   "Standard errors clustered by race identifier + runner identifier.",
   "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))


# ========== TABLE 7  ========== 
# render table 7 
# POOLED INTERACTIONS MODEL MT SAC FEMALES AND MALES, 2002 - 2023
# NB: PM 2.5 excluded because too many missing values for hourly and daily measurements

df_F_MTSAC_pool <- df_F_MTSAC %>% select(race_name,year,year_count,course,gender,
  section,school,program_score,runner_name,runner_id,
  ozone_racetime,temper_racetime,PM25_daily_on_race_day,
  AQI_daily_on_race_day,grade,division,finish_time_seconds)

df_M_MTSAC_pool <- df_M_MTSAC %>% select(race_name,year,year_count,course,gender,
  section,school,program_score,runner_name,runner_id,
  ozone_racetime,temper_racetime,PM25_daily_on_race_day,
  AQI_daily_on_race_day,grade,division,finish_time_seconds)

df_F_WP_2002_2023_pool <- df_F_WP_2000_2023 %>% select(race_name,year,year_count,
  course,gender,runner_name, runner_id,
  section,school,program_score,
  ozone_racetime,temper_racetime,PM25_daily_on_race_day,
  AQI_daily_on_race_day,grade,division,finish_time_seconds) %>%
  filter(year >= 2002) %>%
  mutate(year_count = year - 2001)

df_F_WP_2002_2023_pool <- df_F_WP_2002_2023_pool %>% 
  mutate(division = as.character(division))

df_F_WP_2002_2023_pool$division <-  
  str_replace_all(df_F_WP_2002_2023_pool$division,
  c("^D1" = "D1_D2",
  "^D2" = "D1_D2",
  "^D3"    = "D3",
  "^D4" = "D4_D5",
  "^D5" = "D4_D5"))

df_F_WP_2002_2023_pool <- df_F_WP_2002_2023_pool %>% mutate(division = 
    factor(division, levels = c("D1_D2","D3","D4_D5")))

# now the males
df_M_WP_2002_2023_pool <- df_M_WP_2000_2023 %>% select(race_name,year,year_count,course,gender,
  section,school,program_score,runner_name,runner_id,
  ozone_racetime,temper_racetime,PM25_daily_on_race_day,
  AQI_daily_on_race_day,grade,division,finish_time_seconds) %>% 
  filter(year >= 2002)  %>%
  mutate(year_count = year - 2001)



df_M_WP_2002_2023_pool <- df_M_WP_2002_2023_pool %>% 
  mutate(division = as.character(division))

df_M_WP_2002_2023_pool$division <-  
  str_replace_all(df_M_WP_2002_2023_pool$division,
  c("^D1" = "D1_D2",
  "^D2" = "D1_D2",
  "^D3"    = "D3",
  "^D4" = "D4_D5",
  "^D5" = "D4_D5"))

df_M_WP_2002_2023_pool <- df_M_WP_2002_2023_pool %>% mutate(division = 
  factor(division, levels = c("D1_D2","D3","D4_D5")))

# bind the files

df_pool_MTSAC_WP_2002_2023 <- rbind(df_F_MTSAC_pool,
    df_F_WP_2002_2023_pool,df_M_MTSAC_pool,df_F_WP_2002_2023_pool) %>%
  arrange(year,course,gender)

# do the pooled regressions

reg_pool_2002_2023_ix  <- feols(finish_time_seconds ~ 
  course +
  gender + 
  year_count + 
  year_count:gender +  
  ozone_racetime +
  ozone_racetime:gender +  
  temper_racetime +
  temper_racetime:gender +
  grade + 
  gender:grade +
  division +
  division:gender,
  cluster = ~ race_name + runner_id,
  data = df_pool_MTSAC_WP_2002_2023)

(sry_reg_pool_2002_2023_ix <- summary(reg_pool_2002_2023_ix))

reg_pool_2002_2023_noix  <- feols(finish_time_seconds ~ 
  course +
  gender + 
  year_count +
  ozone_racetime +
  temper_racetime +
  grade + 
  division,
  cluster = ~ race_name + runner_id,
  data = df_pool_MTSAC_WP_2002_2023)

(sry_reg_pool_2002_2023_noix <- summary(reg_pool_2002_2023_noix))

list_table_7 <- list(
  `pooled MTSAC WP with interactions` = reg_pool_2002_2023_ix,
  `pooled MTSAC WP without interactions ` = reg_pool_2002_2023_noix)

# LIST Table 7

summary_table_7 <- modelsummary(
  list_table_7,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("courseWoodward Park" = "Woodward Park",
                  "genderMALE" = "Male",
                  "year_count" = "Year",
                  "year_count:gender" = "Male x year",
                  "ozone_racetime" = "Hourly ozone",
                  "temper_racetime" = "Hourly temper",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12",
                  "divisionD3" = "Division D3",
                  "divisionD4_D5" = "Division D4 D5",
                  "genderMALE:year_count" = "Male x year",
                  "genderMALE:ozone_racetime" = "Male x ozone",
                  "genderMALE:temper_racetime" = "Male x temperature",
                  "genderMALE:grade10" = "Male x Grade 10",
                  "genderMALE:grade11" = "Male x Grade 11",
                  "genderMALE:grade12" = "Male x Grade 12",
                  "genderMALE:divisionD3" = "Male x D3",
                  "genderMALE:divisionD4_D5" = "Male x D4-D5"),
  
  title = "Table 7: Interaction effects of gender were unimportant with year and
environmental variables in pooled data, Mt SAC and Woodward Park, 2002-2023",

    notes = list(
    "Standard errors clustered by race identifier + runner identifier.",
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

# list Table 8
 
LIST_REG_DND <- readRDS("LIST_REG_DND.RDS")


list_table_8 <- list(
  `Females, DnD` = LIST_REG_DND$reg_F_MTSAC_WP_DnD,
  `Females, DnD and runner FE` = LIST_REG_DND$reg_F_MTSAC_WP_DnD_FE,
  `Males, DnD` = LIST_REG_DND$reg_M_MTSAC_WP_DnD,
  `Males, DnD and runner FE` = LIST_REG_DND$reg_M_MTSAC_WP_DnD_FE)

summary_table_8 <- modelsummary(
  list_table_8,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("finish_time_seconds_MTSAC" = "Finish time at Mt SAC",
                  "year_count_MTSAC" = "Year",
                  "d_ozone_racetime" = "Delta ozone",
                  "d_temper_racetime" = "Delta temperature",
                  "grade_MTSAC10" = "Grade 10",
                  "grade_MTSAC11" = "Grade 11",
                  "grade_MTSAC12" = "Grade 12"),
  
title = "Table 8: Fixed effects by individual runners dominated all 
other effects, Mt SAC and Woodward Park, 2002 - 2023",
  
  notes = list(
    "Standard errors clustered by year in all models; columns 2 and 4 include runner fixed effects.",
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))


# make the list of tables

lst_summary_tab <- ls(pattern = "summary_table_\\d{1}")

names(lst_summary_tab) <- c(
    "Table 1", "Table 2a", "Table 2b", "Table 3",
    "Table 4a", "Table 4b", "Table 4c", "Table 5a",
    "Table 5b", "Table 6", "Table 7", "Table 8")
