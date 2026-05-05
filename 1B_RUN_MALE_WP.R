# ===== WOODWARD PARK ANALYSIS FOR MALES =====

df_M_WP_readin <- readRDS("RUN_M_WP_V2.RDS")

count_original_sample_M_WP <- dim(df_M_WP_readin)[1]

# verify variable types and filter missing values

df_M_WP_1987_2023 <- func_filters_run_WP(df_M_WP_readin)

count_valid_sample_M_WP <- dim(df_M_WP_1987_2023)[1]

(abs_sample_erosion_M_WP <- count_original_sample_M_WP - count_valid_sample_M_WP)

(rel_sample_erosion_M_WP <- abs_sample_erosion_M_WP / count_original_sample_M_WP)

# ===== REGRESSIONS FOR WOODWARD PARK, MALES, POOLED DIVISIONS, 1987-2023 =====

reg_M_WP_1987_2023 <- feols(finish_time_seconds ~ 
  year_count +
  AQI_daily_on_race_day +
  grade +
  division,
  cluster = ~ race_name + runner_id,
  data = df_M_WP_1987_2023)

sry_M_WP_AQI_1987_2023 <- func_sry_reg_M_WP(df_M_WP_1987_2023)

func_M_WP_1987_2023_D <- function(df){
  feols(finish_time_seconds ~ 
  year_count + 
  AQI_daily_on_race_day +
  grade, 
  clusters = ~ race_name + runner_id,
  data = df)}

text_stats_M_WP_AQI_1987_2023 <- func_stats_reg_WP(df_M_WP_1987_2023,reg_M_WP_1987_2023)

sry_M_WP_AQI_1987_2023 <- func_sry_reg_M_WP(df_M_WP_1987_2023,reg_M_WP_1987_2023)

func_M_WP_1987_2023_D <- function(df){
  feols(finish_time_seconds ~ 
              year_count + 
              AQI_daily_on_race_day +
              grade, 
            cluster = ~ race_name + runner_id,
            data = df)}

# ===== REGRESSION DATA FOR WOODWARD PARK, maleS, DIVISIONS D1-D5, 1987 - 2023 =====

df_M_WP_1987_2023_D1 <- df_M_WP_1987_2023 %>% filter(division == "D1")
df_M_WP_1987_2023_D2 <- df_M_WP_1987_2023 %>% filter(division == "D2")
df_M_WP_1987_2023_D1D2 <- df_M_WP_1987_2023 %>% 
  filter(division == "D1" | division == "D2")
df_M_WP_1987_2023_D3 <- df_M_WP_1987_2023 %>% filter(division == "D3")
df_M_WP_1987_2023_D4 <- df_M_WP_1987_2023 %>% filter(division == "D4")
df_M_WP_1987_2023_D5 <- df_M_WP_1987_2023 %>% filter(division == "D5")

reg_M_WP_1987_2023_D1 <- func_M_WP_1987_2023_D(df_M_WP_1987_2023_D1)
reg_M_WP_1987_2023_D2 <- func_M_WP_1987_2023_D(df_M_WP_1987_2023_D2)
reg_M_WP_1987_2023_D1D2 <- func_M_WP_1987_2023_D(df_M_WP_1987_2023_D1D2)
reg_M_WP_1987_2023_D3 <- func_M_WP_1987_2023_D(df_M_WP_1987_2023_D3)
reg_M_WP_1987_2023_D4 <- func_M_WP_1987_2023_D(df_M_WP_1987_2023_D4)
reg_M_WP_1987_2023_D5 <- func_M_WP_1987_2023_D(df_M_WP_1987_2023_D5)

lsreg_M_WP_1987_2023_divs <- mget(ls(pattern = "reg_M_WP_1987_2023_D\\d{1}$"))

# ===== top quintile regressions for males D1D2 1987-2023 =====

# Calculate the 80th percentile threshold

threshold_M_WP_1987_2023_D1D2_Q5  <- quantile(df_M_WP_1987_2023_D1D2$finish_time_seconds, 
                                              probs = 0.2, na.rm = TRUE)

# Slice the D1 dataframe to get rows in the top quintile
# NB: "dfqnt" denotes a data frame sliced on a given quantile

dfqnt_M_WP_1987_2023_D1D2_Q5 <- df_M_WP_1987_2023_D1D2[df_M_WP_1987_2023_D1D2$finish_time_seconds <= threshold_M_WP_1987_2023_D1D2_Q5, ]

func_M_WP_1987_2023 <- function(df){
  
  df <- droplevels(df) 
  feols(finish_time_seconds ~ 
              year_count + 
              AQI_daily_on_race_day + 
          #    #    linear_school_AQI_14 +
              grade +
              division,
            cluster = ~ race_name + runner_id,
            data = df)}

reg_M_WP_1987_2023_D1D2_Q5 <- func_M_WP_1987_2023(dfqnt_M_WP_1987_2023_D1D2_Q5)

# ===== REGRESSIONS FOR WOODWARD PARK, MALES, POOLED DIVISIONS, 2000-2023 =====
# All divisions
# NB: the RUN_M_WP_V2.RDS file has the correct start times after 1999


df_M_WP_2000_2023 <- filter(df_M_WP_1987_2023, year >= 2000)

df_M_WP_2000_2023$year_count <- df_M_WP_2000_2023$year - 1999

reg_M_WP_2000_2023 <- feols(finish_time_seconds ~ 
  year_count + 
  ozone_racetime +
  temper_racetime +
  PM25_daily_on_race_day +
  grade +
  division, 
  cluster = ~ race_name + runner_id,
  data = df_M_WP_2000_2023)

func_M_WP_2000_2023_D <- function(df){
  feols(finish_time_seconds ~ 
              year_count + 
              ozone_racetime +
              temper_racetime +
              PM25_daily_on_race_day +
          #    linear_school_AQI_14 +
              grade, 
            cluster = ~ race_name + runner_id,
            data = df)}

# regressions by each of the five divisions plus merged D1D2

df_M_WP_2000_2023_D1 <- df_M_WP_2000_2023 %>% filter(division == "D1")
df_M_WP_2000_2023_D2 <- df_M_WP_2000_2023 %>% filter(division == "D2")
df_M_WP_2000_2023_D1D2 <- df_M_WP_2000_2023 %>% 
  filter(division == "D1" | division == "D2")
df_M_WP_2000_2023_D3 <- df_M_WP_2000_2023 %>% filter(division == "D3")
df_M_WP_2000_2023_D4 <- df_M_WP_2000_2023 %>% filter(division == "D4")
df_M_WP_2000_2023_D5 <- df_M_WP_2000_2023 %>% filter(division == "D5")

reg_M_WP_2000_2023_D1 <- func_M_WP_2000_2023_D(df_M_WP_2000_2023_D1)
reg_M_WP_2000_2023_D2 <- func_M_WP_2000_2023_D(df_M_WP_2000_2023_D2)
reg_M_WP_2000_2023_D1D2 <- func_M_WP_2000_2023_D(df_M_WP_2000_2023_D1D2)
reg_M_WP_2000_2023_D3 <- func_M_WP_2000_2023_D(df_M_WP_2000_2023_D3)
reg_M_WP_2000_2023_D4 <- func_M_WP_2000_2023_D(df_M_WP_2000_2023_D4)
reg_M_WP_2000_2023_D5 <- func_M_WP_2000_2023_D(df_M_WP_2000_2023_D5)


# ===== top quintile regression for males D1D2 2003-2023 =====
# Calculate the 20th percentile threshold


threshold_M_WP_2000_2023_D1D2_Q5  <- quantile(df_M_WP_2000_2023_D1D2$finish_time_seconds, 
                                              probs = 0.2, na.rm = TRUE)

# Slice the D1D2 dataframe to get rows in the top quintile

dfqnt_M_WP_2000_2023_D1D2_Q5 <- 
  df_M_WP_2000_2023_D1D2[df_M_WP_2000_2023_D1D2$finish_time_seconds <= threshold_M_WP_2000_2023_D1D2_Q5, ]

lsdf_M_WP_qnt <- mget(ls(pattern = "dfqnt_M_WP.+_Q5$"))

dfqnt_M_WP_2000_2023_D1D2_Q5 <- dfqnt_M_WP_2000_2023_D1D2_Q5 %>% droplevels()
  
reg_M_WP_2000_2023_D1D2_Q5 <- feols(finish_time_seconds ~
  year_count +
  ozone_racetime +
  temper_racetime +
  PM25_daily_on_race_day +
  division +
  grade,
  cluster = ~ race_name + runner_id,
  data = dfqnt_M_WP_2000_2023_D1D2_Q5)

