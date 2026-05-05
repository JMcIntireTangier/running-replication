# ===== WOODWARD PARK ANALYSIS FOR FEMALES =====

df_F_WP_readin <- readRDS("RUN_F_WP_V2.RDS")

count_original_sample_F_WP <- dim(df_F_WP_readin)[1]

# verify variable types and filter missing values

df_F_WP_1987_2023 <- func_filters_run_WP(df_F_WP_readin)

count_valid_sample_F_WP <- dim(df_F_WP_1987_2023)[1]

(abs_sample_erosion_F_WP <- count_original_sample_F_WP - count_valid_sample_F_WP)

(rel_sample_erosion_F_WP <- abs_sample_erosion_F_WP / count_original_sample_F_WP)

# there were problems using 
# Check for NAs
# sum(is.na(df_F_WP_1987_2023$race_name))
# sum(is.na(df_F_WP_1987_2023$runner_id))

# Check for empty strings
# sum(df_F_WP_1987_2023$race_name == "")
# sum(df_F_WP_1987_2023$runner_id == "")

# Check unique counts
# n_distinct(df_F_WP_1987_2023$race_name)
# n_distinct(df_F_WP_1987_2023$runner_id)


# Convert to factors
# df_F_WP_1987_2023 <- df_F_WP_1987_2023 %>%
#   mutate(race_name = as.factor(race_name),
#          runner_id = as.factor(runner_id))

# Then try two-way clustering again
# reg_F_WP_1987_2023 <- lm_robust(
#   finish_time_seconds ~ year_count + AQI_daily_on_race_day + grade + division,
#   clusters = c(race_name, runner_id),
#   se_type = "CR2",
#   data = df_F_WP_1987_2023)

# tests for estimatr 1.0.6 and two way clustering

# test regression 1 ... fails
# reg_F_WP_1987_2023 <- lm_robust(
#   finish_time_seconds ~ year_count + AQI_daily_on_race_day + grade + division,
#   clusters = df_F_WP_1987_2023[, c("race_name", "runner_id")],
#   se_type = "CR2",
#   data = df_F_WP_1987_2023)
# 
# # test 2 works one-way only
# 
# reg_F_WP_1987_2023 <- lm_robust(finish_time_seconds ~ year_count +
#         AQI_daily_on_race_day +
#         grade +
#         division,
#       clusters = c(race_name),
#       se_type = "CR2",
#       data = df_F_WP_1987_2023)
# 
# # test 3 ... fails on two -way 
# reg_F_WP_1987_2023 <- lm_robust(finish_time_seconds ~ year_count +
#     AQI_daily_on_race_day +
#     grade +
#     division,
#     clusters = c(race_name,runner_id),
#     se_type = "CR2",
#     data = df_F_WP_1987_2023)
# 
# # test 4 fails on new syntax
# reg_F_WP_1987_2023 <- lm_robust(
#   finish_time_seconds ~ year_count + AQI_daily_on_race_day + grade + division,
#   clusters = ~ race_name + runner_id, 
#   se_type = "CR2",
#   data = df_F_WP_1987_2023)
# 
# # test 5 fails on another new syntax
# reg_F_WP_1987_2023 <- lm_robust(
#   finish_time_seconds ~ year_count + AQI_daily_on_race_day + grade + division,
#   clusters = c("race_name", "runner_id"),
#   se_type = "CR2",
#   data = df_F_WP_1987_2023)

# ===== REGRESSIONS FOR WOODWARD PARK, FEMALES, POOLED DIVISIONS, 1987-2023 =====

# now use fixest

reg_F_WP_1987_2023 <- feols(
  finish_time_seconds ~ year_count + AQI_daily_on_race_day + grade + division,
  cluster = ~ race_name + runner_id, 
  data = df_F_WP_1987_2023)

sry_F_WP_AQI_1987_2023 <- func_sry_reg_F_WP(df_F_WP_1987_2023)

func_F_WP_1987_2023_D <- function(df){
     feols(finish_time_seconds ~ 
      year_count + 
      AQI_daily_on_race_day +  
      grade, 
      cluster = ~ race_name + runner_id,
      data = df)}

# ===== REGRESSION DATA FOR WOODWARD PARK, FEMALES, DIVISIONS D1-D5, 1987 - 2023 =====

df_F_WP_1987_2023_D1 <- df_F_WP_1987_2023 %>% filter(division == "D1")
df_F_WP_1987_2023_D2 <- df_F_WP_1987_2023 %>% filter(division == "D2")
df_F_WP_1987_2023_D1D2 <- df_F_WP_1987_2023 %>% 
filter(division == "D1" | division == "D2")
df_F_WP_1987_2023_D3 <- df_F_WP_1987_2023 %>% filter(division == "D3")
df_F_WP_1987_2023_D4 <- df_F_WP_1987_2023 %>% filter(division == "D4")
df_F_WP_1987_2023_D5 <- df_F_WP_1987_2023 %>% filter(division == "D5")

reg_F_WP_1987_2023_D1 <- func_F_WP_1987_2023_D(df_F_WP_1987_2023_D1)
reg_F_WP_1987_2023_D2 <- func_F_WP_1987_2023_D(df_F_WP_1987_2023_D2)
reg_F_WP_1987_2023_D1D2 <- func_F_WP_1987_2023_D(df_F_WP_1987_2023_D1D2)
reg_F_WP_1987_2023_D3 <- func_F_WP_1987_2023_D(df_F_WP_1987_2023_D3)
reg_F_WP_1987_2023_D4 <- func_F_WP_1987_2023_D(df_F_WP_1987_2023_D4)
reg_F_WP_1987_2023_D5 <- func_F_WP_1987_2023_D(df_F_WP_1987_2023_D5)

lsreg_F_WP_1987_2023_divs <- mget(ls(pattern = "reg_F_WP_1987_2023_D\\d{1}$"))

# ===== top quintile regressions for females D1D2 1987-2023 =====

# Calculate the threshold for the fastest 20% of times

threshold_F_WP_1987_2023_D1D2_Q5  <- quantile(df_F_WP_1987_2023_D1D2$finish_time_seconds, 
     probs = 0.2, na.rm = TRUE)

# Slice the D1 dataframe to get rows in the top quintile
# NB: "dfqnt" denotes a data frame sliced on a given quantile

dfqnt_F_WP_1987_2023_D1D2_Q5 <- df_F_WP_1987_2023_D1D2[df_F_WP_1987_2023_D1D2$finish_time_seconds <= threshold_F_WP_1987_2023_D1D2_Q5, ]

func_F_WP_1987_2023 <- function(df){
  
    df <- droplevels(df)  # Drop unused factor levels
    
     feols(finish_time_seconds ~ 
             year_count + 
             AQI_daily_on_race_day + 
             grade + division,
             cluster = ~ race_name + runner_id,
             data = df)}

reg_F_WP_1987_2023_D1D2_Q5 <- func_F_WP_1987_2023(dfqnt_F_WP_1987_2023_D1D2_Q5)

# ===== REGRESSIONS FOR WOODWARD PARK, FEMALES, POOLED DIVISIONS, 2000-2023 =====
# All divisions
# NB: the RUN_F_WP_V2.RDS file has the correct start times after 1999

df_F_WP_2000_2023 <- filter(df_F_WP_1987_2023, year >= 2000)

df_F_WP_2000_2023$year_count <- df_F_WP_2000_2023$year - 1999

reg_F_WP_2000_2023 <- feols(finish_time_seconds ~ 
     year_count + 
     ozone_racetime +
     temper_racetime +
     PM25_daily_on_race_day +
     grade + 
     division, 
     cluster = ~ race_name + runner_id,
     data = df_F_WP_2000_2023)

func_F_WP_2000_2023_D <- function(df){
  
  df <- droplevels(df)
  
     feols(finish_time_seconds ~ 
             year_count + 
             ozone_racetime +
             temper_racetime +
             PM25_daily_on_race_day +
             grade,
             cluster = ~ race_name + runner_id,
             data = df)}

# regressions by each of the five divisions plus merged D1D2


df_F_WP_2000_2023_D1 <- df_F_WP_2000_2023 %>% filter(division == "D1")
df_F_WP_2000_2023_D2 <- df_F_WP_2000_2023 %>% filter(division == "D2")
df_F_WP_2000_2023_D1D2 <- df_F_WP_2000_2023 %>% 
     filter(division == "D1" | division == "D2")
df_F_WP_2000_2023_D3 <- df_F_WP_2000_2023 %>% filter(division == "D3")
df_F_WP_2000_2023_D4 <- df_F_WP_2000_2023 %>% filter(division == "D4")
df_F_WP_2000_2023_D5 <- df_F_WP_2000_2023 %>% filter(division == "D5")

reg_F_WP_2000_2023_D1 <- func_F_WP_2000_2023_D(df_F_WP_2000_2023_D1)
reg_F_WP_2000_2023_D2 <- func_F_WP_2000_2023_D(df_F_WP_2000_2023_D2)
reg_F_WP_2000_2023_D1D2 <- func_F_WP_2000_2023_D(df_F_WP_2000_2023_D1D2)
reg_F_WP_2000_2023_D3 <- func_F_WP_2000_2023_D(df_F_WP_2000_2023_D3)
reg_F_WP_2000_2023_D4 <- func_F_WP_2000_2023_D(df_F_WP_2000_2023_D4)
reg_F_WP_2000_2023_D5 <- func_F_WP_2000_2023_D(df_F_WP_2000_2023_D5)

# ===== top quintile regression for females D1D2 2003-2023 =====
# Calculate the 20th percentile threshold

threshold_F_WP_2000_2023_D1D2_Q5  <- quantile(df_F_WP_2000_2023_D1D2$finish_time_seconds, 
          probs = 0.2, na.rm = TRUE)

# Slice the D1D2 dataframe to get rows in the top quintile

dfqnt_F_WP_2000_2023_D1D2_Q5 <- 
     df_F_WP_2000_2023_D1D2[df_F_WP_2000_2023_D1D2$finish_time_seconds <= threshold_F_WP_2000_2023_D1D2_Q5, ]

lsdf_F_WP_qnt <- mget(ls(pattern = "dfqnt_F_WP.+_Q5$"))

dfqnt_F_WP_2000_2023_D1D2_Q5 <- dfqnt_F_WP_2000_2023_D1D2_Q5 %>% droplevels()

# Then run regression
reg_F_WP_1987_2023_D1D2_Q5 <- func_F_WP_1987_2023(dfqnt_F_WP_1987_2023_D1D2_Q5)

reg_F_WP_2000_2023_D1D2_Q5 <- feols(finish_time_seconds ~
     year_count +
     ozone_racetime +
          temper_racetime +
          PM25_daily_on_race_day +
          division +
          grade,
     cluster = ~ race_name + runner_id,
     data = dfqnt_F_WP_2000_2023_D1D2_Q5)

