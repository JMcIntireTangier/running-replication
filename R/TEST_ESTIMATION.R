# ========== Test 1 ==========
# compare goodness of fit and heteroskedasticity in linear and log-linear models
# 2 models for each of the 6 data sets giving 12 rows in the return

# NB in these models the column 'program_score' is not used; this is to impose comparability
# between the Mt SAC dfs and the WP dfs ('program_score' not used in any WP models)

dfs_basic <- list(
  df_F_MTSAC = df_F_MTSAC,
  df_M_MTSAC = df_M_MTSAC,
  
  df_F_WP_1987_2023 = df_F_WP_1987_2023,
  df_M_WP_1987_2023 = df_M_WP_1987_2023,
  
  df_F_WP_2000_2023 = df_F_WP_2000_2023,
  df_M_WP_2000_2023 = df_M_WP_2000_2023)

# ========== Test 1: BASIC LINEAR OR LOG MODELS ==========  ==========

message(" --- Test 1: Comparing log-linear specifications for goodness of fit and for heteroskedasticity --- ")

func_log_linear <- function(df, df_name, cluster_var = c(race_name,runner_id)) {
# Fit clustered-robust linear models
# 

df <- ungroup(df)

  testmdl_lin_robust <- feols(
  finish_time_seconds ~ year_count +
    AQI_daily_on_race_day +
    linear_school_AQI_14 +
    grade + division,
  cluster  = ~ race_name + runner_id,
  data     = df)

# Fit clustered-robust log models
  
  testmdl_log_robust <- feols(
    ln_finish_time_seconds ~ year_count +
      ln_AQI_daily_on_race_day +
      ln_school_AQI_14 +
      grade + division,
    cluster  = ~ race_name + runner_id,
    data     = df)
  
# 2. Predictions on seconds scale and fit stats
  dflog_linear <- df %>%
    mutate(
      pred_log    = predict(testmdl_log_robust, newdata = df),
      pred_sec_log = exp(pred_log),
      pred_sec_lin = predict(testmdl_lin_robust, newdata = df))
  
  fit_tbl <- with(dflog_linear, {
    y <- finish_time_seconds
    
    rmse_log  <- sqrt(mean((y - pred_sec_log)^2,  na.rm = TRUE))
    rmse_lin <- sqrt(mean((y - pred_sec_lin)^2, na.rm = TRUE))
    
    mae_log   <- mean(abs(y - pred_sec_log),  na.rm = TRUE)
    mae_lin  <- mean(abs(y - pred_sec_lin), na.rm = TRUE)
    
    r2_log    <- cor(y, pred_sec_log,  use = "complete.obs")^2
    r2_lin   <- cor(y, pred_sec_lin, use = "complete.obs")^2
    
    data.frame(
      dataset   = df_name,
      model     = c("linear model","log model"),
      RMSE_sec  = c(rmse_lin, rmse_log),
      MAE_sec   = c(mae_lin, mae_log),
      R2_sec    = c(r2_lin,   r2_log)
    )})
  
  
# lm() versions for heteroskedasticity tests (no clustering)

  testmdl_lin <- lm(
    finish_time_seconds ~ year_count +
      AQI_daily_on_race_day +
      linear_school_AQI_14 +
      grade + division,
    data = df)
  
  testmdl_log <- lm(
    ln_finish_time_seconds ~ year_count +
      ln_AQI_daily_on_race_day +
      ln_school_AQI_14 +
      grade + division,
    data = df)
  
  # Breusch–Pagan with original regressors
  bp_lin       <- bptest(testmdl_lin)
  bp_log       <- bptest(testmdl_log)
  
  # White-type tests using fitted and fitted^2
  aux_lin <- data.frame(
    res2 = residuals(testmdl_lin)^2,
    fit  = fitted(testmdl_lin))
  
  aux_log <- data.frame(
    res2 = residuals(testmdl_log)^2,
    fit  = fitted(testmdl_log))
  
  bp_lin_white <- bptest(res2 ~ fit + I(fit^2), data = aux_lin)
  bp_log_white <- bptest(res2 ~ fit + I(fit^2), data = aux_log)
  
  het_tbl <- data.frame(
    dataset = df_name,
    model   = c("linear model", "log model"),
    BP_stat_origX  = c(bp_lin$statistic,      bp_log$statistic),
    BP_p_origX     = c(bp_lin$p.value,        bp_log$p.value),
    BP_stat_white  = c(bp_lin_white$statistic, bp_log_white$statistic),
    BP_p_white     = c(bp_lin_white$p.value,   bp_log_white$p.value)
  )
  
  list(
    fit  = fit_tbl,
    het  = het_tbl)
}


sry_log_linear <- lapply(names(dfs_basic), function(nm) {
  func_log_linear(dfs_basic[[nm]], df_name = nm, 
cluster_var = c(race_name,runner_id))})

# Bind all fit tables and all heteroskedasticity tables
fit_all <- do.call(rbind, lapply(sry_log_linear, `[[`, "fit"))
het_all <- do.call(rbind, lapply(sry_log_linear, `[[`, "het"))

fit_all <- fit_all %>%
  mutate(
    gender = factor(case_when(
      grepl("^df_F_", dataset) ~ "Female",
      grepl("^df_M_", dataset) ~ "Male",
      TRUE ~ NA_character_)))

fit_all <- fit_all %>% 
  mutate(
    # ordering of datasets should always be this 
    dataset = factor(
      dataset,
      levels = c(
        "df_F_MTSAC",
        "df_M_MTSAC",
        
        "df_F_WP_1987_2023",
        "df_M_WP_1987_2023",
        
        "df_F_WP_2000_2023",
        "df_M_WP_2000_2023")))

# Ensure model is a factor with consistent ordering

fit_all <- fit_all %>%
  mutate(model = factor(model, levels = c("linear model", "log model")))

fit_all

Test_1a_plot_log_linear_fit <- ggplot(fit_all, aes(x = dataset, y = RMSE_sec, group = model, color = model)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_line(position = position_dodge(width = 0.3)) +
  #  facet_wrap(vars(model), ncol = 2, scales = "fixed") +
  labs(
    title = "Test 1a: Goodness of fit in linear and log models in basic datasets",
    x = "Dataset",
    y = "RMSE (seconds)"
  ) +
  
  scale_x_discrete(labels = 
      c("df_F_MTSAC" = "F MTSAC",
      "df_M_MTSAC" = "M MTSAC",
      "df_F_WP_1987_2023" = "F WP\n1987-2023",
      "df_M_WP_1987_2023" = "M WP\n1987-2023",
      "df_F_WP_2000_2023" = "F WP\n2000-2023",
      "df_M_WP_2000_2023" = "M WP\n2000-2023")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(hjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.9,0.9),
        legend.justification = c("right","top"),
        legend.title = element_blank())

het_all <- het_all %>%
  mutate(
    gender = factor(case_when(
      grepl("^df_F_", dataset) ~ "Female",
      grepl("^df_M_", dataset) ~ "Male",
      TRUE ~ NA_character_)))

het_all <- het_all %>% 
  mutate(
    # explicit ordering of datasets; adjust if you prefer another order
    dataset = factor(
      dataset,
      levels = c(
        "df_F_MTSAC",
        "df_M_MTSAC",
        
        "df_F_WP_1987_2023",
        "df_M_WP_1987_2023",
        
        "df_F_WP_2000_2023",
        "df_M_WP_2000_2023")))


# Ensure model is a factor with consistent ordering
het_all <- het_all %>%
  mutate(model = factor(model, levels = c("linear model", "log model")))

het_all

# Plot BP (original regressors) by dataset and model

Test_1b_plot_log_linear_BP_test <- ggplot(het_all, aes(x = dataset, y = BP_stat_origX, group = model, color = model)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_line(position = position_dodge(width = 0.3)) +
#  facet_wrap(vars(model), ncol = 2, scales = "fixed") +
  labs(
    title = "Test 1b: Heteroskedasticity in linear and log models in basic datasets",
    x = "Dataset",
    y = "Breusch–Pagan statistic"
  ) +
  scale_x_discrete(labels = 
                     c("df_F_MTSAC" = "F MSTAC",
                       "df_M_MTSAC" = "M MTSAC",
                       "df_F_WP_1987_2023" = "F WP\n1987-2023",
                       "df_M_WP_1987_2023" = "M WP\n1987-2023",
                       "df_F_WP_2000_2023" = "F WP\n2000-2023",
                       "df_M_WP_2000_2023" = "M WP\n2000-2023")) +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.9,0.9),
        legend.justification = c("right","top"),
        legend.title = element_blank())

Test_1a_plot_log_linear_fit
Test_1b_plot_log_linear_BP_test


message(" --- End Test 1a and Test 1b comparing log-linear specifications --- 
lesson = log and linear models very similar on GoF
lesson = log and linear models similar on heteroskedasticity except in M MTSAC")

# Test 1c
#
# PLOT  HIGHLY HETEROSKEDASTIC VALUES IN df_M_MTSAC
# 


df_M_MTSAC_long <- df_M_MTSAC %>%
  filter(finish_time_seconds <= 1800) %>%
  mutate(finish_time_seconds_ln = log(finish_time_seconds)) %>%
  pivot_longer(cols = c(finish_time_seconds, finish_time_seconds_ln),
                      names_to = "scale",
                      values_to = "value") %>%
  mutate(scale = str_replace_all(scale,
                                 c("finish_time_seconds_ln" = "Log time",
                                   "finish_time_seconds"    = "Raw time")))

skews <- df_M_MTSAC_long %>%
  group_by(scale) %>%
  summarise(skew = skewness(value, na.rm = TRUE))

df_M_MTSAC_long <- df_M_MTSAC_long %>%
  left_join(skews, by = "scale") %>%
  mutate(scale_lab = paste0(scale, "\nSkewness = ", round(skew, 2)))

ttl_raw_log <- c("Raw time","Log time")

Test_1c_plot_log_linear_dists <- ggplot(df_M_MTSAC_long, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, color = "white", fill = "grey70") +
  geom_density(color = "red") +
  facet_wrap(~ scale_lab, scales = "free") +
  theme_bw() +
  labs(x = "Finish time", y = "Density",
       title = "Test 1c: Log vs raw finish times for male runners at Mt SAC")

Test_1c_plot_log_linear_dists


# ========== Test 2: DIRECT AND INDIRECT TEMPERATURE AND O3 MODELS ==========


func_temper_effect <- function(df,  df_name) {
  
  ozone_fit <- feols(
    ozone_racetime ~ year_count + 
      temper_racetime + 
      AQI_daily_on_race_day + 
      grade + 
      division,
    cluster  = ~ race_name + runner_id,
    data = df)
  
  temper_dir_O3 <- coef(ozone_fit)[["temper_racetime"]]
  
  testmdl_lin_robust <- feols(
    finish_time_seconds ~ year_count +
      ozone_racetime +
      temper_racetime +
      AQI_daily_on_race_day +
      grade + 
      division,
    cluster = ~ race_name + runner_id,
    data     = df)
  
  ozone_dir_time       <- coef(testmdl_lin_robust)[["ozone_racetime"]]
  temper_dir_time <- coef(testmdl_lin_robust)[["temper_racetime"]]
  
  temper_indir_via_O3 <- temper_dir_O3 * ozone_dir_time
  total_temp_effect    <- temper_dir_time + temper_indir_via_O3
  
  cor_O3_AQI <- cor(df$ozone_racetime,df$AQI_daily_on_race_day,use = "complete.obs")
  
  temper_tbl <- data.frame(
    dataset   = df_name,
    temper_dir_O3,
    ozone_dir_time,
    temper_dir_time,
    temper_indir_via_O3,
    total_temp_effect,
    cor_O3_AQI)}

sry_temper_effect <- lapply(names(dfs_basic), function(nm) {
  func_temper_effect(dfs_basic[[nm]], df_name = nm)})


# Bind all fit tables and all heteroskedasticity tables

temper_all <- as_tibble(do.call(rbind, sry_temper_effect))

temper_all <- temper_all %>% pivot_longer(
  temper_dir_O3:cor_O3_AQI,
  names_to = "vars",
  values_to = "value")

temper_all <- temper_all %>%
  mutate(
    gender = factor(case_when(
      grepl("^df_F_", dataset) ~ "Female",
      grepl("^df_M_", dataset) ~ "Male",
      TRUE ~ NA_character_)))

temper_all$dataset <- str_replace_all(temper_all$dataset,c("df_" = ""))

temper_all <- temper_all %>%   
  mutate(
    # explicit ordering of datasets; adjust if you prefer another order
    dataset = factor(
      dataset,
      levels = c(
        "F_MTSAC",
        "M_MTSAC",
        
        "F_WP_1987_2023",
        "M_WP_1987_2023",
        
        "F_WP_2000_2023",
        "M_WP_2000_2023")))

vars_to_plot <- c(
  "temper_dir_O3",
  "ozone_dir_time",
  "temper_dir_time",
  "temper_indir_via_O3",
  "total_temp_effect")

Test_2_plot_temper_effect <- temper_all %>%
  filter(vars %in% vars_to_plot) %>%
  ggplot(aes(x = vars, y = value, fill = vars)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ dataset, ncol = 2) +
  labs(
    title = "Test 2: Direct and indirect effects of temperature",
    subtitle = "on finish times at Mt SAC and Woodward Park",
    x = NULL,
    y = "Effect size on finish time in seconds",
    fill = "Effect") +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(hjust = 1),
        legend.position = "none")

Test_2_plot_temper_effect


# ========== Test PLOT 3: QUALITY OF PM 2.5 DATA ========== 


func_count_PM25 <- function(df) {
valid_daily_PM25 <- sum(complete.cases(df[c("PM25_daily_on_race_day","AQI_daily_on_race_day")]))
missing_daily_PM25 <- sum(!complete.cases(df[c("PM25_daily_on_race_day","AQI_daily_on_race_day")]))
valid_hourly_PM25 <- sum(complete.cases(df[c("mean_PM25_hourly_FRM","ozone_racetime")]))
missing_hourly_PM25 <- sum(!complete.cases(df[c("mean_PM25_hourly_FRM","ozone_racetime")]))

tibble(valid_daily_PM25 = valid_daily_PM25,
       missing_daily_PM25 = missing_daily_PM25,
       valid_hourly_PM25 = valid_hourly_PM25,
       missing_hourly_PM25)}
  
df_PM25 <- dfs_basic %>%
  lapply(func_count_PM25) %>% 
  bind_rows(.id = "source_id")

df_PM25_long <- df_PM25 %>%
  pivot_longer(cols = -source_id, names_to = "metric", values_to = "count")

df_PM25_long <- df_PM25_long %>% mutate(source_id = factor(source_id))
         
df_PM25_long <- df_PM25_long %>% mutate(source_id = 
  fct_relevel("df_F_MTSAC","df_M_MTSAC",
"df_F_WP_1987_2023","df_M_WP_1987_2023",
"df_F_WP_2000_2023","df_M_WP_2000_2023"))

df_PM25_long <- df_PM25_long %>%
  mutate(metric = as.factor(as.character(metric)))

df_PM25_long <- df_PM25_long %>%
  mutate(metric = fct_relevel(metric,"valid_daily_PM25",
      "missing_daily_PM25",
      "valid_hourly_PM25",
      "missing_hourly_PM25"))

  
Test_3_plot_PM25 <- ggplot(df_PM25_long,
       aes(x = source_id, y = count, fill = metric)) +
  labs(title = "Test 3: PM 2.5 valid and missing value counts by dataset") +

  geom_col(position = "dodge") + 
  
  labs(x = "Dataset", y = "Count", fill = "Metric") +
  
  scale_y_continuous(labels = comma_format(big.mark = ",", decimal_mark = ".")) +

  scale_x_discrete(labels = 
                     c("df_F_MTSAC" = "F MTSAC",
                       "df_M_MTSAC" = "M MTSAC",
                       "df_F_WP_1987_2023" = "F WP  \n1987-2023",
                       "df_M_WP_1987_2023" = "M WP  \n1987-2023",
                       "df_F_WP_2000_2023" = "F WP  \n2000-2023",
                       "df_M_WP_2000_2023" = "M WP  \n2000-2023")) +
  
  scale_fill_discrete(
    labels = c("valid daily PM25",
               "missing daily PM25",
               "valid hourly PM25",
               "missing hourly PM25")
  ) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(hjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.33,0.90),
        legend.justification = c("center","top"),
        legend.title = element_blank())

Test_3_plot_PM25


# ========== Test 4: COMPARE LINEAR LAGGED AQI TO SPLINE AQI ========== ==========

# Test 4a: count valid and missing values for AQI 14 
# labelled as test_plot_AQI_linear_or_spline

dfs_basic_AQI <- list(df_F_WP_1987_2023   = df_F_WP_1987_2023,
                      df_M_WP_1987_2023   = df_M_WP_1987_2023,
                      
                      df_F_WP_2000_2023   = df_F_WP_2000_2023,
                      df_M_WP_2000_2023   = df_M_WP_2000_2023)

func_count_AQI <- function(df) {
  
  valid_linear_AQI_14 <- sum(complete.cases(df["linear_school_AQI_14"]))
  missing_linear_AQI_14 <- sum(!complete.cases(df["linear_school_AQI_14"]))
  
  valid_spline_AQI_14 <- sum(complete.cases(df["ln_AQI_lag_WP2"]))
  
  missing_spline_AQI_14 <- sum(!complete.cases(df["ln_AQI_lag_WP2"]))
  
  tibble(valid_linear_AQI_14 = valid_linear_AQI_14,
         missing_linear_AQI_14 = missing_linear_AQI_14,
         valid_spline_AQI_14 = valid_spline_AQI_14,
         missing_spline_AQI_14)}

df_AQI <- dfs_basic_AQI %>%
  lapply(func_count_AQI) %>% 
  bind_rows(.id = "source_id")

df_AQI_long <- df_AQI %>%
  pivot_longer(cols = -source_id, names_to = "metric", values_to = "count")

df_AQI_long <- df_AQI_long %>%
  mutate(metric = as.factor(as.character(metric)))

df_AQI_long <- df_AQI_long %>%
  mutate(metric = fct_relevel(metric,"valid_linear_AQI_14",
                              "missing_linear_AQI_14",
                              "valid_spline_AQI_14",
                              "missing_spline_AQI_14"))

Test_4a_plot_AQI_linear_or_spline <- ggplot(df_AQI_long,
                                      aes(x = source_id, y = count, fill = metric)) +
  labs(title = "Test 4a: AQI linear and spline valid and missing values at Woodward Park") +
  
  geom_col(position = "dodge") + 
  
  labs(x = "Dataset", y = "Count", fill = "Metric") +
  
  scale_x_discrete(labels = 
                     c("df_F_WP_1987_2023" = "F WP\n1987-2023",
                       "df_M_WP_1987_2023" = "M WP\n1987-2023",
                       "df_F_WP_2000_2023" = "F WP\n2000-2023",
                       "df_M_WP_2000_2023" = "M WP\n2000-2023")) +
  
  scale_y_continuous(labels = comma_format(big.mark = ",", decimal_mark = ".")) +
  
  scale_fill_discrete(
    labels = c("valid linear AQI 14",
               "missing linear AQI 14",
               "valid spline AQI 14",
               "missing spline AQI 14")
  ) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(hjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.33,0.85),
        legend.justification = c("center","top"),
        legend.title = element_blank())

Test_4a_plot_AQI_linear_or_spline

# Test plot lagged-linear to spline
# using grade as factor with and without interactions
# 2 models for each of 6 datasets, giving 12 rows in the return df

message(" --- Running test 4b to compare linear lagged AQI to spline lagged AQI with AIC --- ")

func_linear_spline_AQI_14 <- function(df, df_name) {
  
  # Make sure grade is available as both factor and numeric 9–12
  
  df <- df %>%
    mutate(
      grade_f = factor(grade),               # factor version (if not already)
      grade_n = as.numeric(as.character(grade)))  # 9,10,11,12 as numeric
  
  
  # 1) grade as factor, no interactions, linear AQI 14
  linear_AQI_14 <- lm(
    finish_time_seconds ~ year_count +
      AQI_daily_on_race_day +
      linear_school_AQI_14 +
      grade_f,
    data = df)
  
  # 2) grade as factor, no interactions, spline AQI 14
  spline_AQI_14 <- lm(
    finish_time_seconds ~
      AQI_daily_on_race_day +
      cf_AQI_lag1 + cf_AQI_lag2 + cf_AQI_lag3 + cf_AQI_lag4 +
      grade_f,
    data = df)
  
  data.frame(
    dataset   = df_name,
    model     = c("linear_AQI_14",
                  "spline_AQI_14"),
    AIC_value = c(AIC(linear_AQI_14),
                  AIC(spline_AQI_14)))}

AQI_14_grade_tbl <- do.call(
  rbind,
  lapply(names(dfs_basic), function(nm) {
    func_linear_spline_AQI_14(dfs_basic[[nm]], df_name = nm)
  }))

AQI_14_grade_tbl <- AQI_14_grade_tbl %>%
  mutate(
    gender = factor(case_when(
      grepl("^df_F_", dataset) ~ "Female",
      grepl("^df_M_", dataset) ~ "Male",
      TRUE ~ NA_character_)))

AQI_14_grade_tbl <- AQI_14_grade_tbl %>%
  mutate(
    dataset = factor(dataset,
                     levels = c(
                       "df_F_MTSAC",
                       "df_M_MTSAC",
                       
                       "df_F_WP_1987_2023",
                       "df_M_WP_1987_2023",
                       
                       "df_F_WP_2000_2023",
                       "df_M_WP_2000_2023")))

# drop rows without gender if any
AQI_14_grade_tbl <- AQI_14_grade_tbl %>%
  filter(!is.na(gender))

AQI_14_grade_tbl <- arrange(AQI_14_grade_tbl,gender, dataset)

AQI_14_grade_tbl

Test_4b_plot_AQI_14_linear_or_spline <- ggplot(AQI_14_grade_tbl,
  aes(x = dataset, y = AIC_value,
  color = model, group = model)) +
  geom_point(size = 2) +
  geom_line() +
  #   facet_wrap(vars(gender), ncol = 2, scales = "fixed") +
  labs(
    title = "Test 4b: Comparing linear-lagged AQI to spline-lagged AQI using Akaike information criterion",
    x = "Dataset",
    y = "AIC statistic"
  ) +
  
  scale_x_discrete(labels = 
                     c("df_F_MTSAC" = "F MSTAC",
                       "df_M_MTSAC" = "M MTSAC",
                       
                       "df_F_WP_1987_2023" = "F WP\n1987-2023",
                       "df_M_WP_1987_2023" = "M WP\n1987-2023",
                       
                       "df_F_WP_2000_2023" = "F WP\n2000-2023",
                       "df_M_WP_2000_2023" = "M WP\n2000-2023")) +
  
  scale_y_continuous(labels = comma_format(big.mark = ",", decimal_mark = ".")) +
  
  scale_color_discrete(
    labels = c("linear AQI 14","spline AQI 14")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(hjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.67,0.85),
        legend.justification = c("center","top"),
        legend.title = element_blank())

Test_4b_plot_AQI_14_linear_or_spline

# 
# # ========== Test PLOT 4c: COMPARE GOODNESS OF FIT AND HETEROSKEDASTICITY
# in linear and spline AQI 14 models for the Q5 datasets
# 2 models for each of the 6 data sets giving 12 rows in the return

message(" --- Running Test 4c to compare linear lagged AQI to spline AQI for GoF and heteroskedasticity with RE framework --- ")

func_spline_linear_Q5 <- function(df, df_name) {
  # Fit clustered-robust linear models
  
  testmdl_lin_robust <- feols(
    finish_time_seconds ~ year_count +
      AQI_daily_on_race_day +
  #    linear_school_AQI_14 +
      grade,
    cluster  = ~ race_name + runner_id,
    data     = df)
  
  summary(testmdl_lin_robust)
  
  # Fit clustered-robust ln models
  
  testmdl_spline_robust <- feols(
    finish_time_seconds ~ year_count +
      AQI_daily_on_race_day +
      cf_AQI_lag1 +
      cf_AQI_lag2 +
      cf_AQI_lag3 +
      cf_AQI_lag4 +
      grade,
    cluster  = ~ race_name + runner_id,
    
    data     = df)
  
  summary(testmdl_spline_robust)
  
  # 2. Predictions on seconds scale and fit stats
  df_spline_linear_Q5 <- df %>%
    mutate(
      pred_spline    = predict(testmdl_spline_robust, newdata = df),
      pred_lin = predict(testmdl_lin_robust, newdata = df)
    )
  
  fit_tbl_spline <- with(df_spline_linear_Q5, {
    y <- finish_time_seconds
    
    rmse_spline  <- sqrt(mean((y - pred_spline)^2,  na.rm = TRUE))
    rmse_lin <- sqrt(mean((y - pred_lin)^2, na.rm = TRUE))
    
    mae_spline   <- mean(abs(y - pred_spline),  na.rm = TRUE)
    mae_lin  <- mean(abs(y - pred_lin), na.rm = TRUE)
    
    r2_spline    <- cor(y, pred_spline,  use = "complete.obs")^2
    r2_lin   <- cor(y, pred_lin, use = "complete.obs")^2
    
    data.frame(
      dataset   = df_name,
      model     = c("linear AQI", "spline AQI"),
      RMSE_sec  = c(rmse_spline, rmse_lin),
      MAE_sec   = c(mae_spline,  mae_lin),
      R2_sec    = c(r2_spline,   r2_lin)
    )
  })
  
  # 3. lm() versions for heteroskedasticity tests (no clustering)
  testmdl_lin <- lm(
    finish_time_seconds ~ year_count +
      AQI_daily_on_race_day +
      # linear_school_AQI_14 +
      grade,
    data = df)
  
  m_spline <- lm(
    finish_time_seconds ~ year_count +
      AQI_daily_on_race_day +
      cf_AQI_lag1 +
      cf_AQI_lag2 +
      cf_AQI_lag3 +
      cf_AQI_lag4 +
      grade,
    data = df)
  
  # Breusch–Pagan with original regressors
  bp_lin       <- bptest(testmdl_lin)
  bp_spline      <- bptest(m_spline)
  
  # White-type tests using fitted and fitted^2
  aux_lin <- data.frame(
    res2 = residuals(testmdl_lin)^2,
    fit  = fitted(testmdl_lin))
  
  aux_spline <- data.frame(
    res2 = residuals(m_spline)^2,
    fit  = fitted(m_spline))
  
  bp_lin_white <- bptest(res2 ~ fit + I(fit^2), data = aux_lin)
  bp_spline_white <- bptest(res2 ~ fit + I(fit^2), data = aux_spline)
  
  het_tbl_spline <- data.frame(
    dataset = df_name,
    model   = c("linear AQI", "spline AQI"),
    BP_stat_origX  = c(bp_lin$statistic,      bp_spline$statistic),
    BP_p_origX     = c(bp_lin$p.value,        bp_spline$p.value),
    BP_stat_white  = c(bp_lin_white$statistic, bp_spline_white$statistic),
    BP_p_white     = c(bp_lin_white$p.value,   bp_spline_white$p.value))
  
  list(
    fit  = fit_tbl_spline,
    het  = het_tbl_spline)
}

dfs_Q5 <- list(
  dfqnt_F_MTSAC_D1D2_Q5 = dfqnt_F_MTSAC_D1D2_Q5,
  dfqnt_M_MTSAC_D1D2_Q5 = dfqnt_M_MTSAC_D1D2_Q5,
  
  dfqnt_F_WP_1987_2023_D1D2_Q5   = dfqnt_F_WP_1987_2023_D1D2_Q5,
  dfqnt_M_WP_1987_2023_D1D2_Q5   = dfqnt_M_WP_1987_2023_D1D2_Q5,
  
  dfqnt_F_WP_2000_2023_D1D2_Q5   = dfqnt_F_WP_2000_2023_D1D2_Q5,
  dfqnt_M_WP_2000_2023_D1D2_Q5   = dfqnt_M_WP_2000_2023_D1D2_Q5)

sry_spline_linear_Q5 <- lapply(names(dfs_Q5), function(nm) {
  func_spline_linear_Q5(dfs_Q5[[nm]], 
df_name = nm)})

# Bind all fit tables and all heteroskedasticity tables
fit_spline_Q5 <- do.call(rbind, lapply(sry_spline_linear_Q5, `[[`, "fit"))
het_spline_Q5 <- do.call(rbind, lapply(sry_spline_linear_Q5, `[[`, "het"))

# Ensure model is a factor with consistent ordering
fit_spline_Q5 <- fit_spline_Q5 %>%
  mutate(model = factor(model, levels = c("linear AQI", "spline AQI")))

fit_spline_Q5 <- fit_spline_Q5 %>%
  mutate(
    # explicit ordering of datasets; adjust if you prefer another order
    dataset = factor(dataset,
                     levels = c(
                       "dfqnt_F_MTSAC_D1D2_Q5",
                       "dfqnt_M_MTSAC_D1D2_Q5",
                       
                       "dfqnt_F_WP_1987_2023_D1D2_Q5",
                       "dfqnt_M_WP_1987_2023_D1D2_Q5",
                       
                       "dfqnt_F_WP_2000_2023_D1D2_Q5",
                       "dfqnt_M_WP_2000_2023_D1D2_Q5")))

het_spline_Q5 <- het_spline_Q5 %>%
  mutate(model = factor(model, levels = c("linear AQI", "spline AQI")))

het_spline_Q5 <- het_spline_Q5 %>%
  mutate(
    # explicit ordering of datasets; adjust if you prefer another order
    dataset = factor(dataset,
                     levels = c(
                       "dfqnt_F_MTSAC_D1D2_Q5",
                       "dfqnt_M_MTSAC_D1D2_Q5",
                       
                       "dfqnt_F_WP_1987_2023_D1D2_Q5",
                       "dfqnt_M_WP_1987_2023_D1D2_Q5",
                       
                       "dfqnt_F_WP_2000_2023_D1D2_Q5",
                       "dfqnt_M_WP_2000_2023_D1D2_Q5")))

# Plot fit spline for Q5
Test_4c_plot_fit_spline_Q5 <- ggplot(fit_spline_Q5, aes(x = dataset, y = RMSE_sec, group = model, color = model)) +
  geom_point(position = position_dodge(width = 0.1), size = 2) +
  geom_line(position = position_dodge(width = 0.1)) +
  labs(
    title = "Test 4c: RMSE in linear and spline models of lagged AQI in top quintile from large schools",
    x = "Dataset",
    y = "RMSE (seconds)"
  ) +
  scale_x_discrete(labels = 
                     c("dfqnt_F_MTSAC_D1D2_Q5" = "F MTSAC",
                       "dfqnt_M_MTSAC_D1D2_Q5" = "M MTSAC",
                       
                       "dfqnt_F_WP_1987_2023_D1D2_Q5" = "F WP\n1987-2023",
                       "dfqnt_M_WP_1987_2023_D1D2_Q5" = "M WP\n1987-2023",
                       
                       "dfqnt_F_WP_2000_2023_D1D2_Q5" = "F WP\n2000-2023",
                       "dfqnt_M_WP_2000_2023_D1D2_Q5" = "M WP\n2000-2023")) +
  scale_y_continuous(labels = comma_format(big.mark = ",", decimal_mark = ".")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(hjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.67,0.85),
        legend.justification = c("right","top"),
        legend.title = element_blank())

Test_4c_plot_fit_spline_Q5

# ^^^
# 
# Plot BP spline by Q5 datasets
Test_4d_plot_BP_spline_Q5 <- ggplot(het_spline_Q5, aes(x = dataset, y = BP_stat_origX, color = model, group = model)) +
  geom_point(position = position_dodge(width = 0.15), size = 2) +
  geom_line(position = position_dodge(width = 0.15)) +
  
  #  facet_wrap(vars(model), ncol = 2, scales = "fixed") +
  
  labs(
    title = "Test 4d: Heteroskedasticity in linear and spline models of lagged AQI",
    x = "Dataset",
    y = "Breusch-Pagan statistic") +
  
  scale_x_discrete(labels = 
                     c("dfqnt_F_MTSAC_D1D2_Q5" = "F MTSAC",
                       "dfqnt_M_MTSAC_D1D2_Q5" = "M MTSAC",
                       
                       "dfqnt_F_WP_1987_2023_D1D2_Q5" = "F WP\n1987-2023",
                       "dfqnt_M_WP_1987_2023_D1D2_Q5" = "M WP\n1987-2023",
                       
                       "dfqnt_F_WP_2000_2023_D1D2_Q5" = "F WP\n2000-2023",
                       "dfqnt_M_WP_2000_2023_D1D2_Q5" = "M WP\n2000-2023")) +
  
  scale_y_continuous(labels = comma_format(big.mark = ",", decimal_mark = ".")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(hjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.67,0.85),
        legend.justification = c("center","top"),
        legend.title = element_blank())

Test_4d_plot_BP_spline_Q5

message(" --- Completed TESTS 4a, 4b, 4c, and 4e to compare linear AQI 14 to spline AQI 14 --- ")


# ========== Test 4e: SIMPLE CORRELATIONS OF FINISH TIMES AND LAGGED AQI
# no figure yet

func_test_AQI_14 <- function(df) {
  df <- df %>% filter(!is.na(linear_school_AQI_14)) %>%
    group_by(year) %>%
    mutate(valid_n = n(),
           cor_fts_AQI_14 = cor(finish_time_seconds,linear_school_AQI_14, use = "complete.obs"),
           mean_fts = mean(finish_time_seconds),
           mean_AQI_14 = mean(linear_school_AQI_14)) %>%
    select(year,valid_n,mean_fts,mean_AQI_14,cor_fts_AQI_14) %>%
    distinct(year,.keep_all = T) %>%
    arrange(desc(cor_fts_AQI_14))}

df_AQI_finish_times <- lapply(dfs_basic,func_test_AQI_14)

func_test_spline_AQI_14 <- function(df) {
  df <- df %>% filter(!is.na(cf_AQI_lag1)) %>%
    group_by(year) %>%
    mutate(valid_n = n(),
           cor_fts_spline1 = cor(finish_time_seconds,cf_AQI_lag1, use = "complete.obs"),
           cor_fts_spline2 = cor(finish_time_seconds,cf_AQI_lag2, use = "complete.obs"),
           cor_fts_spline3 = cor(finish_time_seconds,cf_AQI_lag3, use = "complete.obs"),
           cor_fts_spline4 = cor(finish_time_seconds,cf_AQI_lag4, use = "complete.obs"),
           mean_fts = mean(finish_time_seconds),
           mean_AQI_14 = mean(linear_school_AQI_14)) %>%
    select(year,valid_n,mean_fts,mean_AQI_14,contains("cor_fts_spline")) %>%
    ungroup() %>%
    distinct(year,.keep_all = T) %>%
    arrange(year)}

df_spline_AQI_finish_times <- lapply(dfs_basic,func_test_spline_AQI_14)

df_bind_spline_AQI_finish_times <- rbind(df_spline_AQI_finish_times[[1]],
                                    df_spline_AQI_finish_times[[2]],
                                    df_spline_AQI_finish_times[[3]],
                                    df_spline_AQI_finish_times[[4]],
                                    df_spline_AQI_finish_times[[5]],
                                    df_spline_AQI_finish_times[[6]]) %>% as.data.frame()
                                    

# 
# ========== Test 5: GRADE AS FACTOR OR AS NUMERIC ==========
# with and without interactions
# 4 models for each of 6 datasets, giving 24 rows in the return

message(" --- Running test 5 to compare grade as factor or numeric --- ")

func_grade_coding <- function(df, df_name) {
  # Make sure grade is available as both factor and numeric 9–12
  df <- df %>%
    mutate(
      grade_f = factor(grade),               
      grade_n = as.numeric(as.character(grade)))  # 9,10,11,12 as numeric
  
  # 1) grade as factor, no interactions
  testmdl_fac_noix <- lm(
    finish_time_seconds ~ year_count +
      AQI_daily_on_race_day +
      linear_school_AQI_14 +
      grade_f + division,
    data = df)
  
  # 2) grade as factor, year_count interactions with grade and division
  testmdl_fac_ix <- lm(
    finish_time_seconds ~
      year_count*grade_f + year_count*division +
      AQI_daily_on_race_day +
      linear_school_AQI_14 +
      grade_f + division,
    data = df)
  
  # 3) grade as numeric, no interactions
  testmdl_num_noix <- lm(
    finish_time_seconds ~ year_count +
      AQI_daily_on_race_day +
      linear_school_AQI_14 +
      grade_n + division,
    data = df)
  
  # 4) grade as numeric, year_count interactions with grade and division
  testmdl_num_ix <- lm(
    finish_time_seconds ~
      year_count*grade_n + year_count*division +
      AQI_daily_on_race_day +
      linear_school_AQI_14 +
      grade_n + division,
    data = df)
  
  data.frame(
    dataset   = df_name,
    model     = c("grade_factor_no_ix",
                  "grade_factor_with_ix",
                  "grade_numeric_no_ix",
                  "grade_numeric_with_ix"),
    AIC_value = c(AIC(testmdl_fac_noix),
                  AIC(testmdl_fac_ix),
                  AIC(testmdl_num_noix),
                  AIC(testmdl_num_ix)))
}

AIC_grade_tbl <- do.call(
  rbind,
  lapply(names(dfs_basic), function(nm) {
    func_grade_coding(dfs_basic[[nm]], df_name = nm)
  }))

AIC_grade_tbl <- AIC_grade_tbl %>%
  mutate(
    gender = factor(case_when(
      grepl("^df_F_", dataset) ~ "Female",
      grepl("^df_M_", dataset) ~ "Male",
      TRUE ~ NA_character_)))

AIC_grade_tbl <- AIC_grade_tbl %>%
  mutate(
    # explicit ordering of datasets; adjust if you prefer another order
    dataset = factor(
      dataset,
      levels = c(
        "df_F_MTSAC",
        "df_M_MTSAC",
        
        "df_F_WP_1987_2023",
        "df_M_WP_1987_2023",
        
        "df_F_WP_2000_2023",
        "df_M_WP_2000_2023")))

# drop rows without gender if any
AIC_grade_tbl <- AIC_grade_tbl %>%
  filter(!is.na(gender))

AIC_grade_tbl <- arrange(AIC_grade_tbl,gender, dataset)

Test_5_plot_grade_AIC <- ggplot(AIC_grade_tbl,
                   aes(x = dataset, y = AIC_value,
                       color = model, group = model)) +
  geom_point(position = position_dodge(width = 0.15), size = 2) +
  geom_line(position = position_dodge(width = 0.15)) +
#  facet_wrap(vars(gender), ncol = 2, scales = "fixed") +
  
  labs(
    title = "Test 5: Testing specifications using Akaike information criterion 
    for grade as factor (numeric) with (without) interactions between year:grade and year:division",
    x = "",
    y = "AIC statistic") +
  
  scale_x_discrete(labels = 
      c("df_F_MTSAC" = "F MSTAC",
      "df_M_MTSAC" = "M MTSAC",
      "df_F_WP_1987_2023" = "F WP\n1987-2023",
      "df_M_WP_1987_2023" = "M WP\n1987-2023",
      "df_F_WP_2000_2023" = "F WP\n2000-2023",
      "df_M_WP_2000_2023" = "M WP\n2000-2023")) +
 
  scale_y_continuous(labels = comma_format(big.mark = ",", decimal_mark = ".")) +
  
  scale_color_discrete(labels = c(
    "Grade as factor, without ix",
    "Grade as factor, with ix",
    "Grade as numeric, without ix",
    "Grade as numeric, with ix")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(hjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.67,0.85),
        legend.justification = c("center","top"),
        legend.title = element_blank())

Test_5_plot_grade_AIC

message(" --- Completed Test plot AIC to compare grade as factor or numeric with RE framework --- ")

# ========== Test 6: MORE TESTS ON THE NOISIEST (MTSAC) DATA ==========
# Fit clustered-robust linear AQI 14 models on df_M_MTSAC and df_F_MTSAC
 
testmdl_lin_robust_M_MTSAC <- feols(
finish_time_seconds ~ year_count +
  ozone_racetime +
  temper_racetime +
  linear_school_AQI_14 +
  grade,
cluster = ~ race_name + runner_id,
data     = df_M_MTSAC)

# Fit clustered-robust spline AQI 14 models on df_M_MTSAC
testmdl_spline_robust_M_MTSAC <- feols(
  finish_time_seconds ~ year_count +
    ozone_racetime +
    temper_racetime +
    cf_AQI_lag1 +
    cf_AQI_lag2 +
    cf_AQI_lag3 +
    cf_AQI_lag4 +
    grade,
  cluster = ~ race_name + runner_id,
  data     = df_M_MTSAC)

lin_df_M_MTSAC <- df_M_MTSAC %>%
  filter(!is.na(linear_school_AQI_14))

spline_df_M_MTSAC <- df_M_MTSAC %>%
  filter(!is.na(cf_AQI_lag1))


list_table_M_AQI <- list(
  `linear M AQI 14` = testmdl_lin_robust_M_MTSAC,
  `spline M AQI 14` = testmdl_spline_robust_M_MTSAC)

Test_6a_table_M_AQI <- modelsummary(
  list_table_M_AQI,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("year_count" = "Year",
                  "ozone_racetime" = "Hourly ozone",
                  "temper_racetime" = "Hourly temper",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12",
                  "program_score" = "Program score",
                  "linear_school_AQI_14" = "School AQI",
                  "cf_AQI_lag1" = "AQI lag 1",
                  "cf_AQI_lag2" = "AQI lag 2",
                  "cf_AQI_lag3" = "AQI lag 3",
                  "cf_AQI_lag4" = "AQI lag 4"),
  
  title = "Test 6a: Male times, treatments linear and spline AQI, MT SAC")

Test_6a_table_M_AQI <- Test_6a_table_M_AQI %>%
  line_spacing(space = 0.9, part = "all") %>%    # single-spacing
  padding(padding.top = 3, padding.bottom = 0,  # no extra padding
          part = "all")

Test_6a_table_M_AQI <- set_table_properties(Test_6a_table_M_AQI,layout = "autofit", width = 1)

# female races
# fit clustered_robust linear AQI 14 models on df_F_MTSAC
testmdl_lin_robust_F_MTSAC <- feols(
  finish_time_seconds ~ year_count +
    ozone_racetime +
    temper_racetime +
    linear_school_AQI_14 +
    grade,
  cluster = ~ race_name + runner_id,
  data     = df_F_MTSAC)

# Fit clustered-robust spline AQI 14 models on df_F_MTSAC
testmdl_spline_robust_F_MTSAC <- feols(
  finish_time_seconds ~ year_count +
    ozone_racetime +
    temper_racetime +
    cf_AQI_lag1 +
    cf_AQI_lag2 +
    cf_AQI_lag3 +
    cf_AQI_lag4 +
    grade,
  cluster = ~ race_name + runner_id,
  data     = df_F_MTSAC)

lin_df_F_MTSAC <- df_F_MTSAC %>%
  filter(!is.na(linear_school_AQI_14))

spline_df_F_MTSAC <- df_F_MTSAC %>%
  filter(!is.na(cf_AQI_lag1))

list_table_F_AQI <- list(
  `linear F AQI 14` = testmdl_lin_robust_F_MTSAC,
  `spline F AQI 14` = testmdl_spline_robust_F_MTSAC)

Test_6b_table_F_AQI <- modelsummary(
  list_table_F_AQI,
  output = "flextable",
  estimate = c("{estimate}{stars}"),
  gof_omit = ".IC|Lik",
  coef_rename = c("year_count" = "Year",
                  "ozone_racetime" = "Hourly ozone",
                  "temper_racetime" = "Hourly temper",
                  "grade10" = "Grade 10",
                  "grade11" = "Grade 11",
                  "grade12" = "Grade 12",
                  "program_score" = "Program score",
                  "linear_school_AQI_14" = "School AQI",
                  "cf_AQI_lag1" = "AQI lag 1",
                  "cf_AQI_lag2" = "AQI lag 2",
                  "cf_AQI_lag3" = "AQI lag 3",
                  "cf_AQI_lag4" = "AQI lag 4"),
  
title = "Test 6b: Female times, treatments linear and spline AQI, MT SAC")

Test_6b_table_F_AQI <- Test_6b_table_F_AQI %>%
  line_spacing(space = 0.9, part = "all") %>%    # single-spacing
  padding(padding.top = 3, padding.bottom = 0,  # no extra padding
          part = "all")

Test_6b_table_F_AQI <- set_table_properties(Test_6b_table_F_AQI,
                                            layout = "autofit", width = 1)

lst_estimate <- ls(pattern = "Test_\\d{1}")

# 
# url <- "https://cdn.posit.co/posit-ai/manifest.json"
# readLines(url, warn = FALSE)
# 
# readLines("https://cran.r-project.org", warn = FALSE)
# 
# getOption("download.file.method")
# system("echo $http_proxy && echo $https_proxy")
