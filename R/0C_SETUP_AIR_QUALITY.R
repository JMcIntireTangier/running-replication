# 0C_DESCRIBE_AIR_QUALITY.
# called by MARKDOWN_RUNNING.Rmd
# mainly used to create dfs for figures in 3b_RENDER_FIGURES.R

# Figure 1a AQI FRESNO daily
# Figure 1b PM25 FRESNO daily
# Figure 1c AQI LA daily
# Figure 1d PM25 LA daily
 
# Figure 2a WP ozone hourly 
# Figure_2b WP temper hourly
# Figure 2c MTSAC ozone hourly
# Figure 2d MTSAC temper hourly 

# ggplots created with name of the format "plot_.+$"
# are NOT in the paper and are used to describe the data


# ===== WP AQI and PM25 DAILY =====
# this creates figs 1a and 1b for AQI PM25 WP daily


df_WP_AQI_PM25_daily_1980_2023 <- readRDS("send_WP_AQI_PM25_DAILY.RDS")

df_WP_AQI_PM25_daily_1980_2023 <- df_WP_AQI_PM25_daily_1980_2023 %>%
  mutate(groupmonth = factor(case_when(
    month_AQI >= 9 ~ "Cross-country season",
    TRUE ~ "Training season")))

df_WP_AQI_PM25_daily_1980_2023$month_AQI <- str_replace_all(
  df_WP_AQI_PM25_daily_1980_2023$month_AQI,
  c("9" = "September", "10" = "October",
  "11" = "November", "12" = "December"))

df_WP_AQI_PM25_daily_1980_2023 <- df_WP_AQI_PM25_daily_1980_2023 %>%
  mutate(month_AQI = factor(month_AQI))

df_WP_AQI_PM25_daily_1980_2023 <- df_WP_AQI_PM25_daily_1980_2023 %>%
  mutate(month_AQI = fct_relevel(month_AQI,"September", "October", "November","December"))

df_WP_AQI_PM25_daily_1980_2023$month_PM25 <- str_replace_all(
  df_WP_AQI_PM25_daily_1980_2023$month_PM25,
  c("9" = "September", "10" = "October",
    "11" = "November", "12" = "December"))

df_WP_AQI_PM25_daily_1980_2023 <- df_WP_AQI_PM25_daily_1980_2023 %>%
  mutate(month_PM25 = factor(month_PM25))

df_WP_AQI_PM25_daily_1980_2023 <- df_WP_AQI_PM25_daily_1980_2023 %>%
  mutate(month_PM25 = fct_relevel(month_PM25,"September", "October", "November","December"))

df_WP_AQI_PM25_daily_1980_2023 <- df_WP_AQI_PM25_daily_1980_2023 %>%
  filter(AQI_daily_site_name == "Clovis-Villa" | AQI_daily_site_name == "Fresno-Sky Park")

df_WP_AQI_PM25_daily_1980_2023 <- df_WP_AQI_PM25_daily_1980_2023 %>%
  mutate(groupday = factor(case_when(
    weekday_AQI == "Friday" ~ "Weekend",
    weekday_AQI == "Saturday" ~ "Weekend",
    weekday_AQI == "Sunday" ~ "Weekend",
    TRUE ~ "Workday")))

df_WP_AQI_PM25_daily_1980_2023 <- df_WP_AQI_PM25_daily_1980_2023 %>%
  group_by(year_AQI,month_AQI, groupday) %>%
  mutate(
    meanDailyAQI = mean(AQI_daily_at_site, na.rm = T),
    meanDailyOzone = mean(ozone_daily_at_site,na.rm = T),
    meanDailyPM25 = mean(PM25_daily_mean_at_site, na.rm = T)) %>%
  ungroup()


# ===== MTSAC AQI and PM25 DAILY  =====
# creates figs 1c and 1d for AQI LA daily


df_MTSAC_AQI_PM25_daily_1980_2023 <- readRDS("send_MTSAC_AQI_PM25_DAILY.RDS")

df_MTSAC_AQI_PM25_daily_1980_2023 <- df_MTSAC_AQI_PM25_daily_1980_2023 %>%
  mutate(groupmonth = factor(case_when(
    month_AQI >= 9 ~ "Cross-country season",
    TRUE ~ "Training season")))

df_MTSAC_AQI_PM25_daily_1980_2023 <- df_MTSAC_AQI_PM25_daily_1980_2023 %>%
  mutate(month_AQI = factor(month_AQI))

df_MTSAC_AQI_PM25_daily_1980_2023$month_AQI <- str_replace_all(
  df_MTSAC_AQI_PM25_daily_1980_2023$month_AQI,
  c("9" = "September", "10" = "October",
    "11" = "November", "12" = "December"))

df_MTSAC_AQI_PM25_daily_1980_2023 <- df_MTSAC_AQI_PM25_daily_1980_2023 %>%
  mutate(month_AQI = fct_relevel(month_AQI,"September", "October", "November","December"))

df_MTSAC_AQI_PM25_daily_1980_2023 <- df_MTSAC_AQI_PM25_daily_1980_2023 %>%
  mutate(month_PM25 = factor(month_PM25))

df_MTSAC_AQI_PM25_daily_1980_2023$month_PM25 <- str_replace_all(
  df_MTSAC_AQI_PM25_daily_1980_2023$month_PM25,
  c("9" = "September", "10" = "October",
    "11" = "November", "12" = "December"))

df_MTSAC_AQI_PM25_daily_1980_2023 <- df_MTSAC_AQI_PM25_daily_1980_2023 %>%
  mutate(month_PM25 = fct_relevel(month_PM25,"September", "October", "November","December"))

df_MTSAC_AQI_PM25_daily_1980_2023 <- df_MTSAC_AQI_PM25_daily_1980_2023 %>%
  mutate(groupday = factor(case_when(
    weekday_AQI == "Friday" ~ "Weekend",
    weekday_AQI == "Saturday" ~ "Weekend",
    weekday_AQI == "Sunday" ~ "Weekend",
    TRUE ~ "Workday")))

df_MTSAC_AQI_PM25_daily_1980_2023 <- df_MTSAC_AQI_PM25_daily_1980_2023 %>%
  filter(AQI_daily_site_name == "Azusa" | AQI_daily_site_name == "Glendora")

df_MTSAC_AQI_PM25_daily_1980_2023 <- df_MTSAC_AQI_PM25_daily_1980_2023 %>%
  filter(groupmonth == "Cross-country season")

df_MTSAC_AQI_PM25_daily_1980_2023 <- df_MTSAC_AQI_PM25_daily_1980_2023 %>%
  group_by(year_AQI, month_AQI, groupday) %>%
  mutate(
    meanDailyAQI = mean(AQI_daily, na.rm = T),
    meanDailyOzone = mean(ozone_daily,na.rm = T),
    meanDailyPM25 = mean(PM25_daily_mean, na.rm = T)) %>%
  ungroup()


# ===== GET WP OZONE HOURLY LT ======
# read from send WP OZONE HOURLY LT RDS for Fig 2a


df_WP_ozone_hourly_lt <- readRDS("SEND_WP_OZONE_HOURLY_LT.RDS")

df_WP_ozone_hourly_lt <- filter(df_WP_ozone_hourly_lt, ref_hour %in% c(8,10,12,14))

 df_WP_ozone_hourly_lt$month <- str_replace_all(
   df_WP_ozone_hourly_lt$month,
   c("9" = "September", "10" = "October", "11" = "November"))

df_WP_ozone_hourly_lt$month <- as.factor(df_WP_ozone_hourly_lt$month)

df_WP_ozone_hourly_lt <- df_WP_ozone_hourly_lt %>%
   mutate(month = fct_relevel(month,
     "September", "October", "November"))

df_WP_ozone_hourly_lt <- df_WP_ozone_hourly_lt %>%
      mutate(groupday =
        case_when(weekday %in% c("Friday", "Saturday", "Sunday") ~ "Weekend",
        TRUE ~ "Workday"))

df_WP_ozone_hourly_lt$groupday <- factor(df_WP_ozone_hourly_lt$groupday)

df_WP_ozone_hourly_lt <- df_WP_ozone_hourly_lt %>%
  filter(ozone_hourly_distance_to_WP <= 25)
  
df_WP_ozone_hourly_lt <- df_WP_ozone_hourly_lt %>%
  group_by(year, month, groupday, ref_hour) %>%
  reframe(
    N_obs = n(),
    Mean_ozone_distance_to_WP = mean(ozone_hourly_distance_to_WP, na.rm = T),
    Mean_ozone_hourly = mean(ozone_hourly, na.rm = T),
    SD_ozone = sd(ozone_hourly, na.rm = T),
    CV_ozone = SD_ozone / Mean_ozone_hourly) %>%
  ungroup()


#
# ===== GET WP TEMPER HOURLY LT  =====
# 


df_WP_temper_hourly_lt <- readRDS("SEND_WP_TEMPER_HOURLY_LT.RDS")

df_WP_temper_hourly_lt <- df_WP_temper_hourly_lt %>%
  filter(year >= 1980)

df_WP_temper_hourly_lt <- df_WP_temper_hourly_lt %>%
  filter(between(month,9,11))
  
df_WP_temper_hourly_lt$month <- str_replace_all(
  df_WP_temper_hourly_lt$month,
  c("9" = "September", "10" = "October",
    "11" = "November", "12" = "December"))

df_WP_temper_hourly_lt <- df_WP_temper_hourly_lt %>%
  mutate(month = fct(month))

df_WP_temper_hourly_lt <- df_WP_temper_hourly_lt %>%
  mutate(groupday = factor(case_when(
    weekday == "Friday" ~ "Weekend",
    weekday == "Saturday" ~ "Weekend",
    weekday == "Sunday" ~ "Weekend",
    TRUE ~ "Workday")))

df_WP_temper_hourly_lt <- df_WP_temper_hourly_lt %>%
  filter(ref_hour %in% c(8,10,12,14))
  
df_WP_temper_hourly_lt <- df_WP_temper_hourly_lt %>%
  group_by(year,month,ref_hour,groupday) %>%
  summarise(mean_temper_hourly = mean(temper_hourly,na.rm = TRUE),
         N_temper_hourly = n(),
         .groups = "drop")

# ===== MTSAC OZONE HOURLY LT=====

df_MTSAC_ozone_hourly_lt <- readRDS("SEND_MTSAC_OZONE_HOURLY_LT.RDS")

df_MTSAC_ozone_hourly_lt$month <- str_replace_all(
  df_MTSAC_ozone_hourly_lt$month,
  c("9" = "September", "10" = "October", "11" = "November"))

df_MTSAC_ozone_hourly_lt$month <- as.factor(df_MTSAC_ozone_hourly_lt$month)

df_MTSAC_ozone_hourly_lt <- df_MTSAC_ozone_hourly_lt %>%
  mutate(month = fct_relevel(month,
    "September", "October", "November"))

df_MTSAC_ozone_hourly_lt <- df_MTSAC_ozone_hourly_lt %>%
  mutate(groupday =
           case_when(weekday %in% c("Friday", "Saturday", "Sunday") ~ "Weekend",
                     TRUE ~ "Workday"))

df_MTSAC_ozone_hourly_lt$groupday <- factor(df_MTSAC_ozone_hourly_lt$groupday)

df_MTSAC_ozone_hourly_lt <- df_MTSAC_ozone_hourly_lt %>%
  filter(ozone_hourly_distance_to_MTSAC <= 25)

df_MTSAC_ozone_hourly_lt <- filter(df_MTSAC_ozone_hourly_lt, ref_hour %in% c(8,10,12,14))

df_MTSAC_ozone_hourly_lt <- df_MTSAC_ozone_hourly_lt %>%
  group_by(year, month, groupday, ref_hour) %>%
  reframe(
    N_obs = n(),
    Mean_ozone_distance_to_MTSAC = mean(ozone_hourly_distance_to_MTSAC, na.rm = T),
    Mean_ozone_hourly = mean(ozone_hourly, na.rm = T),
    SD_ozone = sd(ozone_hourly, na.rm = T),
    CV_ozone = SD_ozone / Mean_ozone_hourly) %>%
  ungroup()

sry_MTSAC_ozone_hourly_lt <- df_MTSAC_ozone_hourly_lt %>%
  group_by(month,groupday,ref_hour) %>%
  summarise(N_obs = n(),
            mean_ozone_hourly_max = mean(Mean_ozone_hourly))

vl_MTSAC_ozone_hourly_max <- sry_MTSAC_ozone_hourly_lt %>% 
  group_by(groupday) %>% 
  slice_max(mean_ozone_hourly_max, n = 1) %>% 
  ungroup() %>%          # optional, if you don't want grouped output
  select(groupday, ref_hour,mean_ozone_hourly_max)

vl_MTSAC_ozone_hourly_min <-sry_MTSAC_ozone_hourly_lt %>% 
  group_by(groupday) %>% 
  slice_min(mean_ozone_hourly_max, n = 1) %>% 
  ungroup() %>%          # optional, if you don't want grouped output
  select(groupday, ref_hour,mean_ozone_hourly_max)

#
# ===== GET MTSAC TEMPER HOURLY LT  =====
# 

df_MTSAC_temper_hourly_lt <- readRDS("SEND_MTSAC_TEMPER_HOURLY_LT.RDS")

df_MTSAC_temper_hourly_lt <- df_MTSAC_temper_hourly_lt %>%
  filter(year >= 1980)

df_MTSAC_temper_hourly_lt <- df_MTSAC_temper_hourly_lt %>%
  filter(between(month,9,11))

df_MTSAC_temper_hourly_lt$month <- str_replace_all(
  df_MTSAC_temper_hourly_lt$month,
  c("9" = "September", "10" = "October",
    "11" = "November"))

df_MTSAC_temper_hourly_lt <- df_MTSAC_temper_hourly_lt %>%
  mutate(month = fct(month))

df_MTSAC_temper_hourly_lt <- df_MTSAC_temper_hourly_lt %>%
  mutate(weekday = fct(weekday))

df_MTSAC_temper_hourly_lt <- df_MTSAC_temper_hourly_lt %>%
  mutate(groupday = factor(case_when(
    weekday == "Friday" ~ "Weekend",
    weekday == "Saturday" ~ "Weekend",
    weekday == "Sunday" ~ "Weekend",
    TRUE ~ "Workday")))

df_MTSAC_temper_hourly_lt <- df_MTSAC_temper_hourly_lt %>%
  filter(ref_hour %in% c(8,10,12,14))

df_MTSAC_temper_hourly_lt <- df_MTSAC_temper_hourly_lt %>%
  group_by(year,month,ref_hour,groupday) %>%
  summarise(mean_temper_hourly = mean(temper_hourly,na.rm = TRUE),
            N_temper_hourly = n(),
            .groups = "drop")


# ===== LONG TERM RELATION BETWEEN OZONE AND AQI IN FRESNO

# NB this code checks relation b/w ozone and AQI in Fresno
# not used in the paper

df_WP_AQI_PM25_daily_1980_2023 <- df_WP_AQI_PM25_daily_1980_2023 %>%
  mutate(year_count = year_AQI - 1986)

cor(df_WP_AQI_PM25_daily_1980_2023$meanDailyAQI,
    df_WP_AQI_PM25_daily_1980_2023$meanDailyOzone)

df_WP_AQI_PM25_daily_1980_2023 %>% filter(year_AQI <= 2000) %>% 
  select(contains("mean")) %>% summary()

df_WP_AQI_PM25_daily_1980_2023 %>% filter(year_AQI >= 2001) %>% 
  select(contains("mean")) %>%   summary()

df_WP_AQI_PM25_daily_1980_2023 %>% filter(groupday == "Weekend") %>% 
  select(contains("mean")) %>% summary()

df_WP_AQI_PM25_daily_1980_2023 %>% filter(groupday == "Workday") %>% 
  select(contains("mean")) %>%   summary()

reg_WP_lt_ozone_AQI <- lm(meanDailyOzone ~ year_count +
                                month_AQI + groupday, 
                              data = df_WP_AQI_PM25_daily_1980_2023)

tidy(reg_WP_lt_ozone_AQI)

# ===== LONG TERM RELATION BETWEEN OZONE AND AQI IN LA COUNTY

df_MTSAC_AQI_PM25_daily_1980_2023 <- df_MTSAC_AQI_PM25_daily_1980_2023 %>%
  mutate(year_count = year_AQI - 1979)

cor(df_MTSAC_AQI_PM25_daily_1980_2023$meanDailyAQI,
    df_MTSAC_AQI_PM25_daily_1980_2023$meanDailyOzone)

df_MTSAC_AQI_PM25_daily_1980_2023 %>% filter(year_AQI <= 2000) %>% 
  select(contains("mean")) %>% summary()

df_MTSAC_AQI_PM25_daily_1980_2023 %>% filter(year_AQI >= 2001) %>% 
  select(contains("mean")) %>%   summary()

df_MTSAC_AQI_PM25_daily_1980_2023 %>% filter(groupday == "Weekend") %>% 
  select(contains("mean")) %>% summary()

df_MTSAC_AQI_PM25_daily_1980_2023 %>% filter(groupday == "Workday") %>% 
  select(contains("mean")) %>%   summary()

reg_LA_lt_ozone_AQI <- lm(meanDailyOzone ~ year_count +
                            month_AQI + groupday, 
                          data = df_MTSAC_AQI_PM25_daily_1980_2023)

tidy(reg_LA_lt_ozone_AQI)

