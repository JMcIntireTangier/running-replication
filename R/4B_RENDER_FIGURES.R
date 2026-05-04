# FILE 4B_RENDER_FIGURES.R

# ===== PLOT ENVIRONMENTAL VARIABLES =====
# 
# Figure 1a WP temperature
# Figure 1b MTSAC temperature
# 
# Fig 2a WP AQI daily
# Fig 2b WP PM25 daily
# Fig 2c MTSAC AQI daily
# Fig 2d MTSAC PM25 daily

# NB the hjust = argument in element_text|element_markdown is
# relative to the left margin
# hjust = 0 is left; hjust = 1 is right


Figure_1a_WP_temper_hourly_lt <- ggplot(data = df_WP_temper_hourly_lt,
    aes(x = year,y = mean_temper_hourly, color = factor(ref_hour))) +
  geom_point(aes(y = mean_temper_hourly, color = factor(ref_hour)), size = 1, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Fig. 1a: Woodward Park hourly temperature since the 1980s:",
    subtitle = "<span style='color:blue'>8 AM</span>, <span style='color:red'>10 AM</span>, <span style='color:dodgerblue'>12 PM</span>, <span style='color:brown'>2 PM</span>",
    y = "Temperature in degrees C",
    x = "",
    caption = "Data source: [6]") +
  scale_x_continuous(breaks = seq(1980, 2023, by = 10)) +
  scale_y_continuous(breaks = seq(0, max(df_WP_temper_hourly_lt$mean_temper_hourly, na.rm = TRUE), by = 5)) +
  scale_color_manual(
    values = c("8" = "blue", "10" = "red", "12" = "dodgerblue", "14" = "brown"),
    name = "AM hour"
  ) +
  
  facet_grid(rows = vars(month), cols = vars(groupday),
             
             labeller = labeller(month,
            groupday = c("Workday" = "Workday (M-Th)", "Weekend" = "Weekend (F-Su)")))  +
  
  theme_running() +
  theme(plot.title = element_text(hjust = 0., margin = margin(b = 4)),
        plot.subtitle = element_markdown(hjust = 0.0, size = 10, margin = margin(t = 2, b = 2)),
        legend.position = "none",
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_line(color = "gray98"),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0.))


Figure_1b_MTSAC_temper_hourly_lt <- ggplot(data = df_MTSAC_temper_hourly_lt, 
  aes(x = year,y = mean_temper_hourly, color = factor(ref_hour))) +
  geom_point(aes(y = mean_temper_hourly, color = factor(ref_hour)), size = 1, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Fig. 1b: Mt SAC hourly temperature has risen since 1980:",
    subtitle = "<span style='color:blue'>8 AM</span>, <span style='color:red'>10 AM</span>, <span style='color:dodgerblue'>12 PM</span>, <span style='color:brown'>2 PM</span>",
    y = "Temperature in degrees C",
    x = "",
    caption = "Data source: [6]") +
  scale_x_continuous(breaks = seq(1980, 2023, by = 10)) +
  scale_y_continuous(breaks = 
                       seq(0, max(df_MTSAC_temper_hourly_lt$mean_temper_hourly, na.rm = TRUE), by = 5),
                     limits = c(10,35)) +
  scale_color_manual(
    values = c("8" = "blue", "10" = "red", "12" = "dodgerblue", "14" = "brown"),
    name = "AM hour"
  ) +
  
  facet_grid(rows = vars(month), cols = vars(groupday),
             
             labeller = labeller(month,
                                 groupday = c("Workday" = "Workday (M-Th)", "Weekend" = "Weekend (F-Su)")))  +
  
  theme_running() +
  theme(plot.title = element_text(hjust = 0., margin = margin(b = 4)), 
        plot.subtitle = element_markdown(hjust = 0.0, size = 10, margin = margin(t = 2, b = 2)),
        legend.position = "none",
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_line(color = "gray98"),
        plot.caption.position = "plot",  
        plot.caption = element_text(hjust = 0.))

Figure_2a_WP_AQI_daily <- ggplot(df_WP_AQI_PM25_daily_1980_2023,
     aes(x = year_AQI, y = meanDailyAQI, group = groupday, color = groupday)) +
     geom_smooth(method = "loess") +
     facet_wrap(vars(month_AQI), ncol = 2, scales = "fixed") +
     labs(
          title = "Fig. 2a: Fresno daily AQI has improved since 1987",
          subtitle = "with little difference between <span style='color:brown'>workday (M-Th)</span> and <span style='color:gold'>weekend (F-Su)</span>",
          x = "",
          y = "Mean daily air quality index",
          color = "",
          caption = "Data source: [6]") +
     scale_x_continuous(breaks = seq(1987, 2023, by = 6)) +
     scale_color_manual(values = c("Workday" = "brown", "Weekend" = "gold")) +
     theme_running() +
     theme(plot.title = element_text(hjust = 0., margin = margin(b = 4)),
           plot.subtitle = element_markdown(hjust = 0.0, size = 10, margin = margin(t = 2, b = 2)),
          legend.position = "none",
          panel.grid.major = element_line(color = "gray95"),
          panel.grid.minor = element_line(color = "gray98"),
          plot.caption.position = "plot",  # Aligns relative to entire plot
          plot.caption = element_text(hjust = 0.)) 


Figure_2b_MTSAC_AQI_daily <- ggplot(df_MTSAC_AQI_PM25_daily_1980_2023 ,
    aes(x = year_AQI, y = meanDailyAQI, group = groupday, color = groupday)) +
  geom_smooth(method = "loess") +
  facet_wrap(vars(month_AQI), ncol = 2, scales = "fixed") +
  labs(
    title = "Fig. 2b: Mt SAC daily AQI improved in the 80s & 90s",
    subtitle = "with weekend being greater than workday <span style='color:brown'>workday (M-Th)</span> and
          <span style='color:gold'>weekend (F-Su)</span>",
    x = "",
    y = "Mean daily air quality index",
    caption = "Data source: [6]",
    color = "") +
  scale_x_continuous(breaks = seq(1980, 2023, by = 6)) +
  scale_color_manual(values = c("Workday" = "brown", "Weekend" = "gold")) +
  theme_running() +
  theme(plot.title = element_text(hjust = 0., margin = margin(b = 4)),
        plot.subtitle = element_markdown(hjust = 0.,size = 10, margin = margin(t = 2, b = 2)),
        legend.position = "none",
        plot.caption.position = "plot",  
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_line(color = "gray98"),
        plot.caption = element_text(hjust = 0.))

Figure_2c_WP_ozone_hourly_lt <- ggplot(data = df_WP_ozone_hourly_lt, aes(x = year)) +
  geom_point(aes(y = Mean_ozone_hourly, color = factor(ref_hour)), size = 1, alpha = 0.4) +
  geom_smooth(aes(y = Mean_ozone_hourly, color = factor(ref_hour)), method = "loess", se = FALSE) +
  labs(
    title = "Fig. 2c: Fresno hourly ozone fell in September and October",
    subtitle = "Hours: <span style='color:blue'>8 AM</span>, <span style='color:red'>10 AM</span>, <span style='color:dodgerblue'>12 PM</span>, <span style='color:brown'>2 PM</span>",
    y = "Ozone in parts per billion",
    x = "",
    caption = "Data source: [6]"
  ) +
  scale_x_continuous(breaks = seq(1980, 2023, by = 10)) +
  scale_y_continuous(breaks = seq(0, max(df_WP_ozone_hourly_lt$Mean_ozone_hourly, na.rm = TRUE), by = 20)) +
  scale_color_manual(
    values = c("8" = "blue", "10" = "red", "12" = "dodgerblue", "14" = "brown"),
    name = "AM hour") +
  
  facet_wrap(vars(month, groupday), nrow = 3, scales = "fixed",
             labeller = labeller(groupday = c("Workday" = "Workday (M-Th)", "Weekend" = "Weekend (F-Su)"))) +
  
  theme_running() +
  
  theme(plot.title = element_text(hjust = 0., margin = margin(b = 4)), 
        plot.subtitle = element_markdown(hjust = 0.0, size = 10, margin = margin(t = 2, b = 2)),
        legend.position = "none",
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_line(color = "gray98"),
        plot.caption.position = "plot",  
        plot.caption = element_text(hjust = 0.))

Figure_2d_MTSAC_ozone_hourly_lt <- ggplot(data = df_MTSAC_ozone_hourly_lt, aes(x = year)) +
  geom_point(aes(y = Mean_ozone_hourly, color = factor(ref_hour)), size = 1, alpha = 0.4) +
  geom_smooth(aes(y = Mean_ozone_hourly, color = factor(ref_hour)), method = "loess", se = FALSE) +
  labs(
    title = "Fig. 2d: Mt San Antonio College hourly ozone fell in September and October",
    subtitle = "Hours: <span style='color:blue'>8 AM</span>, <span style='color:red'>10 AM</span>, <span style='color:dodgerblue'>12 PM</span>, <span style='color:brown'>2 PM</span>",
    y = "Ozone in parts per billion",
    x = "",
    caption = "Data source: [6]"
  ) +
  scale_x_continuous(breaks = seq(1980, 2023, by = 10)) +
  scale_y_continuous(breaks = 
                       seq(0, max(df_MTSAC_ozone_hourly_lt$Mean_ozone_hourly, na.rm = TRUE), by = 20),
                     limits = c(0,120)) +
  scale_color_manual(
    values = c("8" = "blue", "10" = "red", "12" = "dodgerblue", "14" = "brown"),
    name = "AM hour"
  ) +
  facet_wrap(vars(month, groupday), nrow = 3, scales = "fixed",
             labeller = labeller(groupday = c("Workday" = "Workday (M-Th)", "Weekend" = "Weekend (F-Su)"))) +
  theme_running() +
  theme(plot.title = element_text(hjust = 0., margin = margin(b = 4)), 
        plot.subtitle = element_markdown(hjust = 0.0, size = 10, margin = margin(t = 2, b = 2)),
        legend.position = "none",
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_line(color = "gray98"),
        plot.caption.position = "plot",  
        plot.caption = element_text(hjust = 0.))

df_WP_PM25_daily_1980_2023 <- df_WP_AQI_PM25_daily_1980_2023 %>%
  filter(!is.na(month_PM25))

Figure_2e_WP_PM25_daily <- ggplot(df_WP_PM25_daily_1980_2023,
                                  aes(x = year_PM25, y = meanDailyPM25, group = groupday, color = groupday)) +
  geom_smooth(method = "loess") +
  facet_wrap(vars(month_PM25), ncol = 2, scales = "fixed") +
  labs(
    title = "Fig. 2e: Fresno daily PM 2.5 fell since 1999",
    subtitle = "with little difference between <span style='color:brown'>workday (M-Th)</span> and <span style='color:gold'>weekend (F-Su)</span>",
    x = "",
    y = "Micrograms/cubic meter",
    color = "",
    caption = "Data source: [6]") +
  scale_x_continuous(breaks = seq(1999, 2023, by = 4)) +
  scale_color_manual(values = c("Workday" = "brown", "Weekend" = "gold")) +
  theme_running() +
  theme(plot.title = element_text(hjust = 0., margin = margin(b = 4)),
        plot.subtitle = element_markdown(hjust = 0.0, size = 10, margin = margin(t = 2, b = 2)),
        legend.position = "none",
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_line(color = "gray98"),
        plot.caption.position = "plot",  # Aligns relative to entire plot
        plot.caption = element_text(hjust = 0.)) # hjust = 0 is left; hjust = 1 is right

df_MTSAC_PM25_daily_1980_2023 <- df_MTSAC_AQI_PM25_daily_1980_2023 %>%
  filter(!is.na(month_PM25))

Figure_2f_MTSAC_PM25_daily <- ggplot(df_MTSAC_PM25_daily_1980_2023,
    aes(x = year_PM25, y = meanDailyPM25, group = groupday, color = groupday)) +
  geom_smooth(method = "loess") +
  facet_wrap(vars(month_PM25), ncol = 2, scales = "fixed") +
  labs(
    title = "Fig. 2f: Mt SAC daily PM2.5 improved from 1999 - 2010",
    subtitle = "and was stable after 2010, with little difference between <span style='color:brown'>workday (M-Th)</span> and
          <span style='color:gold'>weekend (F-Su)</span>",
    x = "",
    y = "Micrograms/cubic meter",
    caption = "Data source: [6]",
    color = "") +
  scale_x_continuous(breaks = seq(1999, 2022, by = 3)) +
  scale_color_manual(values = c("Workday" = "brown", "Weekend" = "gold")) +
  theme_running() +
  theme(plot.title = element_text(hjust = 0., margin = margin(b = 4)),
        plot.subtitle = element_markdown(hjust = 0.,size = 10, margin = margin(t = 2, b = 2)),
        legend.position = "none",
        plot.caption.position = "plot",  
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_line(color = "gray98"),
        plot.caption = element_text(hjust = 0.))



# ===== PLOT FINISH TIMES =====
# 
# finish time data to be plotted 
# Fig 3 WP, 1987-2023
# 
# regression data to be plotted 
# Fig 4a WP, 1987-2023
# Fig 4b, WP, 1987-2023, top quintile
# Fig 4c, WP, 2000-2023
# Fig 4d, WP, 2000-2023

df_F_WP_1987_2023 <- df_F_WP_1987_2023 %>%
  group_by(year,division,grade) %>%
  mutate(YDG_N = n(),
         YDG_mean_finish_time = round(mean(finish_time_seconds, na.rm = T),1),
         YDG_sd_finish_time = round(sd(finish_time_seconds,na.rm = T),2)) %>% 
  ungroup()

df_F_WP_1987_2023 <- df_F_WP_1987_2023 %>%
  group_by(year,division,grade) %>%
  mutate(YDG_z_score = (finish_time_seconds - YDG_mean_finish_time) / YDG_sd_finish_time) %>%
  ungroup()

df_M_WP_1987_2023 <- df_M_WP_1987_2023 %>%
  group_by(year,division,grade) %>%
  mutate(YDG_N = n(),
         YDG_mean_finish_time = round(mean(finish_time_seconds, na.rm = T),1),
         YDG_sd_finish_time = round(sd(finish_time_seconds,na.rm = T),2)) %>% 
  ungroup()

df_M_WP_1987_2023 <- df_M_WP_1987_2023 %>%
  group_by(year,division,grade) %>%
  mutate(YDG_z_score = (finish_time_seconds - YDG_mean_finish_time) / YDG_sd_finish_time) %>%
  ungroup()

df_WP_pooled_1987_2023 <- rbind(df_F_WP_1987_2023,df_M_WP_1987_2023)

# Fig 3

sry_pooled_WP <- df_WP_pooled_1987_2023 %>%
  group_by(year, gender, division, grade) %>%
  summarise(
    YDG_mean_finish_time = mean(finish_time_seconds, na.rm = TRUE),
    n = n(),
    .groups = "drop") %>%
  mutate(
    division = factor(division, levels = c("D1","D2","D3","D4","D5")),
    grade = factor(grade, levels = c("9","10","11","12")))

Figure_3_pooled_WP_1987_2023 <- ggplot(sry_pooled_WP,
     aes(x = year, y = YDG_mean_finish_time, 
     group = gender, color = gender)) +
  
    geom_point(size = 0.8, alpha = 0.4) +
  
     geom_smooth(method = "loess") +

    geom_hline(yintercept = 1200, linetype = "dotted", color = "gray50", alpha = 0.4) +
    
     facet_grid(rows = vars(division), cols = vars(grade),
               
labeller = labeller(division = c("D1" = "Division 1",
                                 "D2" = "Division 2",
                                 "D3" = "Division 3",
                                 "D4" = "Division 4",
                                 "D5" = "Division 5"),

        grade = c("9" = "Grade 9",  "10" = "Grade 10",
          "11" = "Grade 11", "12" = "Grade 12"))) +
  
     labs(
          title = "Fig. 3: Mean annual finish times at Woodward Park, 1987-2023",
          subtitle = "by division and grade for <span style='color:dodgerblue'>male</span> and <span style='color:red'>female</span> runners",
          x = "",
          y = "Finish time in seconds",
          caption = "Data source: [15]",
          color = "") +
     scale_x_continuous(breaks = seq(1987, 2023, by = 10)) +
     scale_color_manual(values = c("MALE" = "dodgerblue", "FEMALE" = "red")) +
     theme_running() +
     theme(plot.title = element_text(hjust = 0., margin = margin(b = 4)),  
       plot.subtitle = element_markdown(hjust = 0.0,size = 10, margin = margin(t = 2, b = 2)),
            legend.position = "none",
       panel.grid.major = element_line(color = "gray95"),
       panel.grid.minor = element_line(color = "gray98"),
           plot.caption.position = "plot",  
           plot.caption = element_text(hjust = 0.))

# Fig 4a

Figure_4a_WP_1987_2023 <- ggplot(df_bar_WP_1987_put, aes(y = type_row, x = value,
          fill = gender, color = gender)) +
     geom_boxplot() +
     geom_vline(xintercept = 0, linetype = "solid", color = "red") +
     labs(
          title = "Fig. 4a: Effects of daily AQI, grade, school size, and year on running times",
          subtitle = "Woodward Park, 1987 - 2023, <span style='color:red'>Female</span> and <span style='color:dodgerblue'>Male</span> runners",
          x = "z-score of effect estimate",
          y = "",
          caption = "Data source: [6; 15]") +
     scale_color_manual(values = c("M" = "dodgerblue", "F" = "red")) +
     
   #  facet_wrap(~type_effect, scales = "free_x") +
     
     theme_running() +
     
     theme(plot.title = element_text(hjust = 0.0, margin = margin(b = 4)),
           plot.subtitle = element_markdown(hjust = 0.0,size = 10, margin = margin(t = 2, b = 2)),
          axis.text.x = element_text(hjust = 1),
          strip.text = element_text(size = 8),
          axis.text.y = element_text(angle = 30, hjust = 1),
          legend.text = element_text(),
          legend.position = "none", 
          plot.caption.position = "plot",  
          plot.caption = element_text(hjust = 0.))

# Fig 4b

Figure_4b_WP_1987_2023 <- ggplot(df_bar_WP_1987_put_D1D2_Q5, aes(y = type_row, x = value, fill = gender, color = gender)) +
     geom_boxplot() +
     geom_vline(xintercept = 0, linetype = "solid", color = "red") +
     labs(
          title = "Fig. 4b: Effects of daily AQI, grade, and year on running times",
          subtitle = "top quintile, D1 and D2 schools, Woodward Park, 1987 - 2023, <span style='color:red'>Female</span> and <span style='color:dodgerblue'>Male</span> runners",
          x = "z-score of effect estimate",
          y = "",
          caption = "Data source: [6; 15]") +
     scale_color_manual(values = c("M" = "dodgerblue", "F" = "red")) +
     
  #   facet_wrap(~type_effect, scales = "free_x") +
     
     theme_running() +
     theme(plot.title = element_text(hjust = 0.0, margin = margin(b = 4)),
           plot.subtitle = element_markdown(hjust = 0.0,size = 10, margin = margin(t = 2, b = 2)),
           axis.text.x = element_text(hjust = 1),
          strip.text = element_text(size = 8),
          axis.text.y = element_text(angle = 30, hjust = 1),
          legend.text = element_text(),
          legend.position = "none",
          plot.caption.position = "plot",  
          plot.caption = element_text(hjust = 0.))

# Fig 4c

Figure_4c_WP_2000_2023 <- ggplot(df_bar_WP_2000_put, aes(y = type_row, x = value, 
          fill = gender, color = gender)) +
     geom_boxplot() +
     geom_vline(xintercept = 0, linetype = "solid", color = "red") +
     labs(
          title = "Fig. 4c: Effects of temperature, ozone, grade, school size, year on running times",
          subtitle = "Woodward Park, 2000 - 2023, <span style='color:red'>Female</span> and <span style='color:dodgerblue'>Male</span> runners",
          x = "z-score of effect estimate",
          caption = "Data source: [6;15]",
          y = "") +
     scale_color_manual(values = c("M" = "dodgerblue", "F" = "red")) +
     
  #   facet_wrap(~type_effect, scales = "free_x") +
     
     
     theme_running() +
     theme(plot.title = element_text(hjust = 0.0, margin = margin(b = 4)),
           plot.subtitle = element_markdown(hjust = 0.0,size = 10, margin = margin(t = 2, b = 2)),
           axis.text.x = element_text(hjust = 1),
          strip.text = element_text(size = 8),
          axis.text.y = element_text(angle = 30, hjust = 1),
          legend.text = element_text(),
          legend.position = "none",
          plot.caption.position = "plot",  
          plot.caption = element_text(hjust = 0.5))


# Fig 4d

Figure_4d_WP_2000_2023 <- ggplot(df_bar_WP_2000_put_D1D2_Q5, aes(y = type_row, x = value, fill = gender, color = gender)) +
     geom_boxplot() +
     geom_vline(xintercept = 0, linetype = "solid", color = "red") +
     labs(
          title = "Fig. 4d: Effects of temperature, ozone, grade and year on running times",
          subtitle = "D1 and D2 schools, top quintile, Woodward Park, 2000 - 2023, <span style='color:red'>Female</span> and <span style='color:dodgerblue'>Male</span> runners",
          x = "z-score of effect estimate",
          caption = "Data source: [6, 15]",
          y = "") +
     scale_color_manual(values = c("M" = "dodgerblue", "F" = "red")) +
     
  #   facet_wrap(~type_effect, scales = "free_x") +
     
     theme_running() +
     theme(plot.title = element_text(hjust = 0.0, margin = margin(b = 4)),      
           plot.subtitle = element_markdown(hjust = 0.0,size = 10, margin = margin(t = 2, b = 2)),
           axis.text.x = element_text(hjust = 1),
          strip.text = element_text(size = 8),
          axis.text.y = element_text(angle = 30, hjust = 1),
          legend.text = element_text(),
          legend.position = "none",
          plot.caption.position = "plot",  
          plot.caption = element_text(hjust = 0.))

# finish times 
# Fig 5

# regression plots
# Fig 6a
# Fig 6b

df_F_MTSAC <- df_F_MTSAC %>%
     group_by(year,division,grade) %>%
     mutate(YDG_N = n(),
               YDG_mean_finish_time = round(mean(finish_time_seconds, na.rm = T),1),
               YDG_sd_finish_time = round(sd(finish_time_seconds,na.rm = T),2)) %>% 
     ungroup()

df_F_MTSAC <- df_F_MTSAC %>%
     group_by(year,division,grade) %>%
     mutate(YDG_z_score = (finish_time_seconds - YDG_mean_finish_time) / YDG_sd_finish_time) %>%
     ungroup()
     
df_M_MTSAC <- df_M_MTSAC %>%
     group_by(year,division,grade) %>%
     mutate(YDG_N = n(),
            YDG_mean_finish_time = round(mean(finish_time_seconds, na.rm = T),1),
            YDG_sd_finish_time = round(sd(finish_time_seconds,na.rm = T),2)) %>% 
     ungroup()

df_M_MTSAC <- df_M_MTSAC %>%
     group_by(year,division,grade) %>%
     mutate(YDG_z_score = (finish_time_seconds - YDG_mean_finish_time) / YDG_sd_finish_time) %>%
    ungroup()

df_pooled_MTSAC <- rbind(df_F_MTSAC,df_M_MTSAC)

# Aggregate before plotting (add this BEFORE the ggplot call)
sry_pooled_MTSAC <- df_pooled_MTSAC %>%
  group_by(year, gender, division, grade) %>%
  summarise(
    YDG_mean_finish_time = mean(finish_time_seconds, na.rm = TRUE),
    n = n(),
    .groups = "drop") %>%
  mutate(
    division = factor(division, levels = c("D1_D2","D3","D4_D5")),
    grade = factor(grade, levels = c("9","10","11","12")))

sry_pooled_MTSAC <- sry_pooled_MTSAC %>%
  mutate(facet_label = case_when(
    division == "D1_D2" & grade == 9 ~ "Divisions 1 & 2, Grade 9",
    division == "D1_D2" & grade == 10 ~ "Divisions 1 & 2, Grade 10",
    division == "D1_D2" & grade == 11 ~ "Divisions 1 & 2, Grade 11",
    division == "D1_D2" & grade == 12 ~ "Divisions 1 & 2, Grade 12",
    division == "D3" & grade == 9 ~ "Division 3, Grade 9",
    division == "D3" & grade == 10 ~ "Division 3, Grade 10",
    division == "D3" & grade == 11 ~ "Division 3, Grade 11",
    division == "D3" & grade == 12 ~ "Division 3, Grade 12",
    division == "D4_D5" & grade == 9 ~ "Divisions 4 & 5, Grade 9",
    division == "D4_D5" & grade == 10 ~ "Divisions 4 & 5, Grade 10",
    division == "D4_D5" & grade == 11 ~ "Divisions 4 & 5, Grade 11",
    division == "D4_D5" & grade == 12 ~ "Divisions 4 & 5, Grade 12"
  ))


sry_pooled_MTSAC <- arrange(sry_pooled_MTSAC, year, gender, division, grade)

Figure_5_pooled_MTSAC <- ggplot(sry_pooled_MTSAC,
     aes(x = year, y = YDG_mean_finish_time, 
     group = gender, color = gender)) +
  geom_point(size = 0.8, alpha = 0.4) + 
          geom_smooth(method = "loess") +
  geom_hline(yintercept = 1200,
             linetype = "dotted",
             color = "gray50",
             alpha = 0.4) +
          facet_grid(rows = vars(division), cols = vars(grade),
                     labeller = labeller(
                       division = c("D1_D2" = "Divisions 1 & 2",
                                    "D3" = "Division 3",
                                    "D4_D5" = "Divisions 4 & 5"),
                       grade = c("9" = "Grade 9",
                                 "10" = "Grade 10",
                                 "11" = "Grade 11",
                                 "12" = "Grade 12"))) +

  
          labs(
               title = "Fig. 5: Mean annual finish times at Mt SAC, 2002-2023",
               subtitle = "by division and grade for <span style='color:dodgerblue'>male</span> and <span style='color:red'>female</span> runners",
               x = "",
               y = "Finish time in seconds",
               caption = "Data source: [6, 8]",
               color = "") +
          scale_x_continuous(breaks = seq(2002, 2023, by = 5)) +
          scale_color_manual(values = c("MALE" = "dodgerblue", "FEMALE" = "red")) +
          theme_running() +
          theme(plot.title = element_text(hjust = 0., margin = margin(b = 4)),            
                plot.subtitle = element_markdown(hjust = 0.,size = 10, margin = margin(t = 2, b = 2)),
                legend.position = "none",
                plot.caption.position = "plot",  
                panel.grid.major = element_line(color = "gray95"),
                panel.grid.minor = element_line(color = "gray98"),
                plot.caption = element_text(hjust = 0.))



Figure_6a_MTSAC <- ggplot(df_bar_MTSAC_put, aes(y = type_row, x = value, fill = gender, color = gender)) +
     geom_boxplot() +
     geom_vline(xintercept = 0, linetype = "solid", color = "red") +
     labs(
          title = "Fig. 6a: Effects of temperature and ozone on running performance at Mt SAC",
          subtitle = "Pooled divisions, 2002 - 2023, <span style='color:red'>Female</span> and <span style='color:dodgerblue'>Male</span> runners",
          x = "z-score of effect estimate",
          caption = "Data source: [6, 8]",
          y = "") +
     scale_color_manual(values = c("M" = "dodgerblue", "F" = "red")) +
     
 #    facet_wrap(~type_effect, scales = "free_x") +
     
     theme_running() +
     theme(plot.title = element_text(hjust = 0.0, margin = margin(b = 4)),            
           plot.subtitle = element_markdown(hjust = 0, size = 10, margin = margin(t = 2, b = 2)),
           axis.text.x = element_text(hjust = 1),
          strip.text = element_text(size = 8),
          axis.text.y = element_text(angle = 30, hjust = 1),
          legend.text = element_text(),
          legend.position = "none",
          plot.caption.position = "plot",  
          plot.caption = element_text(hjust = 0.))

Figure_6b_MTSAC <- ggplot(df_bar_MTSAC_put_D1D2_Q5, aes(y = type_row, x = value, fill = gender, color = gender)) +
     geom_boxplot() +
     geom_vline(xintercept = 0, linetype = "solid", color = "red") +
     labs(
          title = "Fig. 6b: Effects of temperature and ozone on running performance at Mt SAC",
          subtitle = "Divisions 1 and 2, top quintile, Mt SAC, 2002 - 2023, <span style='color:dodgerblue'>M</span> and <span style='color:red'>F</span> runners",
          x = "z-score of effect estimate",
          caption = "Data source: [6, 8]",
          y = "") +
     scale_color_manual(values = c("M" = "dodgerblue", "F" = "red")) +
     
#     facet_wrap(~type_effect, scales = "free_x") +
     
     theme_running() +

     theme(plot.title = element_text(hjust = 0, margin = margin(b = 4)),            
           plot.subtitle = element_markdown(hjust = 0, size = 10, margin = margin(t = 2, b = 2)),
           axis.text.x = element_text(hjust = 1),
          strip.text = element_text(size = 8),
          axis.text.y = element_text(angle = 30, hjust = 1),
          legend.text = element_text(),
          legend.position = "none",
          plot.caption.position = "plot",  
          plot.caption = element_text(hjust = 0.))



class(df_F_WP_1987_2023)
class(df_M_WP_1987_2023)

class(df_F_MTSAC)
class(df_M_MTSAC)

# ========== ESTIMATE DIRECT AND INDIRECT EFFECTS OF TEMPERATURE ========

dfs_basic_temper_effect <- list(
  df_F_MTSAC = df_F_MTSAC,
  
  df_F_WP_2000_2023 = df_F_WP_2000_2023,
  
  df_M_MTSAC = df_M_MTSAC,
  
  df_M_WP_2000_2023 = df_M_WP_2000_2023)

names(dfs_basic_temper_effect)

func_temper_effect <- function(df,  df_name, cluster_var = race_name) {
  
ozone_fit <- feols(
    ozone_racetime ~ year_count + 
      temper_racetime + 
      PM25_daily_on_race_day + 
      grade + 
      division,
    cluster = ~ race_name + runner_id,
    data = df)

# temperature effect on O3  
temper_dir_O3 <- coef(ozone_fit)[["temper_racetime"]]
  
test_temper_robust <- feols(
    finish_time_seconds ~ year_count +
      ozone_racetime +
      temper_racetime +
      PM25_daily_on_race_day +
      grade + 
      division,
    cluster = ~ race_name + runner_id,
    data     = df)
  
#ozone effect on finish time
  ozone_dir_time  <- coef(test_temper_robust)[["ozone_racetime"]]
  
# direct temperature effect on finish time
temper_dir_time <- coef(test_temper_robust)[["temper_racetime"]]

# indirect temperature effect on finish time via ozone effect on finish time   
  temper_indir_via_O3 <- temper_dir_O3 * ozone_dir_time

# total temperature effect on finish time = sum of direct + indirect effects
  total_temp_effect    <- temper_dir_time + temper_indir_via_O3

# direct PM25 effect on race time
PM25_direct_time <- coef(test_temper_robust)[["PM25_daily_on_race_day"]]
  
# correlation between ozone and temper 
cor_O3_temper <- cor(df$ozone_racetime,df$temper_racetime,use = "complete.obs")

# correlation between ozone and PM25
cor_PM25_temper <- cor(df$PM25_daily_on_race_day,
                       df$temper_racetime,use = "complete.obs")

temper_tbl <- data.frame(
    dataset   = df_name,
    temper_dir_O3,
    ozone_dir_time,
    temper_dir_time,
    temper_indir_via_O3,
    total_temp_effect,
    PM25_direct_time,
    cor_PM25_temper,
    cor_O3_temper)}

sry_temper_effect <- lapply(names(dfs_basic_temper_effect), function(nm) {
  func_temper_effect(dfs_basic_temper_effect[[nm]], df_name = nm, cluster_var = race_name)})

# Bind all fit tables and all heteroskedasticity tables

temper_effect_all <- as_tibble(do.call(rbind, sry_temper_effect))

temper_effect_all <- temper_effect_all %>% pivot_longer(
  temper_dir_O3:cor_O3_temper,
  names_to = "vars",
  values_to = "value")

temper_effect_all <- temper_effect_all %>%
  mutate(
    gender = factor(case_when(
      grepl("^df_F_", dataset) ~ "Female",
      grepl("^df_M_", dataset) ~ "Male",
      TRUE ~ NA_character_)))

temper_effect_all$dataset <- str_replace_all(temper_effect_all$dataset,c("df_" = ""))

temper_effect_all <- temper_effect_all %>%   
  mutate(
    # explicit ordering of datasets; adjust if you prefer another order
    dataset = factor(
      dataset,
      levels = c(
        "F_MTSAC",
        "M_MTSAC",
        
        "F_WP_2000_2023",
        "M_WP_2000_2023")))

facet_labels <- c("F_MTSAC" = "Female: Mt SAC",
                  "M_MTSAC" = "Male: Mt SAC",
                  "F_WP_2000_2023" = "Female: Woodward Park",
                  "M_WP_2000_2023" = "Male: Woodward Park")

vars_to_plot <- c(
  "temper_dir_O3",
  "ozone_dir_time",
  "temper_dir_time",
  "temper_indir_via_O3",
  "total_temp_effect")

Figure_X_test_plot_temper_effect <- temper_effect_all %>%
  filter(vars %in% vars_to_plot) %>%
  
  ggplot(aes(x = vars, y = value, fill = vars)) +
  
  geom_col() +
  
  coord_flip() +

  facet_wrap(~ dataset, ncol = 2, labeller = labeller(dataset = facet_labels,
        .multi_line = FALSE)) +

  labs(
    title = "Fig. X: Direct and indirect effects of temperature",
    subtitle = "on finish times at Mt SAC (2002-2023) and Woodward Park (2000-2023)",
    x = NULL,
    y = "Effect size on finish time in seconds",
    fill = "Effect") +
  
  scale_x_discrete(labels = 
                     c("total_temp_effect" = "Total temperature\non time",
                       "temper_indir_via_O3" = "Indirect temperature\non time",
                       "temper_dir_time" = "Direct temperature\non time",
                       "temper_dir_O3" = "Temperature\non O3",
                       "ozone_dir_time" = "Direct ozone\non time")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(hjust = 1),
        legend.position = "none",
        strip.background = element_blank())
        
Figure_X_test_plot_temper_effect


# get the various coefficients for the direct and indirect temperature regressions

# derive summary values for all sample and D1D2_Q5
# 

func_summary_temper_cfs <- function(dfx) {
  
dfx <- dfx %>%
    
    reframe(
      mean_FTS = mean(finish_time_seconds,na.rm = T),
      median_FTS = median(finish_time_seconds, na.rm = T),
      mean_O3 = mean(ozone_racetime,na.rm = T),
      median_O3 = median(ozone_racetime, na.rm = T),
      mean_temper = mean(temper_racetime,na.rm = T),
      median_temper = median(temper_racetime, na.rm = T),
      mean_PM25 = mean(PM25_daily_on_race_day,na.rm = T))
  
  return(dfx)
  
}

# summarize data on FTS, O3,temper, PM25 in the 2002-2023 datasets (n = 4)
sry_F_WP_temper_effect <- func_summary_temper_cfs(df_F_WP_2000_2023)

sry_M_WP_temper_effect <- func_summary_temper_cfs(df_M_WP_2000_2023)

sry_F_MTSAC_temper_effect <- func_summary_temper_cfs(df_F_MTSAC)

sry_M_MTSAC_temper_effect <- func_summary_temper_cfs(df_M_MTSAC)

temper_effect_all <- as_tibble(do.call(rbind, sry_temper_effect))

func_reg_temper <- function(dfx) {

  test_temper_robust <- feols(
    finish_time_seconds ~ year_count +
      ozone_racetime +
      temper_racetime +
      PM25_daily_on_race_day +
      grade + 
      division,
    cluster = ~ race_name + runner_id,
    data     = dfx)}
  
# reg_F_WP_temper_effect = reg_F_WP_2000_2023
reg_F_WP_temper_effect <- func_reg_temper(df_F_WP_2000_2023)

# reg_M_WP_temper_effect = reg_M_WP_2000_2023
reg_M_WP_temper_effect <- func_reg_temper(df_M_WP_2000_2023)

# reg_F_MTSAC_temper_effect != reg_F_MTSAC because latter includes 'program_score' variable
reg_F_MTSAC_temper_effect <- func_reg_temper(df_F_MTSAC)

# reg_M_MTSAC_temper_effect != reg_M_MTSAC because latter includes 'program_score' variable
reg_M_MTSAC_temper_effect <- func_reg_temper(df_M_MTSAC)

# Get coefficients and their standard errors

coef_F_WP_temper_effect <- coef(reg_F_WP_temper_effect)
se_F_WP_temper_effect <- sqrt(diag(vcov(reg_F_WP_temper_effect)))

coef_M_WP_temper_effect <- coef(reg_M_WP_temper_effect)
se_M_WP_temper_effect <- sqrt(diag(vcov(reg_M_WP_temper_effect)))

coef_F_MTSAC_temper_effect <- coef(reg_F_MTSAC_temper_effect)
se_F_MTSAC_temper_effect <- sqrt(diag(vcov(reg_F_MTSAC_temper_effect)))

coef_M_MTSAC_temper_effect <- coef(reg_M_MTSAC_temper_effect)
se_M_MTSAC_temper_effect <- sqrt(diag(vcov(reg_M_MTSAC_temper_effect)))

# Calculate z-statistics for difference between coefficients

z_stats_FM_WP_temper_effect <- (coef_M_WP_temper_effect - coef_F_WP_temper_effect) / 
  sqrt(se_M_WP_temper_effect^2 + se_F_WP_temper_effect^2)

z_stats_FM_MTSAC_temper_effect <- (coef_M_MTSAC_temper_effect - coef_F_MTSAC_temper_effect) / 
  sqrt(se_M_MTSAC_temper_effect^2 + se_F_MTSAC_temper_effect^2)

# Calculate p-values

p_values_FM_WP_temper_effect <- 2 * (1 - pnorm(abs(z_stats_FM_WP_temper_effect)))
p_values_FM_MTSAC_temper_effect <- 2 * (1 - pnorm(abs(z_stats_FM_MTSAC_temper_effect)))

# Calculate difference and its standard error

diff_FM_WP_temper_effect <- coef_F_WP_temper_effect - coef_M_WP_temper_effect
se_diff_WP_temper_effect <- sqrt(diag(vcov(reg_M_WP_temper_effect)) + 
                               diag(vcov(reg_F_WP_temper_effect)))


# check the mt sac data by year, gender, grade, and division

saveRDS(df_pooled_MTSAC,"POOLED_MTSAC.RDS")

# Read RDS file
df_pooled_mtsac_rds <- readRDS("POOLED_MTSAC.RDS")

# summarize 
summary_mtsac_YGGD <- df_pooled_mtsac_rds %>%
  filter(gender %in% c("MALE", "FEMALE")) %>%
  group_by(year, gender, grade, division, weekday) %>%
  summarise(
    mean_fts = mean(finish_time_seconds, na.rm = TRUE),
    valid_n_fts = n(),
    .groups = "drop"
  ) %>%
  arrange(year, gender, grade, division)

# Get 2017 values
summary_mtsac_YGGD_2017 <- summary_mtsac_YGGD %>%
  filter(year == 2017) %>%
  select(gender, grade, division, mean_fts_2017 = mean_fts)

# Join and compute ratio to 2017
summary_mtsac_YGGD <- summary_mtsac_YGGD %>%
  left_join(summary_mtsac_YGGD_2017,
            by = c("gender", "grade", "division")) %>%
  mutate(ratio_to_2017 = mean_fts / mean_fts_2017)


plot_mtsac_YGGD <- ggplot(
  summary_mtsac_YGGD,
  aes(x = factor(year), y = mean_fts, fill = factor(grade))) +
  geom_col(position = "dodge") +
  facet_grid(gender ~ division) +
  labs(
    x = "Year",
    y = "Mean finish time (s)",
    fill = "Grade",
    title = "Mean finish time by year, grade, and division",
    subtitle = "Panels by gender; bars within each division show grades") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
# Get list of divisions
    divisions <- unique(summary_mtsac_YGGD$division)
    
    for (div in divisions) {
plot_division <- summary_mtsac_YGGD %>%
        filter(division == div) %>%
        ggplot(aes(x = factor(year), y = mean_fts, fill = factor(grade))) +
        geom_col(position = "dodge") +
        facet_wrap(~ gender, nrow = 1) +
        labs(
          x = "Year",
          y = "Mean finish time (s)",
          fill = "Grade",
          title = paste("Mean finish time by year and grade -", div),
          subtitle = "Separate panels by gender"
        ) +
        theme_bw() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1))
      
   #   print(plot_division)

}

lst_fig <- ls(pattern = "Figure_\\d{1}")
    
names(lst_fig) <- c(
      "Fig. 1a", "Fig. 1b",
      "Fig. 2a", "Fig. 2b", "Fig. 2c", "Fig. 2d", "Fig. 2e", "Fig. 2f",
      "Fig. 3",  
      "Fig. 4a", "Fig. 4b", "Fig. 4c", "Fig. 4d",
      "Fig. 5",
      "Fig. 6a", "Fig. 6b")
    

# make list of figures
