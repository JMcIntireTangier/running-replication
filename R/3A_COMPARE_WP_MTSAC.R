# ===== REGRESSIONS TO COMPARE WP TIMES ON MTSAC TIMES WITHIN YEARS

# FOR RUNNERS WHO RAN BOTH RACES IN SAME YEARS
# ===== D n D models =====
# All divisions and all quintiles
# clusters = c(race_name, runner_id)
# the basic function is within runners who match on year grade division school
# d(finish_time_seconds) = f(d(ozone_race_time, d_temper_racetime,
# d(PM25_daily_on_race_day),d(linear_school_AQI_14),
# fixed effects(year, grade,division))
# 
# match F in WP and MTSAC races
# "_mj" means a df created by matching and joining a MTSAC df and a WP df

# ===== filter WOODWARD PARK to 2002-2023 =====
# get the F WP races and filter year >= 2002

df_F_WP_match <- filter(df_F_WP_2000_2023, year >= 2002)
df_F_WP_match <- df_F_WP_match %>% mutate(year_count = year - 2001)
df_F_WP_match <- df_F_WP_match %>% arrange(year,section,school,runner_name)

#
# get the F MTSAC races
df_F_MTSAC_match <- df_F_MTSAC %>% arrange(year,section,school,runner_name)

# match the sections schools, grades and runner names for F WP and F MTSAC


df_F_MTSAC_WP_mj <- left_join(df_F_MTSAC_match,df_F_WP_match,
  by = c("year","section","school","grade","runner_name"),
    suffix = c("_MTSAC","_WP"),keep = TRUE)
#
df_F_MTSAC_WP_mj <- df_F_MTSAC_WP_mj %>%
  mutate(full_name_match = if_else(trimws(runner_name_MTSAC) == trimws(runner_name_WP), "FULL MATCH","NOT FULL MATCH"))

#
#simplify the resulting joined df for F WP MTSAC
#

df_F_MTSAC_WP_mj <- df_F_MTSAC_WP_mj %>%
  filter(full_name_match == "FULL MATCH")
#

df_F_MTSAC_WP_mj <- df_F_MTSAC_WP_mj %>%
  select(year_count_MTSAC,year_MTSAC,year_WP,
         race_date_MTSAC,race_date_WP,
         course_MTSAC,course_WP,
         section_WP,section_MTSAC,
         start_time_WP,start_time_MTSAC,
         school_WP,school_MTSAC,
         grade_MTSAC,grade_WP,
         runner_name_MTSAC,runner_name_WP,
         metric_distance_MTSAC,metric_distance_WP,
         ozone_racetime_MTSAC,ozone_racetime_WP,
         temper_racetime_MTSAC,temper_racetime_WP,
         finish_time_seconds_MTSAC,finish_time_seconds_WP)
#

df_F_MTSAC_WP_mj <- df_F_MTSAC_WP_mj %>%
  mutate(d_finish_time_seconds = finish_time_seconds_MTSAC - finish_time_seconds_WP,
           d_ozone_racetime = ozone_racetime_MTSAC - ozone_racetime_WP,
         d_temper_racetime = temper_racetime_MTSAC - temper_racetime_WP)
#
#males
# get the M WP races and filter year >= 2002
#

df_M_WP_match <- filter(df_M_WP_2000_2023, year >= 2002)
df_M_WP_match <- df_M_WP_match %>% mutate(year_count = year - 2001)
df_M_WP_match <- df_M_WP_match %>% arrange(year,section,school,runner_name)
#
# get the F MTSAC races
df_M_MTSAC_match <- df_M_MTSAC %>% arrange(year,section,school,runner_name)

# match the sections schools, grades and runner names for F WP and F MTSAC

df_M_MTSAC_WP_mj <- left_join(df_M_MTSAC_match,df_M_WP_match,
                              by = c("year","section","school","grade","runner_name"),
                              suffix = c("_MTSAC","_WP"),keep = TRUE)

df_M_MTSAC_WP_mj <- df_M_MTSAC_WP_mj %>%
  mutate(full_name_match = if_else(trimws(runner_name_MTSAC) == trimws(runner_name_WP), "FULL MATCH","NOT FULL MATCH"))
#
#simplify the resulting joined df for M WP MTSAC
#
df_M_MTSAC_WP_mj <- df_M_MTSAC_WP_mj %>%
  filter(full_name_match == "FULL MATCH")
#
df_M_MTSAC_WP_mj <- df_M_MTSAC_WP_mj %>%
  select(year_count_MTSAC,year_MTSAC,year_WP,
         race_date_MTSAC,race_date_WP,
         section_MTSAC,section_WP,
         start_time_MTSAC,start_time_WP,
         school_MTSAC,school_WP,
         grade_MTSAC,grade_WP,
         runner_name_MTSAC,runner_name_WP,
          metric_distance_MTSAC,metric_distance_WP,
         ozone_racetime_MTSAC,ozone_racetime_WP,
         temper_racetime_MTSAC,temper_racetime_WP,
         finish_time_seconds_MTSAC,finish_time_seconds_WP)
#
df_M_MTSAC_WP_mj <- df_M_MTSAC_WP_mj %>%
  mutate(d_finish_time_seconds = finish_time_seconds_MTSAC - finish_time_seconds_WP,
         d_ozone_racetime = ozone_racetime_MTSAC - ozone_racetime_WP,
         d_temper_racetime = temper_racetime_MTSAC - temper_racetime_WP)
#
# create a regression function without FE with clusters = year_MTSAC
func_MTSAC_WP_DnD <- function(df){
  lm_robust(d_finish_time_seconds ~
              finish_time_seconds_MTSAC +
              year_count_MTSAC +
              grade_MTSAC +
              d_ozone_racetime +
              d_temper_racetime,
            clusters = year_MTSAC,
            se_type =  "CR2",
            data = df)}

# create a regression function with FE with clusters = runner_name_MTSAC
func_MTSAC_WP_DnD_FE <- function(df){
  lm_robust(d_finish_time_seconds ~
              finish_time_seconds_MTSAC +
              year_count_MTSAC +
              grade_MTSAC +
              d_ozone_racetime +
              d_temper_racetime,
            clusters = year_MTSAC,
            fixed_effects = ~ runner_name_MTSAC,
            se_type =  "CR2",
            data = df)}

# NB: the FE models take a L.O.N.G time so go for coffee or something

# 4 regressions: DnD by F|M; FE by F|M

# message("run started at ", Sys.time())
# 
# reg_F_MTSAC_WP_DnD <- func_MTSAC_WP_DnD(df_F_MTSAC_WP_mj)
# reg_F_MTSAC_WP_DnD_FE <- func_MTSAC_WP_DnD_FE(df_F_MTSAC_WP_mj)
# 
# reg_M_MTSAC_WP_DnD <- func_MTSAC_WP_DnD(df_M_MTSAC_WP_mj)
# reg_M_MTSAC_WP_DnD_FE <- func_MTSAC_WP_DnD_FE(df_M_MTSAC_WP_mj)
# 
# # Put the 4 regressions into a list
# LIST_REG_DND <- mget(ls(pattern = "reg_.+WP_DnD"))
# 
# #apply summary on the list
# sry_lst_reg_DnD <- lapply(LIST_REG_DND,summary)

# Save the whole list to a single file
# saveRDS(LIST_REG_DND, file = "LIST_REG_DND.rds")

# the RDS object LIST_REG_DND.rds
# was created and then  saved as RDS to
# avoid the long in running the FEs
# 

LIST_REG_DND <- readRDS("LIST_REG_DND.RDS")

# # prepare robust SEs (and other stats)
# se_list <- starprep(
#   reg_F_MTSAC_WP_DnD,
#   reg_F_MTSAC_WP_DnD_FE,
#   reg_M_MTSAC_WP_DnD,
#   reg_M_MTSAC_WP_DnD_FE,
#   stat = "std.error")  # can also use "p.value", "statistic", etc.

list_table_DnD <- list(
  `Females, DnD` = LIST_REG_DND$reg_F_MTSAC_WP_DnD,
  `Females, DnD and FE` = LIST_REG_DND$reg_F_MTSAC_WP_DnD_FE,
  `Males, DnD` = LIST_REG_DND$reg_M_MTSAC_WP_DnD,
  `Males, DnD and FE` = LIST_REG_DND$reg_M_MTSAC_WP_DnD_FE)

summary_table_8 <- modelsummary(
  list_table_DnD,
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
  title = "Table 8: Comparing Mt SAC and Woodward Park, 2002 - 2023")


# Now you can use them as usual
lapply(LIST_REG_DND, summary)

# Example: use in a table
# modelsummary(LIST_REG_DND, output = "reg_table.html")
# or stargazer(LIST_REG_DND, type = "text")

saveRDS(summary_table_8,"SUMMARY_TABLE_DND.RDS")

# message("run ended at ", Sys.time())


# alternative specification is a stacked model
# in which all F data for both WP and MTSAC are in one df
# and all M data for both WP and MTSAC are in one df df

# ===== stacked models =====

# # put female df in stack format from matching df
# 
# df_F_stacked <- df_F_MTSAC_WP_mj %>%
#   filter(!is.na(runner_name_MTSAC)) %>%
#   select(
#     runner_id = runner_name_MTSAC,  
#     year = year_count_MTSAC,
#     grade = grade_MTSAC,
#     race_date_MTSAC,race_date_WP,
#     finish_time_MTSAC = finish_time_seconds_MTSAC,
#     finish_time_WP    = finish_time_seconds_WP,
#     ozone_MTSAC = ozone_racetime_MTSAC,
#     ozone_WP    = ozone_racetime_WP,
#     temp_MTSAC  = temper_racetime_MTSAC,
#     temp_WP     = temper_racetime_WP) %>%
#     tidyr::pivot_longer(
#     cols = c(race_date_MTSAC,race_date_WP,
#              finish_time_MTSAC, finish_time_WP,
#              ozone_MTSAC, ozone_WP,
#              temp_MTSAC, temp_WP),
#     names_to  = c(".value","course"),
#     names_sep = "_(MTSAC|WP)$") %>% 
#     mutate(
#      course = factor(case_when(
#        month(race_date) == 10 ~ "MTSAC",
#         TRUE ~ "Woodward Park")))
# 
# # runner FE, clusters = year, without PM25
# mod_F_FE_without_PM25 <- lm_robust(
#   finish_time ~ year + course + grade +
#     course:ozone + course:temp,
#   data          = df_F_stacked,
#   fixed_effects = ~ runner_id,
#   clusters      = year,
#   se_type       = "CR2")
# 
# summary(mod_F_FE_without_PM25)
# 
# 
# # put male df in stack format from matching df
# 
#   df_M_stacked <- df_M_MTSAC_WP_mj %>%
#   filter(!is.na(runner_name_MTSAC)) %>%
#   select(
#     runner_id = runner_name_MTSAC,  # or a separate id if you have one
#     year = year_count_MTSAC,
#     grade = grade_MTSAC,
#     race_date_MTSAC,race_date_WP,
#     finish_time_MTSAC = finish_time_seconds_MTSAC,
#     finish_time_WP    = finish_time_seconds_WP,
#     ozone_MTSAC = ozone_racetime_MTSAC,
#     ozone_WP    = ozone_racetime_WP,
#     temp_MTSAC  = temper_racetime_MTSAC,
#     temp_WP     = temper_racetime_WP) %>%
#     tidyr::pivot_longer(
#     cols = c(race_date_MTSAC,race_date_WP,
#              finish_time_MTSAC, finish_time_WP,
#              ozone_MTSAC, ozone_WP,
#              temp_MTSAC, temp_WP),
#     names_to  = c(".value","course"),
#     names_sep = "_(MTSAC|WP)$") %>% 
#     mutate(
#     course = factor(case_when(
#       month(race_date) == 10 ~ "MTSAC",
#       TRUE ~ "Woodward Park")))
# 
# # runner FE, clusters = year, without PM25
# mod_M_FE_without_PM25 <- lm_robust(
#   finish_time ~ year + course + grade +
#   course:ozone + course:temp,
#   data          = df_M_stacked,
#   fixed_effects = ~ runner_id,
#   clusters      = year,
#   se_type       = "CR2")
# 
# summary(mod_M_FE_without_PM25)
# 
# list_table_FE_ix <- list(
#   `Females, FE-IX-PM25` = mod_F_FE_without_PM25,
#   `Males, FE-IX-noPM25` = mod_M_FE_without_PM25)
# 
# summary_table_FE_IX_PM25 <- modelsummary(
#   list_table_FE_ix,
#   output = "flextable",
#   estimate = c("{estimate}{stars}"),
#   gof_omit = ".IC|Lik",
#   coef_rename = c("Year" = "year",
#                   "Course" = "course",
#                   "Grade" = "grade",
#                   
#                   "MTSAC x ozone" = "courseMTSAC:ozone",
#                   "WP x ozone" = "courseWoodward Park:ozone",
#                   
#                   "MTSAC x temp" = "courseMTSAC:temp",
#                   "WP x temp" = "courseWoodward Park:temp"),
#   
# title = "Table: FE_IX_PM2.5, MTSAC and WP, 2002 - 2023")
# 
# message("run ended at ", Sys.time())
# 
# 
# saveRDS(summary_table_FE_IX_PM25,"SUMMARY_TABLE_FE_IX_PM25.RDS")
# read_summary_table_FE_IX_PM25 <- readRDS("SUMMARY_TABLE_FE_IX_PM25.RDS")
# read_summary_table_FE_IX_PM25
# 
# install.packages("beepr")
# library(beepr)
# 
# # ... regressions ...
# 
# beepr::beep(3)  # plays a sound when done


# check the classes in the regressions

lst_reg <- mget(ls(pattern = "^reg_(F|M)"))

(class_reg <- lapply(lst_reg,class))

lst_tst_reg <- mget(ls(pattern = "^testmdl_"))

(class_tst_reg <- lapply(lst_tst_reg,class))
