#
# THIS CODE CHECKS THAT NAMES MATCH BETWEEN WP AND MTSAC
# it was used to check the xlsx files and is ignored in the knit
# the code then regresses the WP data on the MTSAC data for matching runners

# ===== FIRST RECHECK ALL THE MATCHING NAMES ==== 

df_F_WP_matchin <- readRDS("RUN_F_WP_V2.RDS")

df_F_WP_matchin <- func_filters_run_WP(df_F_WP_matchin)
df_M_WP_matchin <- readRDS("RUN_M_WP_V2.RDS")
df_M_WP_matchin <- func_filters_run_WP(df_M_WP_matchin)

df_F_MTSAC_matchin <- readRDS("RUN_F_MTSAC_V2.RDS")
df_F_MTSAC_matchin <- func_filters_run_MTSAC(df_F_MTSAC_matchin)
df_M_MTSAC_matchin <- readRDS("RUN_M_MTSAC_V2.RDS")
df_M_MTSAC_matchin <- func_filters_run_MTSAC(df_M_MTSAC_matchin)

list_df_match <- mget(ls(pattern = "matchin"))

func_list_match <- function(df) {
  df <- df %>%
  mutate(runner_name_12 = substring(runner_name,1,12)) %>%
  select(year,county,section,school,grade,finish_time_seconds,runner_name,runner_name_12) %>%
  arrange(year,county,section,school,grade,runner_name,runner_name_12)
  return(df)
}

df_F_WP_matchin <- func_list_match(df_F_WP_matchin)
df_F_WP_matchin <- df_F_WP_matchin %>% filter(year >= 2002)
df_F_MTSAC_matchin <- func_list_match(df_F_MTSAC_matchin)

df_M_WP_matchin <- func_list_match(df_M_WP_matchin)
df_M_WP_matchin <- df_M_WP_matchin %>% filter(year >= 2002)
df_M_MTSAC_matchin <- func_list_match(df_M_MTSAC_matchin)

df_F_WP_MTSAC_join <- left_join(df_F_WP_matchin,df_F_MTSAC_matchin,
  by = c("year","section","school","grade","runner_name_12"),
  suffix = c("_WP","_MTSAC"),keep = TRUE)

df_F_WP_MTSAC_join <- df_F_WP_MTSAC_join %>%
        mutate(partial_name_match = 
      if_else(runner_name_12_WP == runner_name_12_MTSAC,"PART MATCH","NOT PART MATCH"),
        full_name_match = if_else(runner_name_WP == runner_name_MTSAC, "FULL MATCH","NOT FULL MATCH"))

check_F_WP_MTSAC_join_NOT <- df_F_WP_MTSAC_join %>% 
      filter(str_detect(partial_name_match, "NOT"))

df_M_WP_MTSAC_join <- left_join(df_M_WP_matchin,df_M_MTSAC_matchin,
  by = c("year","section","school","grade","runner_name_12"),
  suffix = c("_WP","_MTSAC"),keep = TRUE)

df_M_WP_MTSAC_join <- df_M_WP_MTSAC_join %>%
  mutate(partial_name_match = 
           if_else(runner_name_12_WP == runner_name_12_MTSAC,"PART MATCH","NOT PART MATCH"),
         full_name_match = if_else(runner_name_WP == runner_name_MTSAC, "FULL MATCH","NOT FULL MATCH"))

check_M_WP_MTSAC_join_NOT <- df_M_WP_MTSAC_join %>% 
  filter(str_detect(partial_name_match, "NOT"))

if (nrow(check_F_WP_MTSAC_join_NOT) == 0) {
  rm(check_F_WP_MTSAC_join_NOT)
#  print("Data frame 'check_F_WP_MTSAC_join_NOT' verified ZERO mismatched runner names.")
} else {
  print(paste("Data frame has", nrow(check_F_WP_MATCH_join_NOT),
              "rows and was not removed."))
}

if (nrow(check_M_WP_MTSAC_join_NOT) == 0) {
  rm(check_M_WP_MTSAC_join_NOT)
#  print("Data frame 'check_M_WP_MTSAC_join_NOT' verified ZERO mismatched runner names.")
} else {
  print(paste("Data frame has", nrow(check_M_WP_MATCH_join_NOT),
              "rows and was not removed."))
}





