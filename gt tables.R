# tables with attendance summary figures for all leagues
library(tidyverse)
library(tidylog)
library(janitor)
library(glue)
library(ggtext)
library(ggrepel)
library(gt)

source("~/Data/r/basic functions.R")
options(scipen=10000)

# functions for summary dfs and plots
source("attend_functions.R")

# run combine rds function
attdfs <- combine_rds_files("~/Data/r/football data projects/data")

glimpse(attdfs)

attdfs %>%
  count(league)

att23_all <-
  attdfs %>%
	mutate(league = ifelse(league == "Fußball-Bundesliga", "Bundesliga", league)) %>%
	group_by(league) %>%
	summarise(attend_avg_league = mean(match_attendance),
						attend_min_league = min(match_attendance),
						attend_max_league = max(match_attendance),
						attend_sum_league = sum(match_attendance),
						capacity_avg_league = mean(capacity),
						capacity_min_league = min(capacity),
						capacity_max_league = max(capacity),
						capacity_sum_league = sum(capacity),
						capacity_pct_league = attend_sum_league / capacity_sum_league) %>%
	select(league, attend_avg_league, attend_min_league, attend_max_league,
				 capacity_avg_league, capacity_min_league, capacity_max_league,
				 capacity_pct_league)

glimpse(att23_all)

saveRDS(att23_all, file = "~/Data/r/football data projects/data/att23_all.rds")

## gt table
attendall_gt <-
att23_all %>%
  arrange(desc(capacity_pct_league)) %>%
  select(league, capacity_avg_league, attend_avg_league, capacity_pct_league) %>%
  gt() %>%
	fmt_number(columns = c(attend_avg_league, capacity_avg_league), decimals = 0) %>%
  fmt_percent(columns = c(capacity_pct_league), decimals = 1) %>%
	cols_label(attend_avg_league = "League Average Attendance", capacity_avg_league = "League Average Stadium Capacity",
	           capacity_pct_league = "League Average Match Capacity", league = "League") %>%
	cols_align(align = "right", columns = everything()) %>%
  cols_align(align = "left", columns = c(league)) %>%
  opt_stylize(style = 5) %>%
	tab_style(
		style = cell_text(align = "center"),
		locations = cells_column_labels(
			columns = c(attend_avg_league, capacity_avg_league, capacity_pct_league))) %>%
  opt_interactive(use_sorting = TRUE)

attendall_gt |> gtsave("images/attendall_gt.png")


# correlations

leagues <- list(att23_all$league)



att23_all %>%
#  filter(league == "Superliga") %>%
  select(capacity_avg_league, capacity_pct_league, attend_avg_league) %>%
  DataExplorer::plot_correlation(maxcat = 5L, type = "continuous", geom_text_args = list("size" = 4))


# scatterplot
att23_all %>%
  ggplot(aes(x = capacity_avg_league, y = capacity_pct_league)) +
    geom_point() +
    geom_smooth() +
    geom_text_repel(aes(label = league)) +
    scale_x_continuous(labels = scales::comma_format(big.mark = ',')) +
    scale_y_continuous(limits = c(0,1), labels = scales::percent_format()) +
    labs(x = "Stadium Capacity", y = "Avg % of Capacity") +
    theme_minimal()

att23_all %>%
  ggplot(aes(x = attend_avg_league, y = capacity_pct_league)) +
  geom_point() +
  geom_smooth() +
  geom_text_repel(aes(label = league)) +
  scale_x_continuous(labels = scales::comma_format(big.mark = ',')) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent_format()) +
  labs(x = "Average Match Attendance", y = "Avg % of Capacity") +
  theme_minimal()


## long way code
# bel_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_bel.rds") %>%
#   select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
# bundes_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_bundes.rds") %>%
#   select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
# epl_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_epl.rds") %>%
#   select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
# laliga_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_laliga.rds") %>%
#   select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
# ligue1_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_ligue1.rds") %>%
#   select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
# mls_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_mls.rds") %>%
#   select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
# ned_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_ned.rds") %>%
#   select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
# por_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_por.rds") %>%
#   select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
# seriea_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_seriea.rds") %>%
#   select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
# sui_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_sui.rds") %>%
#   select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
# swe_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_swe.rds") %>%
#   select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
#
# glimpse(bel_att_23)
# glimpse(bundes_att_23)
# glimpse(epl_att_23)
# glimpse(laliga_att_23)
# glimpse(ligue1_att_23)
# glimpse(mls_att_23)
# glimpse(ned_att_23)
# glimpse(por_att_23)
# glimpse(seriea_att_23)
# glimpse(sui_att_23)
# glimpse(swe_att_23)
#
# att23_all <- bundes_att_23 %>%
#   bind_rows(list(epl_att_23, laliga_att_23, ligue1_att_23, mls_att_23, seriea_att_23))

# # create function to read in all files and rbind
# combine_rds_files <- function(directory) {
#   # List all RDS files in the specified directory
#   rds_files <- list.files(directory, pattern = "^att_2023.*\\.rds", full.names = TRUE)
#
#   # Initialize an empty list to store dataframes
#   df_list <- list()
#
#   # Loop through each RDS file, read it, select columns, and store in the list
#   for (file in rds_files) {
#     data <- readRDS(file)
#     # Select the desired columns
#     selected_data <- data %>%
#       select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
#     df_list[[file]] <- selected_data
#   }
#
#   # Combine all dataframes in the list into one dataframe
#   combined_df <- bind_rows(df_list)
#
#   return(combined_df)
# }
