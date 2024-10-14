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

# functions for summary df and plots
source("attend_functions.R")

bundes_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_bundes.rds") %>%
	select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
epl_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_epl.rds") %>%
	select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
laliga_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_laliga.rds") %>%
	select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
ligue1_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_ligue1.rds") %>%
	select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
mls_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_mls.rds") %>%
	select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
seriea_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_seriea.rds") %>%
	select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)

glimpse(bundes_att_23)

att23_all <- bundes_att_23 %>%
	bind_rows(list(epl_att_23, laliga_att_23, ligue1_att_23, mls_att_23, seriea_att_23))

att23_all %>%
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
				 capacity_pct_league) %>%
	gt() %>%
	fmt_number(columns = c(attend_avg_league, capacity_avg_league), decimals = 0) %>%
	cols_label(attend_avg_league = "League Average Attendance", attend_min_league = "Lowest Match Attendance")
,
						 elev_total = md("Total Elevation *(meters)*"),
						 time_total1 = md("Total Time *(hours/min/sec)*"),
						 time_total2 = md("Total Time *(days/hours/min/sec)*"),
						 cal_total = "Total Calories", kiloj_total = "Total Kilojoules") %>%
	cols_align(align = "center", columns = everything()) %>%
	tab_style(style = cell_fill(color = "grey"),
						locations = cells_body(rows = seq(1, 1, 1))) %>%
	tab_style(
		style = cell_text(align = "center"),
		locations = cells_column_labels(
			columns = c(rides, km_total, elev_total, time_total1, time_total2, cal_total, kiloj_total)))

