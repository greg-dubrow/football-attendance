# mls attendance figures

library(tidyverse)
library(tidylog)
library(janitor)
library(ggtext)
library(ggrepel)

source("~/Data/r/basic functions.R")

# load attendance data
mls_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "USA") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
	select(Competition_Name:Round, match_week, Wk, Day:Referee)

mls_match_2023 %>%
	count(Country)


# for total season pct capacity, sum all matches capacity (potential capacity) &
#   sum actual attendance to account for multiple venues

mls_att23 <- mls_match_2023 %>%
	mutate(league = "MLS") %>%
	select(league, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	mutate(match_stadium = ifelse(match_stadium == "Citypark", "CityPark", match_stadium)) %>%
	mutate(match_stadium = ifelse(
		match_home == "LAFC" & match_stadium == "BMO Field", "BMO Stadium", match_stadium)) %>%
	left_join(mls_stad_df, by = c("match_stadium" = "stadium")) %>%
	# adjustments for flex capacity stadiums
	mutate(capacity = ifelse(
		(match_stadium == "Bank of America Stadium" & match_attendance > 38000),
		74867, capacity)) %>%
	mutate(capacity = ifelse(
		(match_stadium == "Mercedes-Benz Stadium" & match_attendance > 44000),
		71000, capacity)) %>%
	mutate(capacity = ifelse(
		(match_stadium == "Gillette Stadium" & match_attendance > 30000),
		65878, capacity)) %>%
	mutate(capacity = ifelse(
		(match_stadium == "Soldier Field" & match_attendance > 30000),
		61500, capacity)) %>%
	mutate(capacity = ifelse(
		(match_stadium == "BC Place Stadium" & match_attendance > 23000),
		54500, capacity)) %>%
	mutate(capacity = ifelse(
		(match_stadium == "Lumen Field" & match_attendance > 38000),
		68740, capacity)) %>%
	mutate(match_pct_cap = match_attendance / capacity)

mls_att23 %>%
	filter(match_pct_cap > 1) %>%
	select(match_date, match_home, match_away, match_stadium, match_attendance,
				 capacity, match_pct_cap) %>%
	arrange(desc(match_pct_cap), match_stadium) %>%
	view()

mls_att23 %>%
	summarise(attend_avg = mean(match_attendance),
						atten_med = median(match_attendance),
						attend_min = min(match_attendance),
						attend_max = max(match_attendance),
						cap_avg = mean(capacity),
						cap_med = median(capacity))

mls_att23 %>%
	summarise(capacity_tot = sum(capacity),
						attendance_tot = sum(match_attendance),
						matches_tot = n())%>%
	mutate(capacity_pct_season = attendance_tot / capacity_tot) %>%
	mutate(att_per_match = attendance_tot / matches_tot)


mls_att23 %>%
	group_by(match_home, match_stadium) %>%
	mutate(capacity_tot = sum(capacity)) %>%
	mutate(attendance_tot = sum(match_attendance)) %>%
	mutate(capacity_pct_season = attendance_tot / capacity_tot) %>%
	mutate(attendance_avg = round(mean(match_attendance), 0)) %>%
	ungroup() %>%
	group_by(match_home) %>%
	mutate(capacity_tot2 = sum(capacity)) %>%
	mutate(attendance_tot2 = sum(match_attendance)) %>%
	mutate(capacity_pct_team = attendance_tot2 / capacity_tot2) %>%
	ungroup() %>%
	select(team_name = match_home, stadium_name = match_stadium,
				 capcity = capacity, attendance_avg,
				 capacity_tot, attendance_tot, capacity_pct_season, capacity_pct_team) %>%
	distinct(team_name, stadium_name, .keep_all = T) %>%
	view()
