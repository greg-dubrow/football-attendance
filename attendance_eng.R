# england attendance figures...epl and efl leagues

library(tidyverse)
library(tidylog)
library(janitor)
library(ggtext)
library(ggrepel)

source("~/Data/r/basic functions.R")

# load attendance data
eng_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "ENG") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
	select(Competition_Name:Round, match_week, Wk, Day:Referee)

eng_match_2023 %>%
	filter(Competition_Name == "Premier League") %>%
	summarise(min_date = min(Date),
						max_date = max(Date))

glimpse(eng_match_2023)

eng_match_2023 %>%
	filter(Competition_Name == "Premier League") %>%
	count(match_week, Wk, Date) %>%
	arrange(match_week, Date) %>%
	select(-n) %>%
	view()


## england stadium info via wikipedia (last edit May 2 2024)
# creates eng_stad_df

# source("stadium_eng.R")

eng_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_eng.rds")

eng_stad_df %>%
	count(league)

## EPL Attendance & capacity 2022-33
# staddium info for EPL teams 2022-23
eng_stad_epl23 <-
	eng_stad_df %>%
	filter(stadium %in%  c("Anfield", "Brentford Community Stadium", "Craven Cottage", "Elland Road",
												 "Emirates Stadium", "Etihad Stadium","Goodison Park", "King Power Stadium", "London Stadium",
												 "Molineux", "Old Trafford", "Selhurst Park", "St James' Park", "St Mary's Stadium",
												 "Stamford Bridge", "The American Express Community Stadium", "City Ground",
												 "Tottenham Hotspur Stadium", "Villa Park", "Vitality Stadium")) %>%
	filter(league %notin% c("Premiership Rugby", "Women's Super League", "Women's Championship",
													"Super League")) %>%
	select(-league)

glimpse(epl_match_2023)

epl_match_2023 %>%
	count(Home) %>%
	select(-n) %>%
	view()

# join match data to stadium info
epl_att_23 <- eng_match_2023 %>%
	filter(Competition_Name == "Premier League") %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	mutate(match_attendance = ifelse(
		(match_home == "Aston Villa" & match_away == "Brighton"), 42212, match_attendance)) %>%
	mutate(match_stadium = ifelse(match_stadium == "Molineux Stadium", "Molineux", match_stadium)) %>%
	mutate(match_stadium = ifelse(match_stadium == "The City Ground", "City Ground", match_stadium)) %>%
	mutate(match_stadium = ifelse(match_stadium == "St. Mary's Stadium", "St Mary's Stadium", match_stadium)) %>%
	mutate(match_home = ifelse(match_home == "Nott'ham Forest", "Nottingham Forest", match_home)) %>%
	mutate(match_away = ifelse(match_away == "Nott'ham Forest", "Nottingham Forest", match_away)) %>%
	mutate(match_home = str_replace(match_home, "Utd", "United")) %>%
	mutate(match_away = str_replace(match_away, "Utd", "United")) %>%
	left_join(eng_stad_epl23, by = c("match_stadium" = "stadium")) %>%
	mutate(capacity = ifelse(match_stadium == "Anfield", 54000, capacity)) %>%
	mutate(match_pct_cap = match_attendance / capacity)

glimpse(epl_att_23)

# top line stats for league
epl_att_23 %>%
	summarise(matches_tot = n(),
						attendance_tot = sum(match_attendance),
						attend_avg = mean(match_attendance),
						atten_med = median(match_attendance),
						attend_min = min(match_attendance),
						attend_max = max(match_attendance),
						capacity_tot = sum(capacity),
						cap_avg = mean(capacity),
						cap_med = median(capacity)) %>%
	mutate(capacity_pct_season = attendance_tot / capacity_tot)


source("attend_function.R")

attend_sum(epl_att_23, "epl_att_23")
glimpse(epl_att_23_sum)

epl_att_23_sum %>%
	ggplot(aes(stadium_capacity, reorder(team_name, stadium_capacity))) +
	geom_point(aes(x=stadium_capacity, y= reorder(team_name, stadium_capacity)), color="#4E79A7", size=3 ) +
	geom_point(aes(x=attend_avg_team, y= reorder(team_name, stadium_capacity)), color="#A74E79", size=3 ) +
	#	geom_bar(stat = "identity") +
	geom_vline(xintercept = epl_att_23_sum$capacity_avg_league, color = "#4E79A7") +
	geom_vline(xintercept = epl_att_23_sum$attend_avg_league, color = "#A74E79") +
	geom_text(data = epl_att_23_sum %>% filter(stadium_capacity < 70000),
						aes(x = stadium_capacity + 1100, y = team_name,
								label = paste0("Pct capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = -.02)) +
	geom_text(data = epl_att_23_sum %>% filter(stadium_capacity > 70000),
						aes(x = stadium_capacity - 10000, y = team_name,
								label = paste0("Pct capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = .04)) +
#	geom_text(aes(x = 30000, y = "Manchester United", label = "test")) +
	annotate(geom = "richtext",
					 label = "<span style='color: #4E79A7;'><- Average league capacity</span>",
					 x = 48000, y = "Manchester United") +
	annotate(geom = "richtext",
					 label = "<span style='color: #A74E79;'>Average league attendance -></span>",
					 x = 32000, y = "Manchester United") +
	scale_x_continuous(limits = c(10000, 75000), breaks = scales::pretty_breaks(6)) +
	ggtitle(paste0(epl_att_23_sum$league, " <span style='color: #4E79A7;'>Stadium capacity</span> and
			  <span style='color: #A74E79;'>Average attendance</span> by club, 2022-23 season.")) +
	labs(x = "", y = "",
			 subtitle = "If blue dot right of red dot, average attendance below stadium capacity. Sorted by stadium capacity.",
			 caption = "*Data from FBRef using worldfootballr package*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				plot.title = element_markdown(),
				plot.subtitle = element_markdown(),
				plot.caption = element_markdown(),
				axis.text.y = element_text(size = 11))



epl_att_23_sum %>%
	ggplot(aes(stadium_capacity, capacity_pct_team)) +
	geom_point() +
	geom_smooth() +
	geom_label_repel(aes(label = team_name))

# for df of points by calendar week
glimpse(epl_att23)

start_day_epl <- as.Date("2022-08-05")
end_day_epl <- as.Date("2023-05-28")

epl_weeks <- tibble(date = seq(start_day_epl, end_day_epl, by = "1 day")) %>%
	mutate(year = year(date),
				 month_abb = month(date, label = TRUE, abbr = TRUE),
				 day = wday(date, label = TRUE, week_start = 1),
				 #				 first_day_of_season = floor_date(date, "year"),
				 week_of_season = as.integer((date - start_day_epl + wday(start_day_epl, week_start = 1) - 1) / 7) + 1)

glimpse(epl_weeks)

epl_points_by_week_h <- epl_weeks %>%
	select(match_date = date, week_of_season) %>%
	right_join(epl_att23) %>%
	group_by(match_home, week_of_season) %>%
	mutate(points_week_h = case_when(HomeGoals > AwayGoals ~ 3,
																	 HomeGoals == AwayGoals ~ 1,
																	 TRUE ~ 0)) %>%
	ungroup() %>%
	select(team = match_home, match_date, week_of_season, points_week_h,
				 stadium = match_stadium, match_attendance,
				 capacity, match_pct_cap)


	group_by(match_away, week_of_season) %>%
	mutate(points_week_a = case_when(AwayGoals > HomeGoals ~ 3,
																	 AwayGoals == HomeGoals ~ 1,
																	 TRUE ~ 0)) %>%
	ungroup()

glimpse(epl_points_by_week)

epl_points_by_week %>%
	select(week_of_season, match_home, points_week_h, match_away, points_week_a) %>%
	view()

## efl championship

# stadium info for EFL Championship teams 2022-23
eng_stad_eflch23 <-
	eng_stad_df %>%
	filter( stadium %in% c("St Andrew's", "Ewood Park", "Bloomfield Road", "Ashton Gate Stadium",
												 "Turf Moor", "Cardiff City Stadium", "Coventry Building Society Arena",
												 "Kirklees Stadium", "MKM Stadium", "Kenilworth Road", "Riverside Stadium",
												 "The Den", "Carrow Road", "Deepdale", "Loftus Road",
												 "Madejski Stadium", "New York Stadium", "Bramall Lane", "bet365 Stadium",
												 "Stadium of Light", "Swansea.com Stadium", "Vicarage Road", "The Hawthorns", "DW Stadium")) %>%
	filter(league %notin% c("Premiership Rugby", "Women's Super League", "Women's Championship",
													"Super League")) %>%
	select(-league) %>%
	mutate(stadium = ifelse(stadium == "Kirklees Stadium", "The John Smith's Stadium", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Madejski Stadium", "Select Car Leasing Stadium", stadium))


glimpse(efl_ch_match_2023)

efl_ch_match_2023 %>%
	count(Venue, Home) %>%
	select(-n) %>%
	view()

efl_ch_att23 <- eng_match_2023 %>%
	filter(Competition_Name == "EFL Championship") %>%
	filter(!Round == "Final") %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	mutate(match_attendance = ifelse(match_attendance == 33383, 13383, match_attendance)) %>%
	mutate(match_attendance = ifelse(match_attendance == 35287, 25287, match_attendance)) %>%
	mutate(match_attendance = ifelse(match_attendance == 1659, 16608, match_attendance)) %>%
	mutate(match_stadium = ifelse(
		match_stadium == "Kenilworth Road Stadium", "Kenilworth Road", match_stadium)) %>%
	mutate(match_stadium = ifelse(
		match_stadium == "Deepdale Stadium", "Deepdale", match_stadium)) %>%
	mutate(match_stadium = ifelse(
		match_stadium == "Kiyan Prince Foundation Stadium", "Loftus Road", match_stadium)) %>%
	mutate(match_stadium = ifelse(
		match_stadium == "AESSEAL New York Stadium", "New York Stadium", match_stadium)) %>%
	mutate(match_stadium = ifelse(
		match_stadium == "Bet365 Stadium", "bet365 Stadium", match_stadium)) %>%
	mutate(match_stadium = ifelse(
		match_stadium == "Vicarage Road Stadium", "Vicarage Road", match_stadium)) %>%
	mutate(match_stadium = ifelse(
		match_stadium == "The DW Stadium" , "DW Stadium" , match_stadium)) %>%
	mutate(match_stadium = ifelse(
		match_stadium == "St Andrew's Trillion Trophy Stadium", "St Andrew's", match_stadium)) %>%
	left_join(eng_stad_eflch23, by = c("match_stadium" = "stadium")) %>%
	mutate(match_pct_cap = match_attendance / capacity)

efl_ch_att23 %>%
	summarise(matches_tot = n(),
						attendance_tot = sum(match_attendance),
						attend_avg = mean(match_attendance),
						atten_med = median(match_attendance),
						attend_min = min(match_attendance),
						attend_max = max(match_attendance),
						capacity_tot = sum(capacity),
						cap_avg = mean(capacity),
						cap_med = median(capacity)) %>%
	mutate(capacity_pct_season = attendance_tot / capacity_tot)

# different groupings in case teams play at multiple venues.
# only keep capacity_pct_team if multiple venues
efl_ch_att23 %>%
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
				 capacity_tot, attendance_tot, capacity_pct_season) %>%
				 #, capacity_pct_team) %>%
	distinct(team_name, stadium_name, .keep_all = T) %>%
	view()




## redundant code
epl_att_23_sum <- epl_att23 %>%
	# team figures
	group_by(match_home, match_stadium) %>%
	mutate(attend_avg_team = round(mean(match_attendance), 0)) %>%
	mutate(attend_min_team = min(match_attendance)) %>%
	mutate(attend_max_team = max(match_attendance)) %>%
	mutate(attend_tot_team = sum(match_attendance)) %>%
	mutate(capacity_tot_team = sum(capacity)) %>%
	mutate(capacity_pct_team = attend_tot_team / capacity_tot_team) %>%
	ungroup() %>%
	# league figures
	mutate(attend_avg_league = round(mean(match_attendance), 0)) %>%
	mutate(attend_med_league = round(median(match_attendance), 0)) %>%
	mutate(attend_min_league = min(match_attendance)) %>%
	mutate(attend_max_league = max(match_attendance)) %>%
	mutate(capacity_avg_league = round(mean(capacity), 0)) %>%
	mutate(capacity_med_league = round(median(capacity), 0)) %>%
	mutate(capacity_min_league = min(capacity)) %>%
	mutate(capacity_max_league = max(capacity)) %>%
	mutate(attend_tot_league = sum(match_attendance)) %>%
	mutate(capacity_tot_league = sum(capacity)) %>%
	mutate(capacity_pct_league = attend_tot_league / capacity_tot_league) %>%
	# group_by(match_home) %>%
	# mutate(capacity_tot2 = sum(capacity)) %>%
	# mutate(attendance_tot2 = sum(match_attendance)) %>%
	# mutate(capacity_pct_team = attendance_tot2 / capacity_tot2) %>%
	# ungroup() %>%
	select(team_name = match_home, stadium_name = match_stadium, stadium_capacity = capacity,
				 attend_avg_team, attend_min_team, attend_max_team,
				 attend_tot_team, capacity_tot_team, capacity_pct_team,
				 attend_avg_league, attend_med_league, attend_min_league, attend_max_league,
				 capacity_avg_league, capacity_med_league, capacity_min_league, capacity_max_league,
				 attend_tot_league, capacity_tot_league, capacity_pct_league) %>%
	#, capacity_pct_team) %>%
	distinct(team_name, stadium_name, .keep_all = T)
