# england attendance figures...epl and efl leagues
library(tidyverse)
library(tidylog)
library(janitor)
library(glue)
library(ggtext)
library(ggrepel)
library(patchwork)

source("~/Data/r/basic functions.R")
options(scipen=10000)

# functions for summary df and plots
source("attend_functions.R")

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
eng_stad_epl_23 <-
	eng_stad_df %>%
	filter(stadium %in%  c("Anfield", "Brentford Community Stadium", "Craven Cottage", "Elland Road",
												 "Emirates Stadium", "Etihad Stadium","Goodison Park", "King Power Stadium", "London Stadium",
												 "Molineux", "Old Trafford", "Selhurst Park", "St James' Park", "St Mary's Stadium",
												 "Stamford Bridge", "The American Express Community Stadium", "City Ground",
												 "Tottenham Hotspur Stadium", "Villa Park", "Vitality Stadium")) %>%
	filter(league %notin% c("Premiership Rugby", "Women's Super League", "Women's Championship",
													"Super League")) %>%
	select(-league)

glimpse(eng_stad_epl_23)


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
	left_join(eng_stad_epl_23, by = c("match_stadium" = "stadium")) %>%
	mutate(capacity = ifelse(match_stadium == "Anfield", 54000, capacity)) %>%
	mutate(match_pct_cap = match_attendance / capacity)

glimpse(epl_att_23)

saveRDS(epl_att_23, file = "~/Data/r/football data projects/data/att_2023_epl.rds")

epl_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_epl.rds")


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


# create plotting df. 1st argument is df to run function on, 2nd is for name of df with _sum suffix
attend_sum(epl_att_23, "epl_att_23")
glimpse(epl_att_23_sum)

# plot using plotting df
# run function
epl_attplot <- attend_plot_comb(epl_att_23_sum)
epl_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
epl_attplot +
  plot_annotation(title = "<b>English Premier League
  <span style='color: #8DA0CB;'>Average percent of capacity for season</span></b><i> (left bar chart)</i>,
  <b><span style='color: #FF7F00;'>Average attendance</span></b> and
  <b><span style='color: #1F78B4;'>Stadium capacity</span></b> (right bubble chart), by club, 2022-23 season.<br>
		All Premier League clubs were over 90% capacity, and all but two, Bournemouth and Southhampton,
    filled more than 95% of seats.<br>
    See scatterplot below for stadium capacity numbers obscured by overlapping bubbles.",
                  theme = theme(plot.title =
                                  ggtext::element_textbox_simple(
                                    size = 12, fill = "cornsilk",
                                    lineheight = 1.5,
                                    padding = margin(5.5, 5.5, 5.5, 2),
                                    margin = margin(0, 0, 5.5, 0))))

ggsave("images/plot_attendance_23_epl.jpg", width = 14, height = 8,
			 units = "in", dpi = 300)

epl_scatter <- attend_scatter(epl_att_23_sum)
epl_scatter

ggsave("images/plot_att_scatter_23_epl.jpg", width = 15, height = 8,
       units = "in", dpi = 300)






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

