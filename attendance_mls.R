# mls attendance figures
library(tidyverse)
library(tidylog)
library(janitor)
library(glue)
library(ggtext)
library(ggrepel)

source("~/Data/r/basic functions.R")
options(scipen=10000)

# functions for summary df and plots
source("attend_functions.R")

# load attendance data
mls_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "USA") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
	select(Competition_Name:Round, match_week, Wk, Day:Referee) %>%
	mutate(Home = str_replace(Home, "Utd", "United")) %>%
	mutate(Away = str_replace(Away, "Utd", "United")) %>%
	mutate(Home = ifelse(Home == "Atlanta United", "Atlanta United FC", Home)) %>%
	mutate(Away = ifelse(Away == "Atlanta United", "Atlanta United FC", Away)) %>%
	mutate(Home = ifelse(Home == "Austin", "Austin FC", Home)) %>%
	mutate(Away = ifelse(Away == "Austin", "Austin FC", Away)) %>%
	mutate(Home = ifelse(Home == "Charlotte", "Charlotte FC", Home)) %>%
	mutate(Away = ifelse(Away == "Charlotte", "Charlotte FC", Away)) %>%
	mutate(Home = ifelse(Home == "Crew", "Columbus Crew", Home)) %>%
	mutate(Away = ifelse(Away == "Crew", "Columbus Crew", Away)) %>%
	mutate(Home = ifelse(Home == "Dynamo FC", "Houston Dynamo FC", Home)) %>%
	mutate(Away = ifelse(Away == "Dynamo FC", "Houston Dynamo FC", Away)) %>%
	mutate(Home = ifelse(Home == "Fire", "Chicago Fire FC", Home)) %>%
	mutate(Away = ifelse(Away == "Fire", "Chicago Fire FC", Away)) %>%
	mutate(Home = ifelse(Home == "Inter Miami", "Inter Miami CF", Home)) %>%
	mutate(Away = ifelse(Away == "Inter Miami", "Inter Miami CF", Away)) %>%
	mutate(Home = ifelse(Home == "Minnesota United", "Minnesota United FC", Home)) %>%
	mutate(Away = ifelse(Away == "Minnesota United", "Minnesota United FC", Away)) %>%
	mutate(Home = ifelse(Home == "NE Revolution", "New England Revolution", Home)) %>%
	mutate(Away = ifelse(Away == "NE Revolution", "New England Revolution", Away)) %>%
	mutate(Home = ifelse(Home == "Nashville", "Nashville SC", Home)) %>%
	mutate(Away = ifelse(Away == "Nashville", "Nashville SC", Away)) %>%
	mutate(Home = ifelse(Home == "Orlando City", "Orlando City SC", Home)) %>%
	mutate(Away = ifelse(Away == "Orlando City", "Orlando City SC", Away)) %>%
	mutate(Home = ifelse(Home == "Philadelphia", "Philadelphia Union", Home)) %>%
	mutate(Away = ifelse(Away == "Philadelphia", "Philadelphia Union", Away))%>%
	mutate(Home = ifelse(Home == "RSL", "Real Salt Lake", Home)) %>%
	mutate(Away = ifelse(Away == "RSL", "Real Salt Lake", Away)) %>%
	mutate(Home = ifelse(Home == "Rapids", "Colorado Rapids", Home)) %>%
	mutate(Away = ifelse(Away == "Rapids", "Colorado Rapids", Away)) %>%
	mutate(Home = ifelse(Home == "SJ Earthquakes", "San Jose Earthquakes", Home)) %>%
	mutate(Away = ifelse(Away == "SJ Earthquakes", "San Jose Earthquakes", Away)) %>%
	mutate(Home = ifelse(Home == "Seattle", "Seattle Sounders FC", Home)) %>%
	mutate(Away = ifelse(Away == "Seattle", "Seattle Sounders FC", Away)) %>%
	mutate(Home = ifelse(Home == "Sporting KC", "Sporting Kansas City", Home)) %>%
	mutate(Away = ifelse(Away == "Sporting KC", "Sporting Kansas City", Away)) %>%
	mutate(Home = ifelse(Home == "St. Louis", "St. Louis City FC", Home)) %>%
	mutate(Away = ifelse(Away == "St. Louis", "St. Louis City FC", Away)) %>%
	mutate(Home = ifelse(Home == "Vancouver W'caps", "Vancouver Whitecaps FC", Home)) %>%
	mutate(Away = ifelse(Away == "Vancouver W'caps", "Vancouver Whitecaps FC", Away))

glimpse(mls_match_2023)

mls_match_2023 %>%
	count(Country)

# get stadium info
mls_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_mls.rds")

glimpse(mls_stad_df)

# for total season pct capacity, sum all matches capacity (potential capacity) &
#   sum actual attendance to account for multiple venues
mls_att_23 <- mls_match_2023 %>%
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
	mutate(capacity2 = case_when(
		match_stadium == "Bank of America Stadium" & match_attendance > 38000 ~ 74867,
		match_stadium == "Mercedes-Benz Stadium" & match_attendance > 44000 ~ 71000,
		match_stadium == "Gillette Stadium" & match_attendance > 30000 ~ 64628,
		match_stadium == "Soldier Field" & match_attendance > 30000 ~ 61500,
		match_stadium == "BC Place Stadium" & match_attendance > 23000 ~ 54500,
		match_stadium == "Lumen Field" & match_attendance > 38000 ~ 68740,
		TRUE ~ capacity)) %>%
	mutate(match_pct_cap = ifelse(capacity2 > capacity,
																match_attendance / capacity2, match_attendance / capacity)) %>%
	select(league:capacity, capacity2, everything())

glimpse(mls_att_23)

mls_att_23 %>%
	count(match_home, match_stadium) %>%
	arrange(n) %>%
	view()

mls_att_23 %>%
	filter(match_stadium == "Gillette Stadium") %>%
	select(match_away, match_attendance, capacity, capacity2, match_pct_cap) %>%
	view()

saveRDS(mls_att_23, file = "~/Data/r/football data projects/data/att_2023_mls.rds")

mls_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_mls.rds")

## Look at Messi effect for Miami & Inter away, esp clubs prior attendance & when he would have played. ##

mls_att_23 %>%
	count(match_home)

mls_att_23 %>%
	filter(match_stadium == "DRV PNK Stadium") %>%
	mutate(with_messi = )
	ggplot(x = match_date, y = match_pct_cap)

mls_att_23 %>%
	mutate(messi_play = ifelse(
		match_date %in% c("2023-08-26", "2023-08-30", "2023-09-03", "2023-09-20",
		"2023-10-07", "2023-10-21"), 1, 0)) %>%
	mutate(messi_team = ifelse(match_date >= "2023-08-26", 1, 0)) %>%
	select(match_date, match_home, match_away, match_attendance, messi_team, messi_play) %>%
	mutate(miami = ifelse(match_home == "Inter Miami CF" | match_away == "Inter Miami CF", 1, 0)) %>%
	filter(messi_team == 1) %>%
	filter(miami == 1) %>%
	filter(match_home != "Inter Miami CF") %>%
	count(match_home) %>%
	view()

messi_effect <- mls_att_23 %>%
	mutate(miami = ifelse(match_home == "Inter Miami CF" | match_away == "Inter Miami CF", "Yes", "No")) %>%
	mutate(messi_play = ifelse((miami == "Yes" &
		match_date %in% c("2023-08-26", "2023-08-30", "2023-09-03", "2023-09-20",
											"2023-10-07", "2023-10-21")), "Yes", "No")) %>%
	mutate(messi_team = ifelse(match_date >= "2023-08-26" & miami == "Yes", "Yes", "No")) %>%
	mutate(messi_status = case_when(messi_team == 'Yes' & messi_play == "Yes" ~ "Played",
																	messi_team == 'Yes' & messi_play == "No" ~ "Did not play",
																	TRUE ~ "Not in team yet")) %>%
	mutate(messi_away = ifelse(
		match_home %in% c("Atlanta United FC", "Charlotte FC", "Chicago Fire FC", "LAFC", "NY Red Bulls", "Orlando City SC"),
		"Yes", "No"))

glimpse(messi_effect)

messi_effect %>%
	filter(messi_play == "No") %>%
	filter(messi_team == "Yes") %>%
	view()

messi_effect %>%
	filter(match_home == "Charlotte FC") %>%
	view()

messi_effect %>%
	#	filter(match_home == "Inter Miami CF") %>%
	filter(
		match_home %in% c("Atlanta United FC", "Charlotte FC", "Chicago Fire FC", "LAFC",
											"NY Red Bulls", "Orlando City SC", "Inter Miami CF")) %>%
	group_by(match_home, messi_team) %>%
	summarise(avg_capacity = mean(capacity2)) %>%
	pivot_wider(names_from = messi_team,
							names_prefix = "messi_",
							values_from = avg_capacity) %>%
	mutate(messi_diff = messi_Yes - messi_No) %>%
	mutate(messi_diff_pct = messi_diff / messi_No)

messi_effect %>%
#	filter(match_home == "Inter Miami CF") %>%
	filter(
		match_home %in% c("Atlanta United FC", "Charlotte FC", "Chicago Fire FC", "LAFC",
											"NY Red Bulls", "Orlando City SC", "Inter Miami CF")) %>%
	group_by(match_home, messi_team) %>%
	summarise(avg_attend = mean(match_attendance)) %>%
	pivot_wider(names_from = messi_team,
							names_prefix = "messi_",
							values_from = avg_attend) %>%
	mutate(messi_diff = messi_Yes - messi_No) %>%
	mutate(messi_diff_pct = messi_diff / messi_No)

messi_effect %>%
	filter(
		match_home %in% c("Atlanta United FC", "Charlotte FC", "Chicago Fire FC", "LAFC",
											"NY Red Bulls", "Orlando City SC", "Inter Miami CF")) %>%
	ggplot(aes(x = match_date, y = match_attendance, fill = messi_status)) +
	geom_bar(stat = "identity") +
	facet_wrap(~  match_home, scales = "free")
+
	theme(legend.position = "none")

messi_effect %>%
	filter(
		match_home %in% c("Atlanta United FC", "Charlotte FC", "Chicago Fire FC", "LAFC",
											"NY Red Bulls", "Orlando City SC", "Inter Miami CF")) %>%
	ggplot(aes(x = match_date, y = match_pct_cap, fill = messi_play, color = messi_away)) +
	geom_bar(stat = "identity") +
	facet_wrap(~  match_home) +
	theme(legend.position = "none")



# create plotting df. 1st argument is df to run function on, 2nd is for name of df with _sum suffix
attend_sum(mls_att_23, "mls_att_23")
glimpse(mls_att_23_sum)

mls_att_23_sum %>%
	select(stadium_name, stadium_capacity) %>%
	view()


mls_att_23_sum %>%
	count(team_name, stadium_name) %>%
	group_by(team_name) %>%
	mutate(n_team = n()) %>%
#	filter(n_team >1) %>%
	arrange(team_name, stadium_name) %>%
	view()

## need to do mls_sum differently due to expandable stadium capacities
mls_att_23_sum <- mls_att_23 %>%
	group_by(match_home, match_stadium) %>%
	mutate(matches_home = n()) %>%
	mutate(attend_avg_team = round(mean(match_attendance), 0)) %>%
	mutate(attend_min_team = min(match_attendance)) %>%
	mutate(attend_max_team = max(match_attendance)) %>%
	mutate(attend_tot_team = sum(match_attendance)) %>%
	mutate(capacity_tot_team = sum(capacity2)) %>%
	mutate(capacity_pct_team = attend_tot_team / capacity_tot_team) %>%
	mutate(capacity_avg_team = round(capacity_tot_team / matches_home, 0)) %>%
	ungroup() %>%
	# league figures
	add_row(tibble_row(match_home = "League Average")) %>%
	mutate(attend_tot_league = sum(match_attendance, na.rm = TRUE)) %>%
	mutate(capacity_tot_league = sum(capacity2, na.rm = TRUE)) %>%
	mutate(capacity_pct_league = attend_tot_league / capacity_tot_league) %>%
	mutate(attend_avg_league = round(mean(match_attendance, na.rm = TRUE), 0)) %>%
	mutate(capacity_avg_league = round(mean(capacity2, na.rm = TRUE), 0)) %>%
	mutate(attend_avg_team = ifelse(match_home == "League Average", attend_avg_league, attend_avg_team)) %>%
	mutate(capacity_pct_team = ifelse(match_home == "League Average", capacity_pct_league, capacity_pct_team)) %>%
	mutate(capacity = ifelse(match_home == "League Average", capacity_avg_league, capacity2)) %>%
	mutate(capacity_avg_team = ifelse(match_home == "League Average", capacity, capacity_avg_team)) %>%
	# mutate(attend_med_league = round(median(match_attendance), 0)) %>%
	# mutate(attend_min_league = min(match_attendance)) %>%
	# mutate(attend_max_league = max(match_attendance)) %>%
	# mutate(capacity_med_league = round(median(capacity), 0)) %>%
	# mutate(capacity_min_league = min(capacity)) %>%
	mutate(capacity_max_league = max(capacity)) %>%
	# group_by(match_home) %>%
	# mutate(capacity_tot2 = sum(capacity)) %>%
	# mutate(attendance_tot2 = sum(match_attendance)) %>%
	# mutate(capacity_pct_team = attendance_tot2 / capacity_tot2) %>%
	# ungroup() %>%
	select(team_name = match_home, stadium_name = match_stadium,
				 matches_home, stadium_capacity = capacity, capacity_avg_team,
				 attend_avg_team, attend_min_team, attend_max_team,
				 attend_tot_team, capacity_tot_team, capacity_pct_team,
				 attend_avg_league,
				 #        attend_med_league, attend_min_league, attend_max_league,
				 capacity_avg_league,
				 #.      , capacity_med_league, capacity_min_league,
				 capacity_max_league,
				 attend_tot_league,
				 capacity_tot_league, capacity_pct_league, league) %>%
	#, capacity_pct_team) %>%
	distinct(team_name, stadium_name, .keep_all = T) %>%
	mutate(team_name = ifelse(team_name == "CF Montréal" & stadium_name == "Stade Olympique",
														paste0(team_name, "-", stadium_name), team_name)) %>%
	mutate(team_name = ifelse(team_name == "CF Montréal" & stadium_name == "Stade Saputo",
														paste0(team_name, "-", stadium_name), team_name)) %>%
	mutate(team_name = ifelse(team_name == "LA Galaxy" & stadium_name == "Dignity Health Sports Park",
														paste0(team_name, "-", "Dignity"), team_name)) %>%
	mutate(team_name = ifelse(team_name == "LA Galaxy" & stadium_name == "Rose Bowl",
														paste0(team_name, "-", stadium_name), team_name)) %>%
	mutate(team_name = ifelse(team_name == "NYCFC" & stadium_name == "Citi Field",
														paste0(team_name, "-", stadium_name), team_name)) %>%
	mutate(team_name = ifelse(team_name == "NYCFC" & stadium_name == "Yankee Stadium",
														paste0(team_name, "-", stadium_name), team_name)) %>%
	mutate(team_name = ifelse(team_name == "NYCFC" & stadium_name == "Red Bull Arena",
														paste0(team_name, "-", stadium_name), team_name)) %>%
	mutate(team_name = ifelse(team_name == "San Jose Earthquakes" & stadium_name == "Levi's Stadium",
														paste0(team_name, "-", "Levi's"), team_name)) %>%
	mutate(team_name = ifelse(team_name == "San Jose Earthquakes" & stadium_name == "PayPal Park",
														paste0(team_name, "-", "PayPal"), team_name)) %>%
	mutate(team_name = ifelse(team_name == "San Jose Earthquakes" & stadium_name == "Stanford Stadium",
														paste0(team_name, "-", "Stanford"), team_name))


# plot using plotting df
# run function
mls_attplot <- mls_att_23_sum %>%
	ggplot(aes(stadium_capacity, reorder(team_name, capacity_avg_team))) +
	# points for avg attendace & capacity
	geom_point(aes(x=capacity_avg_team, y= reorder(team_name, capacity_avg_team)),
						 color="#1F78B4", size=10, alpha = .5 ) +
	geom_point(aes(x=attend_avg_team, y= reorder(team_name, capacity_avg_team)),
						 color="#FF7F00", size=10, alpha = .5 ) +
	# data labels for points
	geom_text(data = mls_att_23_sum %>% filter(capacity_pct_team < .95),
						aes(x = attend_avg_team,
								label = format(round(attend_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
						color = "black", size = 2.5) +
	geom_text(data = mls_att_23_sum %>% filter(capacity_pct_team >= .95),
						aes(x = attend_avg_team,
								label = format(round(attend_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
						color = "black", size = 2.5, hjust = 1.5) +
	geom_text(aes(x = capacity_avg_team,
								label = format(round(capacity_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
						color = "black", size = 2.5) +
	# line connecting the points.
	geom_segment(aes(x=attend_avg_team + 900 , xend=capacity_avg_team - 900,
									 y=team_name, yend=team_name), color="lightgrey") +
	# sets league average in bold
	scale_y_discrete(labels= function(x) highlight(x, "League Average", "black")) +
	# text for avg season capacity
	geom_text(data = mls_att_23_sum %>% filter(capacity_avg_team < capacity_max_league & team_name != "League Average"),
						aes(x = capacity_avg_team + 1100, y = team_name,
								label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = -.02)) +
	geom_text(data = mls_att_23_sum %>% filter(team_name == "League Average"),
						aes(x = capacity_avg_team + 1100, y = team_name,
								label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = -.02, fontface = "bold")) +
	scale_x_continuous(limits = c(min(mls_att_23_sum$attend_avg_team),
																max(mls_att_23_sum$capacity_avg_team + 3000)),
										 breaks = scales::pretty_breaks(8),
										 labels = scales::comma_format(big.mark = ',')) +
	# scale_x_continuous(limits = c(min(mls_att_23_sum$stadium_capacity - 2000),
	# 															max(mls_att_23_sum$stadium_capacity + 3000)),
	# 									 breaks = scales::pretty_breaks(6),
	# 									 labels = scales::comma_format(big.mark = ',')) +
	labs(x = "Stadium capacity", y = "",
			 subtitle = "*The further the red dot is to the left of the blue dot, the more average attendance is less than stadium capacity. Teams sorted by stadium capacity.*",
			 caption = "*Match attendance data from FBRef using worldfootballr package. Stadium capacity data from Wikipedia*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				plot.title.position = "plot",
				plot.title = ggtext::element_textbox_simple(
					size = 12, fill = "cornsilk",
					lineheight = 1.5,
					padding = margin(5.5, 5.5, 5.5, 2),
					margin = margin(0, 0, 5.5, 0)),
				plot.subtitle = ggtext::element_markdown(size = 10),
				plot.caption = ggtext::element_markdown(),
				axis.text.x = ggtext::element_markdown(size = 10),
				axis.text.y = ggtext::element_markdown(size = 11))

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
mls_attplot +
	geom_text(data = mls_att_23_sum %>% filter(capacity_avg_team == capacity_max_league),
						aes(x = capacity_avg_team - 25000, y = team_name,
								label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = .04)) +
	annotate(geom = "richtext",
					 label = "*Notes:<br>
					 i) Atlanta, Charlotte, Chicago, New England, Seattle & Vancouver played some matches with expanded capacity
					 <br>but in their regular stadium. Capacity figure has been normalized to account for the changes.<br>
					 It is possible expanded capacity figures not reported correctly, so some match percentages may be incorrect.<br>
					 ii) Teams with +100% capacity might have sold standing-room tickets & thus exceeded reported seated capacity.<br>
					 iii) For CF Montréal at Stade Olympique, LA Galaxy at Rose Bowl, SJ Earthquakes at Levi's & Stanford, & <br>
					 NYCFC at Red Bull Area, only 1 match played at each venue.*",
					 x = 45000, y = "Colorado Rapids", hjust = 0, vjust = 0, size = 3.5) +
	labs(
		title = glue::glue("<b>Major League Soccer <span style='color: #FF7F00;'>Average attendance</span>,
	 		  <span style='color: #1F78B4;'>Stadium capacity</span></b>, and<b> avg pct capacity for season</b>, by club, 2023 season.</b><br>
				MLS overall at about 85% capacity. Lower percentages for the more poorly performing teams (Chicago)
											 and for one-off matches at larger venues."))

ggsave("images/plot_attendance_23_mls.jpg", width = 16, height = 10,
			 units = "in", dpi = 300)


######


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

####
