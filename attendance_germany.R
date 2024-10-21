# germany attendance figures...
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
ger_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "GER") %>%
	filter(!Home == "Hamburger SV") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
	select(Competition_Name:Round, match_week, Wk, Day:Referee) %>%
	mutate(Home = ifelse(Home == "Eint Frankfurt", "Eintracht Frankfurt", Home)) %>%
	mutate(Away = ifelse(Away == "Eint Frankfurt", "Eintracht Frankfurt", Away)) %>%
	mutate(Home = ifelse(Home == "M'Gladbach", "Borussia Mönchengladbach", Home)) %>%
	mutate(Away = ifelse(Away == "M'Gladbach", "Borussia Mönchengladbach", Away)) %>%
	mutate(Home = ifelse(Home == "Dortmund", "Borussia Dortmund", Home)) %>%
	mutate(Away = ifelse(Away == "Dortmund", "Borussia Dortmund", Away)) %>%
	mutate(Home = ifelse(Home == "Stuttgart", "VfB Stuttgart", Home)) %>%
	mutate(Away = ifelse(Away == "Stuttgart", "VfB Stuttgart", Away)) %>%
	mutate(Home = ifelse(Home == "Köln", "FC Köln", Home)) %>%
	mutate(Away = ifelse(Away == "Köln", "FC Köln", Away)) %>%
	mutate(Home = ifelse(Home == "Hoffenheim", "TSG 1899 Hoffenheim", Home)) %>%
	mutate(Away = ifelse(Away == "Hoffenheim", "TSG 1899 Hoffenheim", Away)) %>%
	mutate(Home = ifelse(Home == "Leverkusen", "Bayer 04 Leverkusen", Home)) %>%
	mutate(Away = ifelse(Away == "Leverkusen", "Bayer 04 Leverkusen", Away)) %>%
	mutate(Home = ifelse(Home == "Wolfsburg", "VfL Wolfsburg", Home)) %>%
	mutate(Away = ifelse(Away == "Wolfsburg", "VfL Wolfsburg", Away)) %>%
	mutate(Home = ifelse(Home == "Bochum", "VfL Bochum", Home)) %>%
	mutate(Away = ifelse(Away == "Bochum", "VfL Bochum", Away)) %>%
	mutate(Home = ifelse(Home == "Mainz 05", "FSV Mainz 05", Home)) %>%
	mutate(Away = ifelse(Away == "Mainz 05", "FSV Mainz 05", Away)) %>%
	mutate(Home = ifelse(Home == "Freiburg", "SC Freiburg", Home)) %>%
	mutate(Away = ifelse(Away == "Freiburg", "SC Freiburg", Away)) %>%

	mutate(Venue = ifelse(Venue == "Deutsche Bank Park", "Waldstadion", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Signal Iduna Park", "Westfalenstadion", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Mercedes-Benz Arena", "Neckarstadion", Venue)) %>%
	mutate(Venue = ifelse(Venue == "RheinEnergieSTADION", "Müngersdorfer Stadion", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Veltins-Arena", "Arena AufSchalke", Venue)) %>%
	mutate(Venue = ifelse(Venue == "PreZero Arena", "Rhein-Neckar-Arena", Venue))

glimpse(ger_match_2023)

ger_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_ger.rds")

# adjust stuttgart stadium capacity
# per http://stadiumdb.com/news/2022/07/stuttgart_expansion_of_mercedesbenz_arena_for_euro_2024
# frankfurt per https://www.bundesliga.com/en/bundesliga/clubs/eintracht-frankfurt
# and https://www.bundesliga.com/en/bundesliga/news/stadiums-in-2022-23-allianz-arena-signal-iduna-park-red-bull-arena-20773
bundes_att_23 <- ger_match_2023 %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	left_join(ger_stad_df, by = c("match_stadium" = "stadium")) %>%
	mutate(capacity = ifelse(match_stadium == "Neckarstadion", 50000, capacity)) %>%
	mutate(capacity = ifelse(match_stadium == "Waldstadion", 51500, capacity)) %>%
	mutate(capacity = ifelse(match_stadium == "Mewa Arena", 33305, capacity)) %>%
	mutate(match_pct_cap = match_attendance / capacity)

glimpse(bundes_att_23)

saveRDS(bundes_att_23, file = "~/Data/r/football data projects/data/att_2023_bundes.rds")

bundes_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_bundes.rds")


bundes_att_23 %>%
	filter(is.na(capacity)) %>%
	distinct(match_stadium, .keep_all = T) %>%
	select(match_stadium, match_home) %>%
	view()

# top line stats for league
bundes_att_23 %>%
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
attend_sum(bundes_att_23, "bundes_att_23")
glimpse(bundes_att_23_sum)

bundes_att_23_sum %>%
	count(team_name)

# plot using plotting df
# run function
bundes_attplot <- attend_plot_comb(bundes_att_23_sum)
bundes_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
bundes_attplot +
  plot_annotation(title = "<b>Bundesliga Germany
  <span style='color: #8DA0CB;'>Average percent of capacity for season</span></b><i> (left bar chart)</i>,
  <b><span style='color: #FF7F00;'>Average attendance</span></b> and
  <b><span style='color: #1F78B4;'>Stadium capacity</span></b> (right bubble chart), by club, 2022-23 season.<br>
				Bundesliga ticket demand is very strong, with the league averaging more than 93% capacity.
                  The teams at less than 90% tend to be at the bottom of the table.<br>
                  See scatterplot below for stadium capacity numbers obscured by overlapping bubbles.",
                  theme = theme(plot.title =
                                  ggtext::element_textbox_simple(
                                    size = 12, fill = "cornsilk",
                                    lineheight = 1.5,
                                    padding = margin(5.5, 5.5, 5.5, 2),
                                    margin = margin(0, 0, 5.5, 0))))

ggsave("images/plot_attendance_23_bundes.jpg", width = 16, height = 8,
			 units = "in", dpi = 300)

bundes_scatter <- attend_scatter(bundes_att_23_sum)
bundes_scatter

ggsave("images/plot_att_scatter_23_bundes.jpg", width = 15, height = 8,
       units = "in", dpi = 300)
