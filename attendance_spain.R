# spain attendance figures...epl and efl leagues
library(tidyverse)
library(tidylog)
library(janitor)
library(ggtext)
library(ggrepel)

source("~/Data/r/basic functions.R")
options(scipen=10000)

# functions for summary df and plots
source("attend_functions.R")

# load attendance data
esp_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "ESP") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
	select(Competition_Name:Round, match_week, Wk, Day:Referee) %>%
	mutate(Venue = ifelse(Venue == "Iberostar Estadi", "Mallorca Son Moix Stadium", Venue)) %>%
	mutate(Venue = ifelse(Venue == "RCDE Stadium", "Estadi Cornellà-El Prat", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Estadio del Rayo Vallecano", "Vallecas", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Coliseum Alfonso Pérez", "Estadio Coliseum", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Reale Arena" ,"Estadio de Anoeta", Venue))

glimpse(esp_match_2023)

esp_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_spa.rds")

glimpse(esp_stad_df)

esp_stad_df %>%
	count(autonomous_community) %>%
	arrange(desc(n), autonomous_community)

esp_stad_df %>%
	group_by(autonomous_community) %>%
	summarise(capacity_mean = mean(capacity),
						stadium_n = n()) %>%
	arrange(desc(capacity_mean))

esp_stad_df %>%
	count(province) %>%
	arrange(desc(n), province)


# join match data to stadium info
# change bernabeu capacity to 63000
  # https://www.thestadiumbusiness.com/2023/07/18/real-madrid-reports-e11-8m-profit-amid-ongoing-bernabeu-redevelopment/
# change metripolitano to 68456
laliga_att_23 <- esp_match_2023 %>%
#	filter(Competition_Name == "Premier League") %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	left_join(esp_stad_df, by = c("match_stadium" = "stadium")) %>%
	mutate(capacity = ifelse(match_stadium == "Estadio Santiago Bernabéu", 63000, capacity)) %>%
	mutate(capacity = ifelse(match_stadium == "Estadio Cívitas Metropolitano", 68456, capacity)) %>%
	mutate(capacity = ifelse(match_stadium == "Camp Nou" & match_date > as.Date("2022-11-05"),
													 95877, capacity)) %>%
	mutate(match_pct_cap = match_attendance / capacity)

glimpse(laliga_att_23)

# top line stats for league
laliga_att_23 %>%
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
attend_sum(laliga_att_23, "laliga_att_23")
glimpse(laliga_att_23_sum)

# adjust villareal name to account for using two stadiums
laliga_att_23_sum <- laliga_att_23_sum %>%
	mutate(team_name = ifelse(
		team_name == "Villarreal" & stadium_name == "Estadio Ciudad de Valencia", "Villareal - Valencia", team_name))

# plot using plotting df
# run function
laliga_attplot <- attend_plot1(laliga_att_23_sum)
laliga_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
laliga_attplot +
	annotate(geom = "richtext",
					 label = "*Reduced stadium capacity due to construction.*",
					 x = 37500, y = "Real Madrid", fill = NA, label.color = NA, size = 4) +
	annotate(geom = "richtext",
					 label = "*Some matches at Estadi Ciutat de València due to construction at La Cerámica.*",
					 x = 77000, y = "Villareal - Valencia", fill = NA, label.color = NA, size = 4) +
	labs(
		title = glue::glue("<b>La Liga <span style='color: #A74E79;'>Average attendance</span> and
		<span style='color: #4E79A7;'>Stadium capacity</span> by club, 2022-23 season.</b><br>
		In La Liga there is lots of variance in the percentage of tickets sold/given away on match days.
		The more successful clubs are above 80% capacity, with less uscessful clubs mostly between 50% - 70% full."))

laliga_attplot

ggsave("laliga_attendance23_1.jpg", width = 14, height = 8,
			 units = "in", dpi = 300)

## match attendance as pct capacity by match day, time
glimpse(laliga_att_23)

laliga_att_23 %>%
	group_by(match_day, match_time) %>%
	summarise(cap_day_time = mean(match_pct_cap),
						match_n = n()) %>%
	filter(match_n > 5) %>%
	view()

laliga_att_23 %>%
	group_by(match_home, match_day) %>%
	summarise(cap_home_day = mean(match_pct_cap),
						match_n = n()) %>%
#	filter(match_n > 5) %>%
	view()
