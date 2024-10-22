# spain attendance figures...
library(tidyverse)
library(tidylog)
library(janitor)
library(ggtext)
library(ggrepel)
library(glue)
library(patchwork)

source("~/Data/r/basic functions.R")
options(scipen=10000)

# functions for summary df and plots
source("attend_functions.R")

# load attendance data
esp_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "ESP") %>%
	filter(Competition_Name == "La Liga") %>%
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
	mutate(capacity = ifelse(match_stadium == "Estadio de Balaídos", 24685, capacity)) %>%
	mutate(capacity = ifelse(match_stadium == "Estadio Santiago Bernabéu", 63000, capacity)) %>%
	mutate(capacity = ifelse(match_stadium == "Estadio Cívitas Metropolitano", 68456, capacity)) %>%
	mutate(capacity = ifelse(match_stadium == "Camp Nou" & match_date > as.Date("2022-11-05"),
													 95877, capacity)) %>%
	mutate(match_pct_cap = match_attendance / capacity)

glimpse(laliga_att_23)

saveRDS(laliga_att_23, file = "~/Data/r/football data projects/data/att_2023_laliga.rds")

laliga_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_laliga.rds")

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
laliga_attplot <- attend_plot_comb(laliga_att_23_sum)
laliga_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
laliga_attplot +
  plot_annotation(title = "<b>La Liga
  <span style='color: #8DA0CB;'>Average percent of capacity for season</span></b><i> (left bar chart)</i>,
  <b><span style='color: #FF7F00;'>Average attendance</span></b> and
  <b><span style='color: #1F78B4;'>Stadium capacity</span></b> (right bubble chart), by club, 2022-23 season.<br>
		In La Liga there is a fair amount of variance in the percentage capacity numbers. The more successful clubs
    are above 80% capacity, <br>while less successful clubs are mostly between 50% - 70% full.<br>
    Real Madrid & Celta Viga had reduced stadium capacity due to construction.
    Villareal played some matches at Estadi Ciutat de València due to construction at La Cerámica.",
                  theme = theme(plot.title =
                                  ggtext::element_textbox_simple(
                                    size = 12, fill = "cornsilk",
                                    lineheight = 1.5,
                                    padding = margin(5.5, 5.5, 5.5, 2),
                                    margin = margin(0, 0, 5.5, 0))))

ggsave("images/plot_attendance_23_laliga.jpg", width = 14, height = 8,
			 units = "in", dpi = 300)


laliga_scatter <- attend_scatter(laliga_att_23_sum)
laliga_scatter

ggsave("images/plot_att_scatter_23_laliga.jpg", width = 15, height = 8,
       units = "in", dpi = 300)


## match attendance as pct capacity by match day, time
glimpse(laliga_att_23)

laliga_att_23 %>%
	group_by(match_day) %>%
	summarise(cap_day_avg = mean(match_pct_cap),
						cap_day_min = min(match_pct_cap),
						cap_day_max = max(match_pct_cap),
						match_n = n()) %>%
	arrange(cap_day_avg) %>%
	view()

laliga_att_23 %>%
	group_by(match_time) %>%
	summarise(cap_time_avg = mean(match_pct_cap),
						cap_time_min = min(match_pct_cap),
						cap_time_max = max(match_pct_cap),
						match_n = n()) %>%
	arrange(cap_time_avg) %>%
	view()


laliga_att_23 %>%
	group_by(match_day, match_time) %>%
	summarise(cap_day_time = mean(match_pct_cap),
						match_n = n()) %>%
	filter(match_n > 3) %>%
	arrange(cap_day_time) %>%
	view()

laliga_att_23 %>%
	group_by(match_home, match_day) %>%
	summarise(cap_home_day = mean(match_pct_cap),
						match_n = n()) %>%
#	filter(match_n > 5) %>%
	view()
