# italy attendance figures...
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
ita_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "ITA") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
	select(Competition_Name:Round, match_week, Wk, Day:Referee) %>%
	# random fixes
	mutate(Attendance = ifelse (Round == "Relegation tie-breaker", 10000, Attendance)) %>%
	# fix stadium names
	mutate(Venue = ifelse(Venue == "Stadio Giuseppe Meazza", "San Siro", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Stadio Comunale Via Del Mare", "Stadio Via del Mare", Venue)) %>%
	mutate(Venue = ifelse(Venue == "U-Power Stadium", "Stadio Brianteo", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Stadio Comunale Luigi Ferraris", "Stadio Luigi Ferraris", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Mapei Stadium - Città del Tricolore", "Mapei Stadium – Città del Tricolore", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Dacia Arena" & Home == "Spezia",
												"Stadio Alberto Picco", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Dacia Arena" & Home == "Udinese",
												"Stadio Friuli", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Stadio Olimpico di Torino", "Stadio Olimpico Grande Torino", Venue)) %>%

	# fix team names
	mutate(Home = ifelse(Home == "Inter", "Internazionale", Home)) %>%
	mutate(Away = ifelse(Away == "Inter", "Internazionale", Away)) %>%
	mutate(Home = ifelse(Home == "Lecce", "US Lecce", Home)) %>%
	mutate(Away = ifelse(Away == "Lecce", "US Lecce", Away)) %>%
	mutate(Home = ifelse(Home == "Milan", "AC Milan", Home)) %>%
	mutate(Away = ifelse(Away == "Milan", "AC Milan", Away)) %>%
	mutate(Home = ifelse(Home == "Torino", "Torino FC", Home)) %>%
	mutate(Away = ifelse(Away == "Torino", "Torino FC", Away)) %>%
	mutate(Home = ifelse(Home == "Spezia", "Spezia Calcio", Home)) %>%
	mutate(Away = ifelse(Away == "Spezia", "Spezia Calcio", Away)) %>%
	mutate(Home = ifelse(Home == "Monza", "AC Monza", Home)) %>%
	mutate(Away = ifelse(Away == "Monza", "AC Monza", Away)) %>%
	mutate(Home = ifelse(Home == "Empoli", "Empoli FC", Home)) %>%
	mutate(Away = ifelse(Away == "Empoli", "Empoli FC", Away)) %>%
	mutate(Home = ifelse(Home == "Napoli", "SSC Napoli", Home)) %>%
	mutate(Away = ifelse(Away == "Napoli", "SSC Napoli", Away)) %>%
	mutate(Home = ifelse(Home == "Bologna", "Bologna FC", Home)) %>%
	mutate(Away = ifelse(Away == "Bologna", "Bologna FC", Away)) %>%
	mutate(Home = ifelse(Home == "Roma", "AS Roma", Home)) %>%
	mutate(Away = ifelse(Away == "Roma", "AS Roma", Away))

glimpse(ita_match_2023)

# get stadium info
ita_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_ita.rds")

# join match data to stadium info
seriea_att_23 <- ita_match_2023 %>%
#	filter(Competition_Name == "Premier League") %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	left_join(ita_stad_df, by = c("match_stadium" = "stadium")) %>%
	mutate(match_pct_cap = match_attendance / capacity)

glimpse(seriea_att_23)

seriea_att_23 %>%
#	filter(is.na(city)) %>%
	filter(match_home != team) %>%
	select(match_stadium, match_home, city, region, capacity, team) %>%
	distinct(match_stadium, match_home, .keep_all = T) %>%
	view()

saveRDS(seriea_att_23, file = "~/Data/r/football data projects/data/att_2023_seriea.rds")

seriea_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_seriea.rds")

# create plotting df. 1st argument is df to run function on, 2nd is for name of df with _sum suffix
attend_sum(seriea_att_23, "seriea_att_23")
glimpse(seriea_att_23_sum)

# plot using plotting df
# run function
seriea_attplot <- attend_plot1(seriea_att_23_sum)
seriea_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
seriea_attplot +
	geom_text(data = seriea_att_23_sum %>% filter(stadium_capacity == capacity_max_league),
						aes(x = stadium_capacity - 23000, y = team_name,
								label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = .04)) +
	annotate(geom = "richtext",
					 label = "*Shares San Siro with AC Milan.*",
					 x = 14000, y = "Internazionale", fill = NA, label.color = NA, size = 4) +
	annotate(geom = "richtext",
					 label = "*Shares Stadio Olimpico with AS Roma.*",
					 x = 15000, y = "Lazio", fill = NA, label.color = NA, size = 4) +
	labs(
		title = glue::glue("<b>Serie A <span style='color: #FF7F00;'>Average attendance</span>,
	 		  <span style='color: #1F78B4;'>Stadium capacity</span></b>, and<b> avg pct capacity for season</b>, by club, 2022-23 season.</b><br>
				Serie A overall at about 3/4 capacity, and even the top clubs do not get to any more than 90% capacity."))

ggsave("images/plot_attendance_23_seriea.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)


