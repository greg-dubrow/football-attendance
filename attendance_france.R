# france attendance figures...
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

fra_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "FRA") %>%
#	filter(!Home == "Hamburger SV") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
	select(Competition_Name:Round, match_week, Wk, Day:Referee) %>%
	mutate(Home = ifelse(Home == "Lyon", "Olympique Lyonnais", Home)) %>%
	mutate(Away = ifelse(Away == "Lyon", "Olympique Lyonnais", Away)) %>%
	mutate(Home = ifelse(Home == "Marseille", "Olympique de Marseille", Home)) %>%
	mutate(Away = ifelse(Away == "Marseille", "Olympique de Marseille", Away)) %>%
	mutate(Home = ifelse(Home == "Lille", "Lille OSC", Home)) %>%
	mutate(Away = ifelse(Away == "Lille", "Lille OSC", Away)) %>%
	mutate(Home = ifelse(Home == "Clermont Foot", "Clermont Foot 63", Home)) %>%
	mutate(Away = ifelse(Away == "Clermont Foot", "Clermont Foot 63", Away)) %>%
	mutate(Home = ifelse(Home == "Montpellier", "Montpellier HSC", Home)) %>%
	mutate(Away = ifelse(Away == "Montpellier", "Montpellier HSC", Away)) %>%
	mutate(Home = ifelse(Home == "Nice", "OGC Nice", Home)) %>%
	mutate(Away = ifelse(Away == "Nice", "OGC Nice", Away)) %>%
	mutate(Home = ifelse(Home == "Monaco", "AS Monaco FC", Home)) %>%
	mutate(Away = ifelse(Away == "Monaco", "AS Monaco FC", Away)) %>%
	mutate(Home = ifelse(Home == "Lorient", "FC Lorient", Home)) %>%
	mutate(Away = ifelse(Away == "Lorient", "FC Lorient", Away)) %>%
	mutate(Home = ifelse(Home == "Paris S-G", "Paris Saint-Germain F.C.", Home)) %>%
	mutate(Away = ifelse(Away == "Paris S-G", "Paris Saint-Germain F.C.", Away)) %>%
	mutate(Home = ifelse(Home == "Lens", "RC Lens", Home)) %>%
	mutate(Away = ifelse(Away == "Lens", "RC Lens", Away)) %>%
	mutate(Home = ifelse(Home == "Nantes", "FC Nantes", Home)) %>%
	mutate(Away = ifelse(Away == "Nantes", "FC Nantes", Away)) %>%
	mutate(Home = ifelse(Home == "Toulouse", "Toulouse FC", Home)) %>%
	mutate(Away = ifelse(Away == "Toulouse", "Toulouse FC", Away)) %>%
	mutate(Home = ifelse(Home == "Strasbourg", "RC Strasbourg", Home)) %>%
	mutate(Away = ifelse(Away == "Strasbourg", "RC Strasbourg", Away)) %>%
	mutate(Home = ifelse(Home == "Rennes", "Stade Rennais FC", Home)) %>%
	mutate(Away = ifelse(Away == "Rennes", "Stade Rennais FC", Away)) %>%
	mutate(Home = ifelse(Home == "Reims", "Stade Reims", Home)) %>%
	mutate(Away = ifelse(Away == "Reims", "Stade Reims", Away)) %>%
	mutate(Home = ifelse(Home == "Troyes", "Troyes AC", Home)) %>%
	mutate(Away = ifelse(Away == "Troyes", "Troyes AC", Away)) %>%
	mutate(Home = ifelse(Home == "Angers", "Angers SCO", Home)) %>%
	mutate(Away = ifelse(Away == "Angers", "Angers SCO", Away)) %>%
	mutate(Home = ifelse(Home == "Auxerre", "AJ Auxerre", Home)) %>%
	mutate(Away = ifelse(Away == "Auxerre", "AJ Auxerre", Away)) %>%
	mutate(Home = ifelse(Home == "Brest", "Stade Brestois 29", Home)) %>%
	mutate(Away = ifelse(Away == "Brest", "Stade Brestois 29", Away)) %>%
	mutate(Home = ifelse(Home == "Ajaccio", "AC Ajaccio", Home)) %>%
	mutate(Away = ifelse(Away == "Ajaccio", "AC Ajaccio", Away)) %>%

	mutate(Venue = ifelse (Home == "Olympique Lyonnais" & Venue == "Matmut Stadium de Gerland",
												 "Groupama Stadium", Venue)) %>%
	mutate(Venue = ifelse (Home == "Montpellier HSC" & Venue == "Stade Raoul-Barrière",
												 "Stade de la Mosson", Venue)) %>%
	mutate(Venue = ifelse (Home == "OGC Nice" & Venue == "Stade Municipal du Ray",
												 "Allianz Riviera", Venue)) %>%
	mutate(Venue = ifelse (Home == "Lille OSC" & Venue == "Stadium Lille Métropole - Terrain Annexe",
												 "Stade Pierre-Mauroy", Venue)) %>%
	mutate(Venue = ifelse (Home == "Lille OSC" & Venue == "Stadium Lille Métropole",
												 "Stade Pierre-Mauroy", Venue)) %>%

	mutate(Venue = ifelse (Venue == "Stade Gabriel Montpied", "Stade Gabriel-Montpied", Venue)) %>%
	mutate(Venue = ifelse (Venue == "Decathlon Arena - Stade Pierre-Mauroy", "Stade Pierre-Mauroy", Venue)) %>%
	mutate(Venue = ifelse (Venue == "Orange Vélodrome", "Stade Vélodrome", Venue)) %>%
	mutate(Venue = ifelse (Venue == "Stade de Nice", "Allianz Riviera", Venue)) %>%
	mutate(Venue = ifelse (Venue == "Stade de l'Abbé Deschamps", "Stade de l'Abbé-Deschamps", Venue)) %>%
	mutate(Venue = ifelse (Venue == "Stade Louis II.", "Stade Louis II", Venue)) %>%
	mutate(Venue = ifelse (Venue == "Stade Yves Allainmat - Le Moustoir", "Stade du Moustoir", Venue)) %>%
	mutate(Venue = ifelse (Venue == "Stade de la Mosson-Mondial 98", "Stade de la Mosson", Venue)) %>%
	mutate(Venue = ifelse (Venue == "Stade de la Beaujoire - Louis Fonteneau", "Stade de la Beaujoire", Venue))

glimpse(fra_match_2023)

fra_match_2023 %>%
	filter(Home == "Lille") %>%
	view()


fra_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_fra.rds")

fra_att_23 <- fra_match_2023 %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	left_join(fra_stad_df, by = c("match_stadium" = "stadium")) %>%
# 	mutate(capacity = ifelse(match_stadium == "Neckarstadion", 50000, capacity)) %>%
# 	mutate(capacity = ifelse(match_stadium == "Waldstadion", 51500, capacity)) %>%
# 	mutate(capacity = ifelse(match_stadium == "Mewa Arena", 33305, capacity)) %>%
	mutate(match_pct_cap = match_attendance / capacity)

glimpse(fra_att_23)

saveRDS(fra_att_23, file = "~/Data/r/football data projects/data/att_2023_fra.rds")

# top line stats for league
fra_att_23 %>%
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
attend_sum(fra_att_23, "fra_att_23")
glimpse(fra_att_23_sum)

fra_att_23_sum %>%
	select(stadium_name, team_name, stadium_capacity, attend_avg_team) %>%
	view()

fra_attplot <- attend_plot(fra_att_23_sum)
fra_attplot

# league plot - adjust max geom_text (DATA SET), title (LEAGUE)
fra_attplot +
	geom_text(data = fra_att_23_sum %>% filter(stadium_capacity == capacity_max_league),
						aes(x = stadium_capacity - 15000, y = team_name,
								label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = .2)) +
	labs(
		title = glue::glue("<b>Ligue 1 <span style='color: #A74E79;'>Average attendance</span>,
		<span style='color: #4E79A7;'>Stadium capacity </span></b>and<b> avg pct capacity for season</b>, by club, 2022-23 season.<br>
		There is a lot of variance in demand for tickets relative to stadium capacity in Ligue 1.
			Some clubs are well above 90% capacity, some play to less-than half full houses."))


ggsave("ligue1_attendance23.jpg", width = 14, height = 8,
			 units = "in", dpi = 300)
