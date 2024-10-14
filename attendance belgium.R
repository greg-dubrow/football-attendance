# denmark attendance figures...
library(tidyverse)
library(tidylog)
library(janitor)

# load if needed
library(ggtext)
library(ggrepel)
library(glue)

source("~/Data/r/basic functions.R")
options(scipen=10000)

# functions for summary df and plots
source("attend_functions.R")

# load attendance data
bel_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "BEL") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
	select(Competition_Name:Round, match_week, Wk, Day:Referee)

%>%
	mutate(Venue = ifelse(Venue == "CASA Arena Horsens", "Nordstern Arena Horsens", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Parken", "Parken Stadium", Venue))

glimpse(bel_match_2023)

bel_match_2023 %>%
	count(Venue)

# get stadium info
den_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_den.rds") %>%
	mutate()

glimpse(den_stad_df)

den_stad_df %>%
	count(stadium) %>%
	view()

superligadk_att_23 <- den_match_2023 %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	left_join(den_stad_df, by = c("match_stadium" = "stadium")) %>%
	mutate(city = ifelse(match_stadium == "Brøndby Stadion", "Brøndby", city)) %>%
	mutate(team = ifelse(match_stadium == "Brøndby Stadion", "Brøndby", team)) %>%
	mutate(capacity = ifelse(match_stadium == "Brøndby Stadion", 28100, capacity)) %>%
	mutate(match_pct_cap = match_attendance / capacity)

saveRDS(superligadk_att_23, file = "~/Data/r/football data projects/data/att_2023_superliga_dk.rds")

superligadk_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_superliga_dk.rds")


attend_sum(superligadk_att_23, "superligadk_att_23")
glimpse(superligadk_att_23_sum)

# plot using plotting df
# run function
superligadk_attplot <- attend_plot1(superligadk_att_23_sum)
superligadk_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
superligadk_attplot +
	geom_text(data = superligadk_att_23_sum %>% filter(stadium_capacity == capacity_max_league),
						aes(x = stadium_capacity - 16000, y = team_name,
								label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = .04)) +
	labs(
		title = glue::glue("<b>Danish Superliga <span style='color: #FF7F00;'>Average attendance</span>,
	 		  <span style='color: #1F78B4;'>Stadium capacity</span></b>, and<b> avg pct capacity for season</b>, by club, 2022-23 season.</b><br>
				Superliga overall at about 2/3 capacity, only Midtjylland & FC København above 70% capacity.
											 Many clubs have standing spaces included in capacity figure."))

ggsave("images/plot_attendance_23_superligadk.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)


