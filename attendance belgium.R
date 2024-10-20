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
  mutate(Home = ifelse(Home == "Mechelen", "KV Mechelen", Home)) %>%
  mutate(Away = ifelse(Away == "Mechelen", "KV Mechelen", Away)) %>%
  mutate(Home = ifelse(Home == "Union SG", "Union Saint-Gilloise", Home)) %>%
  mutate(Away = ifelse(Away == "Union SG", "Union Saint-Gilloise", Away)) %>%
  mutate(Venue = ifelse(Venue == "GHELAMCO-arena", "Planet Group Arena", Venue)) %>%
  mutate(Venue = ifelse(Venue == "King Power at Den Dreef Stadion", "Den Dreef", Venue)) %>%
  select(Competition_Name:Round, match_week, Wk, Day:Referee)

glimpse(bel_match_2023)


# get stadium info
bel_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_bel.rds") %>%
	mutate(stadium = ifelse(stadium == "Jan Breydel Stadium", "Jan Breydelstadion", stadium)) %>%
  mutate(stadium = ifelse(stadium == "Achter de Kazerne", "AFAS-stadion Achter de Kazerne", stadium)) %>%
  mutate(stadium = ifelse(stadium == "Stade Joseph Marien", "Stade Joseph Mariën", stadium)) %>%
  mutate(stadium = ifelse(stadium == "Kehrweg Stadion", "Stadion am Kehrweg", stadium))

glimpse(bel_stad_df)

den_stad_df %>%
	count(stadium) %>%
	view()

bel_att_23 <- bel_match_2023 %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	left_join(bel_stad_df, by = c("match_stadium" = "stadium")) %>%
  mutate(match_attendance = ifelse(match_home == "Anderlecht" & match_date == "2023-01-18",
                                   0, match_attendance)) %>%
  mutate(match_pct_cap = match_attendance / capacity)

bel_att_23 %>%
  filter(match_home == "Cercle Brugge") %>%
  view()

saveRDS(bel_att_23, file = "~/Data/r/football data projects/data/att_2023_bel.rds")

bel_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_bel.rds")

attend_sum(bel_att_23, "bel_att_23")
glimpse(bel_att_23_sum)

# plot using plotting df
# run function
bel_attplot <- attend_plot1(bel_att_23_sum)
bel_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
bel_attplot +
	geom_text(data = bel_att_23_sum %>% filter(stadium_capacity == capacity_max_league & team_name == "Club Brugge"),
						aes(x = stadium_capacity - 16000, y = team_name,
								label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = .04, vjust = .02)) +
  geom_text(data = bel_att_23_sum %>% filter(stadium_capacity == capacity_max_league & team_name == "Cercle Brugge"),
            aes(x = stadium_capacity - 16000, y = team_name,
                label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
                hjust = .04, vjust = -.1)) +
  labs(
		title = glue::glue("<b>Belgian Jupiler League <span style='color: #FF7F00;'>Average attendance</span>,
	 		  <span style='color: #1F78B4;'>Stadium capacity</span></b>, and<b> avg pct capacity for season</b>, by club, 2022-23 season.</b><br>
				Belgian clubs overall at about 60% capacity, with many above 70% and a few between 15%-30%. <br>
		                   Anderlecht total includes 1 match behind closed doors, thus 0 attendance."))

ggsave("images/plot_attendance_23_belgium.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)


## add interactivity



bel_scatter <- attend_scatter(bel_att_23_sum)
bel_scatter

ggsave("images/plot_att_scatter_23_belgium.jpg", width = 15, height = 8,
       units = "in", dpi = 300)

