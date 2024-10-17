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
por_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "POR") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
  # mutate(Home = ifelse(Home == "Sparta R'dam", "Sparta Rotterdam", Home)) %>%
  # mutate(Away = ifelse(Away == "Sparta R'dam", "Sparta Rotterdam", Away)) %>%
  # mutate(Home = ifelse(Home == "Go Ahead Eag", "Go Ahead Eagles", Home)) %>%
  # mutate(Away = ifelse(Away == "Go Ahead Eag", "Go Ahead Eagles", Away)) %>%
  mutate(Venue = ifelse(Venue == "Estádio da Luz (1954)", "Estádio do Sport Lisboa e Benfica", Venue)) %>%
  # mutate(Venue = ifelse(Venue == "Hitachi Capital Mobility Stadion", "Euroborg", Venue)) %>%
  # mutate(Venue = ifelse(Venue == "De Geusselt", "Stadion De Geusselt", Venue)) %>%
  # mutate(Venue = ifelse(Venue == "Sparta-Stadion Het Kasteel", "Spartastadion Het Kasteel", Venue)) %>%
  # mutate(Venue = ifelse(Venue == "Seacon Stadion De Koel", "De Koel", Venue)) %>%
  # mutate(Attendance = ifelse(Round == "Semi-finals" & Home == "Sparta Rotterdam", 16841, Attendance)) %>%
  ## correcting attendance for Groningen matches played w/ no fans due to fan behaviour
#  mutate(Attendance = ifelse(Home == "Groningen" & is.na(Attendance), 0, Attendance)) %>%
  # delete any match not involving eredivisie team at home
  filter(!(Home == "Estrela")) %>%
  select(Competition_Name:Round, match_week, Wk, Day:Referee)

por_match_2023 %>%
  count(Home)

glimpse(por_match_2023)


# get stadium info
por_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_por.rds")%>%
	mutate(stadium = ifelse(stadium == "Estádio da Luz", "Estádio do Sport Lisboa e Benfica", stadium)) %>%
  mutate(stadium = ifelse(stadium == "Covebo Stadion – De Koel –", "De Koel", stadium))

glimpse(por_stad_df)

por_att_23 <- por_match_2023 %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	left_join(por_stad_df, by = c("match_stadium" = "stadium")) %>%
  mutate(match_pct_cap = match_attendance / capacity)

por_att_23 %>%
  filter(is.na(city)) %>%
  view()

saveRDS(por_att_23, file = "~/Data/r/football data projects/data/att_2023_ned.rds")

por_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_ned.rds")

attend_sum(por_att_23, "por_att_23")
glimpse(por_att_23_sum)

# plot using plotting df
# run function
por_attplot <- attend_plot1(por_att_23_sum)
por_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
por_attplot +
  geom_text(data = por_att_23_sum %>% filter(stadium_capacity == capacity_max_league & team_name == "Ajax"),
            aes(x = stadium_capacity - 23000, y = team_name,
                label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
                hjust = -1)) +
  geom_text(data = por_att_23_sum %>% filter(stadium_capacity == capacity_max_league & team_name == "Feyenoord"),
            aes(x = stadium_capacity - 30000, y = team_name,
                label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
                hjust = -1)) +
  labs(
		title = glue::glue("<b>Dutch Eredivisie <span style='color: #FF7F00;'>Average attendance</span>,
	 		  <span style='color: #1F78B4;'>Stadium capacity</span></b>, and<b> avg pct capacity for season</b>, by club, 2022-23 season.</b><br>
				Dutch clubs overall at about 88% capacity, with many above 90% and only a few below 70%. <br>
		                   Groningen total includes 3 match behind closed doors, thus 0 attendance."))

ggsave("images/plot_attendance_23_eredivisie.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

por_scatter <- attend_scatter(por_att_23_sum)
por_scatter

ggsave("images/plot_att_scatter_23_eredivisie.jpg", width = 15, height = 8,
       units = "in", dpi = 300)

