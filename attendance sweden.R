# sweden attendance figures...
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
swe_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "SWE") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
  mutate(Venue = ifelse(Venue == "Nya Gamla Ullevi", "Gamla Ullevi", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Strandvallen A-Plan", "Strandvallen", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Varberg Energi Arena", "Påskbergsvallen", Venue)) %>%
  select(Competition_Name:Round, match_week, Wk, Day:Referee)

swe_match_2023 %>%
  count(Home)

glimpse(swe_match_2023)

# get stadium info
swe_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_swe.rds")

glimpse(swe_stad_df)

swe_att_23 <- swe_match_2023 %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	left_join(swe_stad_df, by = c("match_stadium" = "stadium")) %>%
  mutate(match_pct_cap = match_attendance / capacity)

glimpse(swe_att_23)

swe_att_23 %>%
  filter(is.na(city)) %>%
  distinct(match_home, match_stadium, .keep_all = TRUE) %>%
  view()

saveRDS(swe_att_23, file = "~/Data/r/football data projects/data/att_2023_swe.rds")

swe_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_swe.rds")

attend_sum(swe_att_23, "swe_att_23")
glimpse(swe_att_23_sum)


# plot using plotting df
# run function
swe_attplot <- attend_plot1(swe_att_23_sum)
swe_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
swe_attplot +
  geom_text(data = swe_att_23_sum %>% filter(stadium_capacity == capacity_max_league & team_name == "AIK Stockholm"),
            aes(x = stadium_capacity - 46000, y = team_name,
                label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
                hjust = -1)) +
  labs(
		title = glue::glue("<b>Swedish Allsvenskan <span style='color: #FF7F00;'>Average attendance</span>,
	 		  <span style='color: #1F78B4;'>Stadium capacity</span></b>, and<b> avg pct capacity for season</b>, by club, 2022-23 season.</b><br>
				Swedish clubs overall at about 64% capacity, with a wide variance in average capacity. Malmö & Göteborg are the only clubs above 80%. <br>
		                   Djurgårdens IF & Hammarby both play in Tele2 Arena."))

ggsave("images/plot_attendance_23_swe.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

swe_scatter <- attend_scatter(swe_att_23_sum)
swe_scatter

ggsave("images/plot_att_scatter_23_swe.jpg", width = 15, height = 8,
       units = "in", dpi = 300)

