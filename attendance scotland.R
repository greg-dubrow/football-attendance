# switzerland attendance figures...
library(tidyverse)
library(tidylog)
library(janitor)

# load if needed
library(ggtext)
library(ggrepel)
library(glue)
library(patchwork)


source("~/Data/r/basic functions.R")
options(scipen=10000)

# functions for summary df and plots
source("attend_functions.R")

# load attendance data
sco_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "SCO") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
  filter(!(Home == "Partick Thistle")) %>%
  mutate(Venue = ifelse(Venue == "Tony Macaroni Arena", "Almondvale Stadium", Venue)) %>%
  mutate(Venue = ifelse(Venue == "BBSP Stadium, Rugby Park", "Rugby Park", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Tynecastle Stadium", "Tynecastle Park", Venue)) %>%
  mutate(Venue = ifelse(Venue == "The Simple Digital Arena", "St. Mirren Park", Venue)) %>%
#  mutate(Venue = ifelse(Venue == "The Simple Digital Arena", "St Mirren Park", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Easter Road Stadium", "Easter Road", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Global Energy Stadium", "Victoria Park", Venue)) %>%
  select(Competition_Name:Round, match_week, Wk, Day:Referee)

sco_match_2023 %>%
  count(Home)

glimpse(sco_match_2023)

# get stadium info
sco_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_sco.rds") %>%
  mutate(stadium = ifelse(stadium == "St Mirren Park", "St. Mirren Park", stadium))


glimpse(sco_stad_df)

sco_att_23 <- sco_match_2023 %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	left_join(sco_stad_df, by = c("match_stadium" = "stadium")) %>%
  mutate(match_pct_cap = match_attendance / capacity)

glimpse(sco_att_23)

sco_att_23 %>%
  filter(is.na(city)) %>%
  distinct(match_home, match_stadium, .keep_all = TRUE) %>%
  view()

saveRDS(sco_att_23, file = "~/Data/r/football data projects/data/att_2023_sco.rds")

sco_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_sco.rds")

attend_sum(sco_att_23, "sco_att_23")
glimpse(sco_att_23_sum)

# adjust ross county down to 1 from 1.006 so it fits on scales
sco_att_23_sum <- sco_att_23_sum %>%
  mutate(capacity_pct_team = ifelse(team_name == "Ross County", 1, capacity_pct_team))

# sco_att_23_sum <- sco_att_23_sum %>%
#   fill(league, .direction = "down")

# plot using plotting df
# run function
sco_attplot <- attend_plot_comb(sco_att_23_sum)
sco_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
sco_attplot +
  plot_annotation(title = "<b>Scottish Premiership
  <span style='color: #8DA0CB;'>Average percent of capacity for season</span></b><i> (left bar chart)</i>,
  <b><span style='color: #FF7F00;'>Average attendance</span></b> and
  <b><span style='color: #1F78B4;'>Stadium capacity</span></b> (right bubble chart), by club, 2022-23 season.<br>
				Generally strong demand for tickets in Scotland, with league average around 82% capacity and more than half the teams
        at 80%. But a handful of teams aren't even at 50%",
                  theme = theme(plot.title =
                                  ggtext::element_textbox_simple(
                                    size = 12, fill = "cornsilk",
                                    lineheight = 1.5,
                                    padding = margin(5.5, 5.5, 5.5, 2),
                                    margin = margin(0, 0, 5.5, 0))))

ggsave("images/plot_attendance_23_sco.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

sco_scatter <- attend_scatter(sco_att_23_sum)
sco_scatter

ggsave("images/plot_att_scatter_23_sco.jpg", width = 15, height = 8,
       units = "in", dpi = 300)

