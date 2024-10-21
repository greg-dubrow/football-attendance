# netherlands attendance figures...
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
ned_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "NED") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
  mutate(Home = ifelse(Home == "Sparta R'dam", "Sparta Rotterdam", Home)) %>%
  mutate(Away = ifelse(Away == "Sparta R'dam", "Sparta Rotterdam", Away)) %>%
  mutate(Home = ifelse(Home == "Go Ahead Eag", "Go Ahead Eagles", Home)) %>%
  mutate(Away = ifelse(Away == "Go Ahead Eag", "Go Ahead Eagles", Away)) %>%
  mutate(Venue = ifelse(Venue == "Johan Cruyff ArenA", "Johan Cruijff Arena", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Hitachi Capital Mobility Stadion", "Euroborg", Venue)) %>%
  mutate(Venue = ifelse(Venue == "De Geusselt", "Stadion De Geusselt", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Sparta-Stadion Het Kasteel", "Spartastadion Het Kasteel", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Seacon Stadion De Koel", "De Koel", Venue)) %>%
  mutate(Attendance = ifelse(Round == "Semi-finals" & Home == "Sparta Rotterdam", 16841, Attendance)) %>%
  ## correcting attendance for Groningen matches played w/ no fans due to fan behaviour
  mutate(Attendance = ifelse(Home == "Groningen" & is.na(Attendance), 0, Attendance)) %>%
  # delete any match not involving eredivisie team at home
  filter(!(Home %in% c("Almere City", "Emmen", "FC Eindhoven", "VVV-Venlo", "Heerenveen",
                             "NAC Breda", "Willem II", "MVV Maastricht"))) %>%
  select(Competition_Name:Round, match_week, Wk, Day:Referee)

ned_match_2023 %>%
  count(Home)

glimpse(ned_match_2023)


# get stadium info
ned_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_ned.rds") %>%
	mutate(stadium = ifelse(stadium == "De Kuip", "Stadion Feijenoord", stadium)) %>%
  mutate(stadium = ifelse(stadium == "Covebo Stadion – De Koel –", "De Koel", stadium))

glimpse(ned_stad_df)

ned_att_23 <- ned_match_2023 %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	left_join(ned_stad_df, by = c("match_stadium" = "stadium")) %>%
  mutate(match_pct_cap = match_attendance / capacity)

ned_att_23 %>%
  filter(is.na(city)) %>%
  view()

saveRDS(ned_att_23, file = "~/Data/r/football data projects/data/att_2023_ned.rds")

ned_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_ned.rds")

attend_sum(ned_att_23, "ned_att_23")
glimpse(ned_att_23_sum)

# plot using plotting df
# run function
ned_attplot <- attend_plot_comb(ned_att_23_sum)
ned_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
ned_attplot +
  plot_annotation(title = "<b>Dutch Eredivisie
  <span style='color: #8DA0CB;'>Average percent of capacity for season</span></b><i> (left bar chart)</i>,
  <b><span style='color: #FF7F00;'>Average attendance</span></b> and
  <b><span style='color: #1F78B4;'>Stadium capacity</span></b> (right bubble chart), by club, 2022-23 season.<br>
				Dutch clubs overall have lots of demand for tickets - league average is 88% capacity, with many clubs above 90% and only a few below 70%. <br>
		    Groningen total includes 3 match behind closed doors, thus 0 attendance.
        See scatterplot below for stadium capacity numbers obscured by overlapping bubbles.",
                  theme = theme(plot.title =
                                  ggtext::element_textbox_simple(
                                    size = 12, fill = "cornsilk",
                                    lineheight = 1.5,
                                    padding = margin(5.5, 5.5, 5.5, 2),
                                    margin = margin(0, 0, 5.5, 0))))

ggsave("images/plot_attendance_23_eredivisie.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

ned_scatter <- attend_scatter(ned_att_23_sum)
ned_scatter

ggsave("images/plot_att_scatter_23_eredivisie.jpg", width = 15, height = 8,
       units = "in", dpi = 300)

