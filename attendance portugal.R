# portugal attendance figures...
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
por_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "POR") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
  # mutate(Home = ifelse(Home == "Sparta R'dam", "Sparta Rotterdam", Home)) %>%
  # mutate(Away = ifelse(Away == "Sparta R'dam", "Sparta Rotterdam", Away)) %>%
  # mutate(Home = ifelse(Home == "Go Ahead Eag", "Go Ahead Eagles", Home)) %>%
  # mutate(Away = ifelse(Away == "Go Ahead Eag", "Go Ahead Eagles", Away)) %>%
  mutate(Venue = ifelse(Venue == "Estádio da Luz (1954)", "Estádio do Sport Lisboa e Benfica", Venue)) %>%
  mutate(Venue = ifelse(Home == "Casa Pia", "Estádio Municipal de Rio Maior", Venue)) %>%
  mutate(Venue = ifelse(Home == "Chaves", "Estádio Municipal de Chaves", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Estádio dos Barreiros", "Estádio do Marítimo", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Estádio do Portimonense SC", "Estádio Municipal de Portimão", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Estádio Do Dragão", "Estádio do Dragão", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Estádio do Rio Ave Futebol Clube", "Estádio dos Arcos", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Estádio Dom Afonso Henriques", "Estádio D. Afonso Henriques", Venue)) %>%
  mutate(Venue = ifelse(Venue == "Estádio Do Vizela", "Estádio do Futebol Clube de Vizela", Venue)) %>%
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
  mutate(stadium = ifelse(stadium == "Estádio da Mata Real", "Estádio da Capital do Móvel", stadium)) %>%
  mutate(stadium = ifelse(stadium == "Estádio do Portimonense Sporting Clube",
                          "Estádio Municipal de Portimão", stadium))

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

saveRDS(por_att_23, file = "~/Data/r/football data projects/data/att_2023_por.rds")

por_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_por.rds")

attend_sum(por_att_23, "por_att_23")
glimpse(por_att_23_sum)

# plot using plotting df
# run function
por_attplot <- attend_plot_comb(por_att_23_sum)
por_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
por_attplot +
  plot_annotation(title = "<b>Portugese Primeira Liga
  <span style='color: #8DA0CB;'>Average percent of capacity for season</span></b><i> (left bar chart)</i>,
  <b><span style='color: #FF7F00;'>Average attendance</span></b> and
  <b><span style='color: #1F78B4;'>Stadium capacity</span></b> (right bubble chart), by club, 2022-23 season.<br>
				Portugese clubs overall at about 60% capacity, only two teams above 80% and a number of clubs below 40%.",
                  theme = theme(plot.title =
                                  ggtext::element_textbox_simple(
                                    size = 12, fill = "cornsilk",
                                    lineheight = 1.5,
                                    padding = margin(5.5, 5.5, 5.5, 2),
                                    margin = margin(0, 0, 5.5, 0))))

ggsave("images/plot_attendance_23_portugal.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

por_scatter <- attend_scatter(por_att_23_sum)
por_scatter

ggsave("images/plot_att_scatter_23_portugal.jpg", width = 15, height = 8,
       units = "in", dpi = 300)

