# denmark attendance figures...
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
den_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "DEN") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
	# fix mixup from FBRef w/ championship & relegation rounds
	rename(round2 = Round) %>%
	mutate(Round = case_when(round2 == "Relegation round" ~ "Championship round",
														round2 == "Championship round" ~ "Relegation round",
														TRUE ~ round2)) %>%
	select(-round2) %>%
	select(Competition_Name:Season_End_Year, Round, match_week, Wk, Day:Referee) %>%
	mutate(Venue = ifelse(Venue == "CASA Arena Horsens", "Nordstern Arena Horsens", Venue)) %>%
	mutate(Venue = ifelse(Venue == "Parken", "Parken Stadium", Venue))

glimpse(den_match_2023)

den_match_2023 %>%
	count(Venue)

# get stadium info
den_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_den.rds")

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

## average capacity by round - regular season, relegation, championship
# did teams in championship half do better relative to regular season than teams in relegation half

glimpse(superligadk_att_23)

superligadk_att_23 %>%
	mutate(round = case_when(round == "Regular season" ~ "1 - Regular season",
													 round == "Championship round" ~ "2 - Championship round",
													 round == "Relegation round" ~ "2 - Relegation round",
													 round == "European play-off match" ~ "3 - European play-off match")) %>%
	arrange(match_home, round) %>%
	group_by(round) %>%
	summarise(attend_avg_round = round(mean(match_attendance), 0)) %>%
	mutate(round_diff = attend_avg_round - lag(attend_avg_round, default = first(attend_avg_round))) %>%
	ungroup() %>%
	view()

superligadk_att_23 %>%
  count(match_home)

superligadk_att_23 %>%
	mutate(round = case_when(round == "Regular season" ~ "1 - Regular season",
													 round == "Championship round" ~ "2 - Championship round",
													 round == "Relegation round" ~ "2 - Relegation round",
													 round == "European play-off match" ~ "3 - European play-off match")) %>%
	arrange(match_home, round) %>%
	group_by(match_home, round) %>%
	summarise(attend_avg_team = round(mean(match_attendance), 0)) %>%
	mutate(round_diff = attend_avg_team - lag(attend_avg_team, default = first(attend_avg_team))) %>%
	ungroup() %>%
	view()

## slopegraph
# data for graph
den_att_rounds <-
superligadk_att_23 %>%
  mutate(round = case_when(round == "Regular season" ~ "1 - Regular season",
                           round == "Championship round" ~ "2 - Championship round",
                           round == "Relegation round" ~ "2 - Relegation round",
                           round == "European play-off match" ~ "3 - European play-off match")) %>%
  arrange(match_home, round) %>%
  group_by(match_home, round) %>%
  summarise(attend_avg_team = round(mean(match_attendance), 0)) %>%
  mutate(round_diff = attend_avg_team - lag(attend_avg_team, default = first(attend_avg_team))) %>%
  filter(round != "3 - European play-off match") %>%
  mutate(round2_grp = ifelse(match_home %in% c("AGF", "Brøndby", "FC Copenhagen",
  "Nordsjælland", "Randers", "Viborg"),  "Championship round", "Relegation round")) %>%
  ungroup() %>%
  ## add final league place
  mutate(table_final = case_when(match_home == "FC Copenhagen" ~ 1, match_home == "Nordsjælland" ~ 2,
                                 match_home == "AGF" ~ 3, match_home == "Viborg" ~ 4,
                                 match_home == "Brøndby" ~ 5, match_home == "Randers" ~ 6,
                                 match_home == "Midtjylland" ~ 7, match_home == "Odense" ~ 8,
                                 match_home == "Silkeborg" ~ 9, match_home == "Lyngby" ~ 10,
                                 match_home == "Horsens" ~ 11, match_home == "Aalborg" ~ 12)) %>%
  select(match_home, table_final, round2_grp, round, attend_avg_team, round_diff)

den_att_rounds %>%
#  filter(round2_grp == "Championship round") %>%
  ggplot(aes(x = round, y = attend_avg_team, group = match_home)) +
  geom_line(aes(color = match_home, alpha = 1), size = 2) +
  geom_point(aes(color = match_home, alpha = 1), size = 4) +
  geom_text_repel(data = den_att_rounds %>% filter(
    round == "1 - Regular season"),
    aes(label = match_home),
    hjust = 1.35, size = 3) +
  geom_text_repel(data = den_att_rounds %>% filter(
    round %in% c("2 - Championship round", "2 - Relegation round")),
    aes(label = paste0("Final place ", table_final)),
    hjust = -.35, size = 3) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ',')) +
  facet_wrap(~ round2_grp, scales = "free") +
  labs(x = "", y = "",
    title = "Does something to play for (or not) impact attendance after the Superliga round split?",
    subtitle = "Teams with chances to win the league or in jeopardy for relegation (places 11 & 12) had increased attendance,
    <br> except for Horsens who were relegated and saw a decline for next round.") +
  theme_minimal() +
  theme(legend.position = "none", panel.border = element_blank(),
        plot.subtitle = element_markdown())

ggsave("images/plot_attendance_23_byrounds_superligadk.jpg", width = 15, height = 8,
       units = "in", dpi = 300)


attend_sum(superligadk_att_23, "superligadk_att_23")
glimpse(superligadk_att_23_sum)

# export to xlsx
write_csv(superligadk_att_23_sum, file = "superligadk_att_23_sum.csv")

# plot using plotting df
# run function
superligadk_attplot <- attend_plot_comb(superligadk_att_23_sum)
superligadk_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
superligadk_attplot +
  plot_annotation(title = "<b> Danish Superliga
  <span style='color: #8DA0CB;'>Average percent of capacity for season</span></b><i> (left bar chart)</i>,
  <b><span style='color: #FF7F00;'>Average attendance</span></b> and
  <b><span style='color: #1F78B4;'>Stadium capacity</span></b> (right bubble chart), by club, 2022-23 season.<br>
				Superliga overall at about 2/3 capacity, only Midtjylland & FC København above 70% capacity.
        Unlike some other small leagues, not much variance between the top and bottom capacity percentages.<br>
        Many clubs have standing spaces included in capacity figure.",
theme = theme(plot.title =
                                  ggtext::element_textbox_simple(
                                    size = 12, fill = "cornsilk",
                                    lineheight = 1.5,
                                    padding = margin(5.5, 5.5, 5.5, 2),
                                    margin = margin(0, 0, 5.5, 0))))

ggsave("images/plot_attendance_23_superligadk.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

den_scatter <- attend_scatter(superligadk_att_23_sum)
den_scatter

ggsave("images/plot_att_scatter_23_superligadk.jpg", width = 15, height = 8,
       units = "in", dpi = 300)

