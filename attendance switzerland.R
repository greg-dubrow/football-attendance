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
sui_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "SUI") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
  filter(!(Home == "FC Stade Lausanne-Ouchy")) %>%
  mutate(Venue = ifelse(Venue == "swissporarena", "Swissporarena", Venue)) %>%
  mutate(Attendance = ifelse(Home == "Basel" & Away == "St. Gallen" & Date == "2022-10-01",
                             22630, Attendance)) %>%
  select(Competition_Name:Round, match_week, Wk, Day:Referee)

sui_match_2023 %>%
  count(Home)

glimpse(sui_match_2023)

# get stadium info
sui_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_sui.rds") %>%
  mutate(stadium = ifelse(stadium == "Stade Tourbillon", "Stade de Tourbillon", stadium)) %>%
  mutate(stadium = ifelse(stadium == "Stade de Suisse", "Stadion Wankdorf", stadium)) %>%
  mutate(stadium = ifelse(stadium == "Cornaredo Stadium", "Stadio di Cornaredo", stadium)) %>%
  mutate(stadium = ifelse(stadium == "kybunpark", "Kybunpark", stadium)) %>%
  mutate(stadium = ifelse(stadium == "Letzigrund", "Stadion Letzigrund", stadium))

glimpse(sui_stad_df)

sui_att_23 <- sui_match_2023 %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	left_join(sui_stad_df, by = c("match_stadium" = "stadium")) %>%
  mutate(match_pct_cap = match_attendance / capacity)

glimpse(sui_att_23)

sui_att_23 %>%
  filter(is.na(city)) %>%
  distinct(match_home, match_stadium, .keep_all = TRUE) %>%
  view()

saveRDS(sui_att_23, file = "~/Data/r/football data projects/data/att_2023_sui.rds")

sui_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_sui.rds")

attend_sum(sui_att_23, "sui_att_23")
glimpse(sui_att_23_sum)

# sui_att_23_sum <- sui_att_23_sum %>%
#   fill(league, .direction = "down")

# plot using plotting df
# run function
sui_attplot <- attend_plot_comb(sui_att_23_sum)
sui_attplot

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
sui_attplot +
  plot_annotation(title = "<b>Swiss Super League
  <span style='color: #8DA0CB;'>Average percent of capacity for season</span></b><i> (left bar chart)</i>,
  <b><span style='color: #FF7F00;'>Average attendance</span></b> and
  <b><span style='color: #1F78B4;'>Stadium capacity</span></b> (right bubble chart), by club, 2022-23 season.<br>
				Swiss clubs overall at about 60% capacity, with a wide variance in average capacity. <br>
				Grasshopper & Zurich both play in Stadion Letzigrund.
        FC Winterthur stadium capcity is 8,550, and average attendance is 8,228.",
                  theme = theme(plot.title =
                                  ggtext::element_textbox_simple(
                                    size = 12, fill = "cornsilk",
                                    lineheight = 1.5,
                                    padding = margin(5.5, 5.5, 5.5, 2),
                                    margin = margin(0, 0, 5.5, 0))))

ggsave("images/plot_attendance_23_sui.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)

sui_scatter <- attend_scatter(sui_att_23_sum)
sui_scatter

ggsave("images/plot_att_scatter_23_sui.jpg", width = 15, height = 8,
       units = "in", dpi = 300)

