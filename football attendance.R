## loads match attendance numbers from fbref via worldfootballr

library(tidyverse)
library(tidylog)
library(worldfootballR)

library(janitor)
library(rvest)
library(httr)
library(polite)
source("~/Data/r/basic functions.R")

## attendances 2023
epl_match_2023 <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2023, tier = "1st")
efl_ch_match_2023 <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2023, tier = "2nd")
mls_match_2023 <- fb_match_results(country = "USA", gender = "M", season_end_year = 2023, tier = "1st")
ger_match_2023 <- fb_match_results(country = "GER", gender = "M", season_end_year = 2023, tier = "1st")
ita_match_2023 <- fb_match_results(country = "ITA", gender = "M", season_end_year = 2023, tier = "1st")
spa_match_2023 <- fb_match_results(country = "ESP", gender = "M", season_end_year = 2023, tier = "1st")
den_match_2023 <- fb_match_results(country = "DEN", gender = "M", season_end_year = 2023, tier = "1st")
fra_match_2023 <- fb_match_results(country = "FRA", gender = "M", season_end_year = 2023, tier = "1st")
bel_match_2023 <- fb_match_results(country = "BEL", gender = "M", season_end_year = 2023, tier = "1st")
ned_match_2023 <- fb_match_results(country = "NED", gender = "M", season_end_year = 2023, tier = "1st")
por_match_2023 <- fb_match_results(country = "POR", gender = "M", season_end_year = 2023, tier = "1st")

glimpse(epl_match_2023)

euro_mls_match_2023 <- epl_match_2023 %>%
	bind_rows(efl_ch_match_2023, ger_match_2023, ita_match_2023, spa_match_2023, den_match_2023, fra_match_2023,
						bel_match_2023, ned_match_2023, por_match_2023)

saveRDS(euro_mls_match_2023, file = "~/Data/r/football data projects/data/euro_mls_match_2023.rds")


ita_season_2023 <- fb_season_team_stats("ITA", "M", 2023, "1st", "league_table")

glimpse(ita_season_2023)

error on "league_table"
view(fb_season_team_stats)

## stadium capacities
# mls via wikipedia
mls_url <- "https://en.wikipedia.org/wiki/List_of_Major_League_Soccer_stadiums"

mls_url_bow <- polite::bow(mls_url)
mls_url_bow

mls_stad_html <-
	polite::scrape(mls_url_bow) %>%  # scrape web page
	rvest::html_nodes("table.wikitable.sortable") %>% # pull out specific table
	rvest::html_table(fill = TRUE)

# add  cap ;
mls_stad_df <-
	mls_stad_html[[1]] %>%
	clean_names() %>%
	select(-image, -ref_s) %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = str_split(capacity, "\\(", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	mutate(capacity = ifelse(stadium == "Allianz Field", 19600, capacity)) %>%
	mutate(coordinates = ifelse(stadium == "Allianz Field", "44°57′10″N 93°9′54″W", coordinates)) %>%
	mutate(stadium = ifelse(stadium == "BC Place ‡", "BC Place Stadium", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Mercedes-Benz Stadium ‡", "Mercedes-Benz Stadium", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Chase Stadium", "DRV PNK Stadium", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Inter&Co Stadium", "Exploria Stadium", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Saputo Stadium", "Stade Saputo", stadium)) %>%
	add_row(tibble_row(stadium = "Citi Field", team = "New York City FC",
										 location = "Queens, New York", capacity = 41922)) %>%
	add_row(tibble_row(stadium = "Stanford Stadium", team = "San Jose Earthquakes",
										 location = "Palo Alto, California", capacity = 50424)) %>%
	add_row(tibble_row(stadium = "Rose Bowl", team = "Los Angeles Galaxy",
										 location = "Pasadena, California", capacity = 89702)) %>%
	add_row(tibble_row(stadium = "Stade Olympique", team = "CF Montréal",
										 location = "Montreal, Quebec", capacity = 56040)) %>%
	add_row(tibble_row(stadium = "Levi's Stadium", team = "San Jose Earthquakes",
										 location = "Santa Clara, California", capacity = 68500))

glimpse(mls_stad_df)


mls_stad_df %>%
#	filter(team == "San Jose Earthquakes") %>%
	count(stadium) %>%
	select(-n) %>%
	view()


## germany
ger_url <- "https://en.wikipedia.org/wiki/List_of_football_stadiums_in_Germany"

ger_url_bow <- polite::bow(ger_url)
ger_url_bow

ger_stad_html <-
	polite::scrape(ger_url_bow) %>%  # scrape web page
	rvest::html_nodes("table.wikitable.sortable") %>% # pull out specific table
	rvest::html_table(fill = TRUE)

ger_stad_df1 <-
	ger_stad_html[[1]] %>%
	clean_names() %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	select(stadium, city, state, capacity, team = tenants, opened)

glimpse(ger_stad_df1)

ger_stad_df2 <-
	ger_stad_html[[2]] %>%
	clean_names() %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	add_column(state = "") %>%
	add_column(opened = 0) %>%
	mutate(opened = as.integer(opened)) %>%
	select(stadium, city = location, state, capacity, team = tenants, opened)

glimpse(ger_stad_df2)

ger_stad_df <- ger_stad_df1 %>%
	rbind(ger_stad_df2)

### join results with stadium info ###

# mls
# for Charlotte FC s games w/ attendance > 38000, capacity is 74,867
# do per-match capacity
# for total season pct capacity, sum all matches capacity (potential capacity) &
 #   sum actual attendance to account for multiple venues
glimpse(mls_stad_df)
glimpse(mls_att23)

mls_att23 <- mls_match_2023 %>%
	mutate(league = "MLS") %>%
	select(league, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	mutate(match_stadium = ifelse(match_stadium == "Citypark", "CityPark", match_stadium)) %>%
	mutate(match_stadium = ifelse(
		match_home == "LAFC" & match_stadium == "BMO Field", "BMO Stadium", match_stadium)) %>%
	left_join(mls_stad_df, by = c("match_stadium" = "stadium")) %>%
	mutate(capacity = ifelse(
		(match_stadium == "Bank of America Stadium" & match_attendance > 38000),
		74867, capacity)) %>%
	mutate(capacity = ifelse(
		(match_stadium == "Mercedes-Benz Stadium" & match_attendance > 44000),
		72000, capacity)) %>%
	mutate(capacity = ifelse(
		(match_stadium == "Gillette Stadium" & match_attendance > 30000),
		65000, capacity)) %>%
	mutate(capacity = ifelse(
		(match_stadium == "Soldier Field" & match_attendance > 30000),
		63500, capacity)) %>%
	mutate(match_pct_cap = match_attendance / capacity)

glimpse(mls_att23)

mls_att23 %>%
	count(match_stadium) %>%
	select(-n) %>%
	view()

mls_att23 %>%
	filter(match_pct_cap > 1) %>%
	select(match_date, match_home, match_away, match_stadium, match_attendance,
				 capacity, match_pct_cap) %>%
	arrange(desc(match_pct_cap), match_stadium) %>%
	view()

mls_att23 %>%
	summarise(attend_avg = mean(match_attendance),
						atten_med = median(match_attendance),
						attend_min = min(match_attendance),
						attend_max = max(match_attendance),
						cap_avg = mean(capacity),
						cap_med = median(capacity))

mls_att23 %>%
	summarise(capacity_tot = sum(capacity),
						attendance_tot = sum(match_attendance),
						matches_tot = n())%>%
	mutate(capacity_pct_season = attendance_tot / capacity_tot) %>%
	mutate(att_per_match = attendance_tot / matches_tot)


mls_att23 %>%
	group_by(match_home, match_stadium) %>%
	mutate(capacity_tot = sum(capacity)) %>%
	mutate(attendance_tot = sum(match_attendance)) %>%
	mutate(capacity_pct_season = attendance_tot / capacity_tot) %>%
	mutate(attendance_avg = round(mean(match_attendance), 0)) %>%
	ungroup() %>%
	group_by(match_home) %>%
	mutate(capacity_tot2 = sum(capacity)) %>%
	mutate(attendance_tot2 = sum(match_attendance)) %>%
	mutate(capacity_pct_team = attendance_tot2 / capacity_tot2) %>%
	ungroup() %>%
	select(team_name = match_home, stadium_name = match_stadium,
				 capcity = capacity, attendance_avg,
				 capacity_tot, attendance_tot, capacity_pct_season, capacity_pct_team) %>%
	distinct(team_name, stadium_name, .keep_all = T) %>%
	view()


## germany
