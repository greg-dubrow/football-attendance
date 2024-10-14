## loads match attendance numbers from fbref via worldfootballr

library(tidyverse)
library(tidylog)
library(worldfootballR)
source("~/Data/r/basic functions.R")

## attendances 2023 top euro leagues & mls
mls_match_2023 <- fb_match_results(country = "USA", gender = "M", season_end_year = 2023, tier = "1st")
epl_match_2023 <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2023, tier = "1st")
ger_match_2023 <- fb_match_results(country = "GER", gender = "M", season_end_year = 2023, tier = "1st")
ita_match_2023 <- fb_match_results(country = "ITA", gender = "M", season_end_year = 2023, tier = "1st")
spa_match_2023 <- fb_match_results(country = "ESP", gender = "M", season_end_year = 2023, tier = "1st")
fra_match_2023 <- fb_match_results(country = "FRA", gender = "M", season_end_year = 2023, tier = "1st")

den_match_2023 <- fb_match_results(country = "DEN", gender = "M", season_end_year = 2023, tier = "1st")
bel_match_2023 <- fb_match_results(country = "BEL", gender = "M", season_end_year = 2023, tier = "1st")
ned_match_2023 <- fb_match_results(country = "NED", gender = "M", season_end_year = 2023, tier = "1st")
por_match_2023 <- fb_match_results(country = "POR", gender = "M", season_end_year = 2023, tier = "1st")
swe_match_2023 <- fb_match_results(country = "SWE", gender = "M", season_end_year = 2023, tier = "1st")

efl_ch_match_2023 <- fb_match_results(country = "ENG", gender = "M", season_end_year = 2023, tier = "2nd")
spa2_match_2023 <- fb_match_results(country = "ESP", gender = "M", season_end_year = 2023, tier = "2nd")

glimpse(epl_match_2023)

euro_mls_match_2023 <- epl_match_2023 %>%
	bind_rows(efl_ch_match_2023, mls_match_2023, ger_match_2023, ita_match_2023, spa_match_2023, den_match_2023,
						fra_match_2023, bel_match_2023, ned_match_2023, por_match_2023, swe_match_2023)

euro_mls_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	rbind(spa2_match_2023)

euro_mls_match_2023 %>%
	count(Competition_Name)

saveRDS(euro_mls_match_2023, file = "~/Data/r/football data projects/data/euro_mls_match_2023.rds")


ita_season_2023 <- fb_season_team_stats("ITA", "M", 2023, "1st", "league_table")

glimpse(ita_season_2023)

# error on "league_table"
# view(fb_season_team_stats)

fra_match_2024 <- fb_match_results(country = "FRA", gender = "M", season_end_year = 2024, tier = "1st")

## gets messi data
mlsurls <-fb_league_urls(country = "USA", gender = "M", season_end_year = 2023, tier = "1st")
mlsurls2 <- fb_teams_urls("https://fbref.com/en/comps/22/2023/2023-Major-League-Soccer-Stats")
mlsurls2[14]
intermiamiurls <- fb_player_urls("https://fbref.com/en/squads/cb8b86a2/2023/Inter-Miami-Stats")
intermiamiurls[26]
messistats23 <- fb_player_match_logs("https://fbref.com/en/players/d70ce98e/Lionel-Messi",
																		 season_end_year = 2023, stat_type = "summary") %>%
	filter(Comp == "MLS")

messistats23 %>%
	count(Date)

eng_tables_23 <-
tm_matchday_table(country_name="England", start_year="2022", matchday=c(1:38))
