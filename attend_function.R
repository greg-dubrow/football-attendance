## df for plotting, with topline league and team figures
# different groupings in case teams play at multiple venues.
# only keep capacity_pct_team if multiple venues


attend_sum <- function(input_df, dfname = "NA") {
	dfout <- input_df %>%
	# team figures
	group_by(match_home, match_stadium) %>%
	mutate(attend_avg_team = round(mean(match_attendance), 0)) %>%
	mutate(attend_min_team = min(match_attendance)) %>%
	mutate(attend_max_team = max(match_attendance)) %>%
	mutate(attend_tot_team = sum(match_attendance)) %>%
	mutate(capacity_tot_team = sum(capacity)) %>%
	mutate(capacity_pct_team = attend_tot_team / capacity_tot_team) %>%
	ungroup() %>%
	# league figures
	mutate(attend_avg_league = round(mean(match_attendance), 0)) %>%
	mutate(attend_med_league = round(median(match_attendance), 0)) %>%
	mutate(attend_min_league = min(match_attendance)) %>%
	mutate(attend_max_league = max(match_attendance)) %>%
	mutate(capacity_avg_league = round(mean(capacity), 0)) %>%
	mutate(capacity_med_league = round(median(capacity), 0)) %>%
	mutate(capacity_min_league = min(capacity)) %>%
	mutate(capacity_max_league = max(capacity)) %>%
	mutate(attend_tot_league = sum(match_attendance)) %>%
	mutate(capacity_tot_league = sum(capacity)) %>%
	mutate(capacity_pct_league = attend_tot_league / capacity_tot_league) %>%
	# group_by(match_home) %>%
	# mutate(capacity_tot2 = sum(capacity)) %>%
	# mutate(attendance_tot2 = sum(match_attendance)) %>%
	# mutate(capacity_pct_team = attendance_tot2 / capacity_tot2) %>%
	# ungroup() %>%
	select(team_name = match_home, stadium_name = match_stadium, stadium_capacity = capacity,
				 attend_avg_team, attend_min_team, attend_max_team,
				 attend_tot_team, capacity_tot_team, capacity_pct_team,
				 attend_avg_league, attend_med_league, attend_min_league, attend_max_league,
				 capacity_avg_league, capacity_med_league, capacity_min_league, capacity_max_league,
				 attend_tot_league, capacity_tot_league, capacity_pct_league, league) %>%
	distinct(team_name, stadium_name, .keep_all = T)
	assign(str_c(dfname, "_sum"), dfout, envir=.GlobalEnv)
}
