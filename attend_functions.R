
# Summary function --------------------------------------------------------

## df for plotting, with topline league and team figures
# different groupings in case teams play at multiple venues.
# only keep capacity_pct_team if multiple venues

attend_sum <- function(input_df, dfname = "NA") {
	dfout <- input_df %>%
		group_by(match_home, match_stadium) %>%
		mutate(attend_avg_team = round(mean(match_attendance), 0)) %>%
		mutate(attend_min_team = min(match_attendance)) %>%
		mutate(attend_max_team = max(match_attendance)) %>%
		mutate(attend_tot_team = sum(match_attendance)) %>%
		mutate(capacity_tot_team = sum(capacity)) %>%
		mutate(capacity_pct_team = attend_tot_team / capacity_tot_team) %>%
		ungroup() %>%
		# league figures
		add_row(tibble_row(match_home = "League Average")) %>%
		mutate(attend_tot_league = sum(match_attendance, na.rm = TRUE)) %>%
		mutate(capacity_tot_league = sum(capacity, na.rm = TRUE)) %>%
		mutate(capacity_pct_league = attend_tot_league / capacity_tot_league) %>%
		mutate(attend_avg_league = round(mean(match_attendance, na.rm = TRUE), 0)) %>%
		mutate(capacity_avg_league = round(mean(capacity, na.rm = TRUE), 0)) %>%
		mutate(attend_avg_team = ifelse(match_home == "League Average", attend_avg_league, attend_avg_team)) %>%
		mutate(capacity_pct_team = ifelse(match_home == "League Average", capacity_pct_league, capacity_pct_team)) %>%
		mutate(capacity = ifelse(match_home == "League Average", capacity_avg_league, capacity)) %>%
		# mutate(attend_med_league = round(median(match_attendance), 0)) %>%
		# mutate(attend_min_league = min(match_attendance)) %>%
		# mutate(attend_max_league = max(match_attendance)) %>%
		# mutate(capacity_med_league = round(median(capacity), 0)) %>%
		# mutate(capacity_min_league = min(capacity)) %>%
		mutate(capacity_max_league = max(capacity)) %>%
		# group_by(match_home) %>%
		# mutate(capacity_tot2 = sum(capacity)) %>%
		# mutate(attendance_tot2 = sum(match_attendance)) %>%
		# mutate(capacity_pct_team = attendance_tot2 / capacity_tot2) %>%
		# ungroup() %>%
		select(team_name = match_home, stadium_name = match_stadium, stadium_capacity = capacity,
					 attend_avg_team, attend_min_team, attend_max_team,
					 attend_tot_team, capacity_tot_team, capacity_pct_team,
					 attend_avg_league,
					 #        attend_med_league, attend_min_league, attend_max_league,
					 capacity_avg_league,
					 #.      , capacity_med_league, capacity_min_league,
					 capacity_max_league,
					 attend_tot_league,
					 capacity_tot_league, capacity_pct_league, league) %>%
		#, capacity_pct_team) %>%
		distinct(team_name, stadium_name, .keep_all = T)
	assign(str_c(dfname, "_sum"), dfout, envir=.GlobalEnv)
}

# Highlighting functions ---------------------------------------------------

## highlight function for plot labels
# from https://stackoverflow.com/questions/61733297/apply-bold-font-on-specific-axis-ticks
highlight = function(x, pat, color="black", family="") {
	ifelse(grepl(pat, x), glue::glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
}

highlight2 = function(x, pat, color="black", family="") {
	ifelse(grepl(pat, x), glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
}


# Plotting functions ------------------------------------------------------

## plotting function. run against plotting df, output as object, then add title
attend_plot1 <- function(plotdf) {
	plotdf %>%
		ggplot(aes(stadium_capacity, reorder(team_name, stadium_capacity))) +
		# points for avg attendace & capacity
		geom_point(aes(x=stadium_capacity, y= reorder(team_name, stadium_capacity)),
							 color="#1F78B4", size=10, alpha = .5 ) +
		geom_point(aes(x=attend_avg_team, y= reorder(team_name, stadium_capacity)),
							 color="#FF7F00", size=10, alpha = .5 ) +
		# data labels for points
		geom_text(data = plotdf %>% filter(capacity_pct_team < .95),
							aes(x = attend_avg_team,
									label = format(round(attend_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
							color = "black", size = 2.5) +
		geom_text(data = plotdf %>% filter(capacity_pct_team >= .95),
							aes(x = attend_avg_team,
									label = format(round(attend_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
							color = "black", size = 2.5, hjust = 1.5) +
		geom_text(aes(x = stadium_capacity,
									label = format(round(stadium_capacity, digits = 0),big.mark=",",scientific=FALSE)),
							color = "black", size = 2.5) +
		# line connecting the points.
		geom_segment(aes(x=attend_avg_team + 900 , xend=stadium_capacity - 900,
										 y=team_name, yend=team_name), color="lightgrey") +
		# sets league average in bold
		scale_y_discrete(labels= function(x) highlight(x, "League Average", "black")) +
		# text for avg season capacity
		geom_text(data = plotdf %>% filter(stadium_capacity < capacity_max_league & team_name != "League Average"),
							aes(x = stadium_capacity + 1100, y = team_name,
									label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
									hjust = -.02)) +
		geom_text(data = plotdf %>% filter(team_name == "League Average"),
							aes(x = stadium_capacity + 1100, y = team_name,
									label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
									hjust = -.02, fontface = "bold")) +
		scale_x_continuous(limits = c(min(plotdf$attend_avg_team),
																	max(plotdf$stadium_capacity + 3000)),
											 breaks = scales::pretty_breaks(6),
											 labels = scales::comma_format(big.mark = ',')) +
		# scale_x_continuous(limits = c(min(plotdf$stadium_capacity - 2000),
		# 															max(plotdf$stadium_capacity + 3000)),
		# 									 breaks = scales::pretty_breaks(6),
		# 									 labels = scales::comma_format(big.mark = ',')) +
		labs(x = "Stadium capacity", y = "",
				 subtitle = "*The further the red dot is to the left of the blue dot, the more average attendance is less than stadium capacity. Teams sorted by stadium capacity.*",
				 caption = "*Match attendance data from FBRef using worldfootballr package. Stadium capacity data from Wikipedia*") +
		theme_minimal() +
		theme(panel.grid = element_blank(),
					plot.title.position = "plot",
					plot.title = ggtext::element_textbox_simple(
						size = 12, fill = "cornsilk",
						lineheight = 1.5,
						padding = margin(5.5, 5.5, 5.5, 2),
						margin = margin(0, 0, 5.5, 0)),
					plot.subtitle = ggtext::element_markdown(size = 10),
					plot.caption = ggtext::element_markdown(),
					axis.text.x = ggtext::element_markdown(size = 10),
					axis.text.y = ggtext::element_markdown(size = 11))
}

### paired color palette
# [1] "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C"
# [7] "#FDBF6F" "#FF7F00"
