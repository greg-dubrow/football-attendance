
# create league average as a team
epl_att_23_sum_alt <- epl_att_23 %>%
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
	distinct(team_name, stadium_name, .keep_all = T) %>%
	mutate(league = ifelse(team_name == "Crystal Palace", league, ""))


bold.labels <- ifelse(.$team_name == "League Average", yes = "bold", no = "plain")

highlight = function(x, pat, color="black", family="") {
	ifelse(grepl(pat, x), glue::glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
}

glimpse(epl_att_23_sum_alt)

epl_att_23_sum_alt %>%
	ggplot(aes(stadium_capacity, reorder(team_name, stadium_capacity))) +
	geom_point(aes(x=stadium_capacity, y= reorder(team_name, stadium_capacity)), color="#4E79A7", size=3 ) +
	geom_point(aes(x=attend_avg_team, y= reorder(team_name, stadium_capacity)), color="#A74E79", size=3 ) +
	scale_y_discrete(labels= function(x) highlight(x, "League Average", "black")) +
	geom_text(data = epl_att_23_sum %>% filter(stadium_capacity < capacity_max_league & team_name != "League Average"),
						aes(x = stadium_capacity + 1100, y = team_name,
								label = paste0("Pct capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = -.02)) +
	geom_text(data = epl_att_23_sum %>% filter(team_name == "League Average"),
						aes(x = stadium_capacity + 1100, y = team_name,
								label = paste0("Pct capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = -.02, fontface = "bold")) +
	geom_text(data = epl_att_23_sum %>% filter(stadium_capacity == capacity_max_league),
						aes(x = stadium_capacity - 10000, y = team_name,
								label = paste0("Pct capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = .04)) +
#	scale_x_continuous(limits = c(10000, 75000), breaks = scales::pretty_breaks(6)) +
	scale_x_continuous(limits = c(min(epl_att_23_sum$stadium_capacity - 1000),
																max(epl_att_23_sum$stadium_capacity + 3000)),
																breaks = scales::pretty_breaks(6)) +
	# ggtitle(glue::glue("<b> EPL <span style='color: #4E79A7;'>Stadium capacity</span> and
	#  		  <span style='color: #A74E79;'>Average attendance</span> by club, 2022-23 season.</b><br>
	# 			All Premier League clubs were over 90% capacity, and all but two, Bournemouth and Southhampton, filled more than 95% of seats.")) +
	labs(x = "Stadium capacity", y = "",
	title = glue::glue("<b>{unique(epl_att_23_sum_alt$league)}, <span style='color: #4E79A7;'>Stadium capacity</span> and
	 		  <span style='color: #A74E79;'>Average attendance</span> by club, 2022-23 season.</b><br>
				All Premier League clubs were over 90% capacity, and all but two, Bournemouth and Southhampton, filled more than 95% of seats."),
			 subtitle = "*If the red dot is to the left of the blue dot, average attendance is less than stadium capacity. Teams sorted by stadium capacity.*",
			 caption = "*Match attendance data from FBRef using worldfootballr package. Stadium capacity data from Wikipedia*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				plot.title.position = "plot",
				plot.title = element_markdown(),
				# plot.title = element_textbox_simple(
				# 	size = 13, fill = "cornsilk",
				#  	lineheight = 1.5,
				#  	padding = margin(5.5, 5.5, 5.5, 5.5),
				#  	margin = margin(0, 0, 5.5, 0)),
		plot.subtitle = element_markdown(),
		plot.caption = element_markdown(),
		axis.text.y = element_markdown(size = 11))

print(element_textbox_simple()) %>%
	view()

attend_plot1 <- function(plotdf) {
	plotdf %>%
	ggplot(aes(stadium_capacity, reorder(team_name, stadium_capacity))) +
	geom_point(aes(x=stadium_capacity, y= reorder(team_name, stadium_capacity)), color="#4E79A7", size=3 ) +
	geom_point(aes(x=attend_avg_team, y= reorder(team_name, stadium_capacity)), color="#A74E79", size=3 ) +
	scale_y_discrete(labels= function(x) highlight(x, "League Average", "black")) +
	geom_text(data = plotdf %>% filter(stadium_capacity < capacity_max_league & team_name != "League Average"),
						aes(x = stadium_capacity + 1100, y = team_name,
								label = paste0("Pct capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = -.02)) +
	geom_text(data = plotdf %>% filter(team_name == "League Average"),
						aes(x = stadium_capacity + 1100, y = team_name,
								label = paste0("Pct capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = -.02, fontface = "bold")) +
	geom_text(data = plotdf %>% filter(stadium_capacity == capacity_max_league),
						aes(x = stadium_capacity - 10000, y = team_name,
								label = paste0("Pct capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = .04)) +
	scale_x_continuous(limits = c(min(plotdf$stadium_capacity - 1000),
																max(plotdf$stadium_capacity + 3000)),
										 breaks = scales::pretty_breaks(6)) +
	labs(x = "Stadium capacity", y = "",
			 subtitle = "*If the red dot is to the left of the blue dot, average attendance is less than stadium capacity. Teams sorted by stadium capacity.*",
			 caption = "*Match attendance data from FBRef using worldfootballr package. Stadium capacity data from Wikipedia*") +
	theme_minimal() +
	theme(panel.grid = element_blank(),
				plot.title.position = "plot",
				plot.title = element_textbox_simple(
					size = 13, fill = "cornsilk",
					lineheight = 1.5,
					padding = margin(5.5, 5.5, 5.5, 5.5),
					margin = margin(0, 0, 5.5, 0)),
				plot.subtitle = element_markdown(),
				plot.caption = element_markdown(),
				axis.text.y = element_markdown(size = 11))
}

epl_attplot <- attend_plot1(epl_att_23_sum_alt)
epl_attplot

epl_attplot +
labs(
title = glue::glue("<b>{unique(epl_att_23_sum$league)}, <span style='color: #4E79A7;'>Stadium capacity</span> and
	 		  <span style='color: #A74E79;'>Average attendance</span> by club, 2022-23 season.</b><br>
				All Premier League clubs were over 90% capacity, and all but two, Bournemouth and Southhampton,
									 filled more than 95% of seats."))

####
#buble chart labels in bubble

fra_att_23_sum %>%
	ggplot(aes(stadium_capacity, reorder(team_name, stadium_capacity))) +
	geom_point(aes(x=stadium_capacity, y= reorder(team_name, stadium_capacity)),
						 color="#4E79A7", size=10, alpha = .5)+
	geom_point(aes(x=attend_avg_team, y= reorder(team_name, stadium_capacity)),
						 color="#A74E79", size=10, alpha = .5 ) +
	geom_segment(aes(x=attend_avg_team , xend=stadium_capacity,
									 y=team_name, yend=team_name), color="grey") +
	theme_minimal() +
	geom_text(data = fra_att_23_sum %>% filter(capacity_pct_team < .95),
		aes(x = attend_avg_team,
								label = format(round(attend_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
						color = "black", size = 3) +
	geom_text(data = fra_att_23_sum %>% filter(capacity_pct_team >= .95),
		aes(x = attend_avg_team,
								label = format(round(attend_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
						color = "black", size = 3, hjust = 1.5) +
	geom_text(aes(x = stadium_capacity,
								label = format(round(stadium_capacity, digits = 0),big.mark=",",scientific=FALSE)),
						color = "black", size = 3) +
	scale_y_discrete(labels= function(x) highlight(x, "League Average", "black")) +
	geom_text(data = fra_att_23_sum %>% filter(stadium_capacity < capacity_max_league & team_name != "League Average"),
						aes(x = stadium_capacity + 1100, y = team_name,
								label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = -.02)) +
	geom_text(data = fra_att_23_sum %>% filter(team_name == "League Average"),
						aes(x = stadium_capacity + 1100, y = team_name,
								label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = -.02, fontface = "bold")) +
	scale_x_continuous(limits = c(min(fra_att_23_sum$attend_avg_team),
																max(fra_att_23_sum$stadium_capacity + 3000)),
										 breaks = scales::pretty_breaks(6),
										 labels = scales::comma_format(big.mark = ','))
