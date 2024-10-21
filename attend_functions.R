
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
	  fill(league, .direction = "down") %>%
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

## bubble and bar plots to combine
# bar chart barchart <-
attend_plot_comb <- function(plotdf) {

# build bar plot
  barplot <-
    plotdf %>%
    arrange(desc(capacity_pct_team)) %>%
    mutate(team_name = fct_reorder(team_name, capacity_pct_team)) %>%
    ggplot(aes(capacity_pct_team, team_name)) +
    geom_col(fill = "#8DA0CB") +
    geom_text(aes(capacity_pct_team, team_name,
                  label = scales::percent(round(capacity_pct_team, digits = 4), accuracy = 0.1)),
              hjust = 1.1,
              colour = "white") +
    scale_y_discrete(labels= function(x) highlight(x, "League Average", "black")) +
    scale_x_continuous(limits = c(0,1),
                       expand = expansion(mult = c(0, 0.01)),
                       labels = scales::percent_format()) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          plot.subtitle = ggtext::element_markdown(size = 10),
          plot.caption = ggtext::element_markdown(),
          axis.text.y = ggtext::element_markdown(size = 11))

# build bubble plot
  bubbleplot <-
    plotdf %>%
    arrange(desc(capacity_pct_team)) %>%
    mutate(team_name = fct_reorder(team_name, capacity_pct_team)) %>%
    ggplot(aes(stadium_capacity, capacity_pct_team)) +
    # points for avg attendance & capacity
    geom_point(aes(x=stadium_capacity, y= reorder(team_name, capacity_pct_team)),
               color="#1F78B4", size=15, alpha = .5 ) +
    geom_point(aes(x=attend_avg_team, y= reorder(team_name, capacity_pct_team)),
               color="#FF7F00", size=15, alpha = .5 ) +
    geom_text(aes(x = attend_avg_team, y= reorder(team_name, capacity_pct_team),
                  label = format(round(attend_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
              color = "black", size = 3.5) +
    geom_text(aes(x = stadium_capacity, y= reorder(team_name, capacity_pct_team),
                  label = format(round(stadium_capacity, digits = 0),big.mark=",",scientific=FALSE)),
              color = "black", size = 3.5) +
    # line connecting the points.
    geom_segment(aes(x=attend_avg_team + 2000 , xend=stadium_capacity - 2000,
                     y=team_name, yend=team_name), color="lightgrey") +
    scale_x_continuous(limits = c(0, max(plotdf$stadium_capacity + 500)),
                       expand = expansion(mult = c(0, 0.01)),
                       breaks = scales::pretty_breaks(6),
                       labels = scales::comma_format(big.mark = ',')) +
    labs(x = "", y = "",
         # subtitle = "*The further the orange dot (avg attendance) is to the left of the blue dot (stadium capacity),
         # the more average attendance is less than stadium capacity.*",
         caption = "*Match attendance data from FBRef using worldfootballr package. Stadium capacity data from Wikipedia*") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.y=element_blank(),
          plot.subtitle = ggtext::element_markdown(size = 10, hjust = 1),
          plot.caption = ggtext::element_markdown())

barplot + bubbleplot +
  plot_layout(widths = c(1.25, 2.25))
}


## plotting function. run against plotting df, output as object, then add title
attend_plot1 <- function(plotdf) {
	plotdf %>%
		ggplot(aes(stadium_capacity, reorder(team_name, stadium_capacity))) +
		# points for avg attendance & capacity
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
				 subtitle = "*The further the orange dot is to the left of the blue dot, the more average attendance is less than stadium capacity. Teams sorted by stadium capacity.*",
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

## plotting function as above but set up for plotly interactivity
attend_plot1_pl <- function(plotdf) {
  plotdf %>%
    ggplot(aes(stadium_capacity, reorder(team_name, stadium_capacity))) +
    # points for avg attendance & capacity
    geom_point(aes(x=stadium_capacity, y= reorder(team_name, stadium_capacity),
                   text = str_glue("Pct of capacity for season = {round(capacity_pct_team * 100, 1)} %")),
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
    # geom_text(data = plotdf %>% filter(stadium_capacity < capacity_max_league & team_name != "League Average"),
    # 					aes(x = stadium_capacity + 1100, y = team_name,
    # 							label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
    # 							hjust = -.02)) +
    # geom_text(data = plotdf %>% filter(team_name == "League Average"),
    # 					aes(x = stadium_capacity + 1100, y = team_name,
    # 							label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
    # 							hjust = -.02, fontface = "bold")) +
    scale_x_continuous(limits = c(min(plotdf$attend_avg_team),
                                  max(plotdf$stadium_capacity + 3000)),
                       breaks = scales::pretty_breaks(6),
                       labels = scales::comma_format(big.mark = ',')) +
    # scale_x_continuous(limits = c(min(plotdf$stadium_capacity - 2000),
    # 															max(plotdf$stadium_capacity + 3000)),
    # 									 breaks = scales::pretty_breaks(6),
    # 									 labels = scales::comma_format(big.mark = ',')) +
    labs(x = "Stadium capacity", y = "",
         subtitle = "*The further the orange dot is to the left of the blue dot, the more average attendance is less than stadium capacity. Teams sorted by stadium capacity.*",
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


## plotting function as above but with ggiraph interactivity
attend_plot1_i <- function(plotdf) {
  plotdf %>%
    ggplot(aes(stadium_capacity, reorder(team_name, stadium_capacity),
               tooltip = paste("Avg pct of capacity = ", round(capacity_pct_team *100, 1), "%"),
               data_id = capacity_pct_team)) +
    # points for avg attendace & capacity
    geom_point_interactive(aes(x=stadium_capacity, y= reorder(team_name, stadium_capacity)),
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
    # geom_text(data = plotdf %>% filter(stadium_capacity < capacity_max_league & team_name != "League Average"),
    #           aes(x = stadium_capacity + 1100, y = team_name,
    #               label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
    #               hjust = -.02)) +
    # geom_text(data = plotdf %>% filter(team_name == "League Average"),
    #           aes(x = stadium_capacity + 1100, y = team_name,
    #               label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
    #               hjust = -.02, fontface = "bold")) +
    scale_x_continuous(limits = c(min(plotdf$attend_avg_team),
                                  max(plotdf$stadium_capacity + 3000)),
                       breaks = scales::pretty_breaks(6),
                       labels = scales::comma_format(big.mark = ',')) +
    labs(x = "Stadium capacity", y = "",
         subtitle = "*The further the orange dot is to the left of the blue dot, the more average attendance is less than stadium capacity. Teams sorted by stadium capacity.*",
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

attend_scatter <- function(plotdf) {
  plotdf %>%
    ggplot(aes(x = stadium_capacity, y = capacity_pct_team)) +
    geom_point() +
    geom_smooth() +
    geom_text_repel(data = plotdf %>% filter(!team_name == "League Average"),
                    aes(label = team_name)) +
    geom_text_repel(data = plotdf %>% filter(team_name == "League Average"),
                    aes(label = team_name), fontface = "bold") +
    scale_x_continuous(labels = scales::comma_format(big.mark = ',')) +
    scale_y_continuous(limits = c(0,1), labels = scales::percent_format()) +
    labs(x = "Stadium Capacity", y = "Avg % of Capacity") +
    theme_minimal() +
    theme(panel.border = element_rect(
      color = 'grey', fill = NA, size = 1))
}

## read in and rbind attendance files
combine_rds_files <- function(directory) {
  # List all RDS files in the specified directory
  rds_files <- list.files(directory, pattern = "^att_2023.*\\.rds", full.names = TRUE)

  # Initialize an empty list to store dataframes
  df_list <- list()

  # Loop through each RDS file, read it, select columns, and store in the list
  for (file in rds_files) {
    data <- readRDS(file)
    # Select the desired columns
    selected_data <- data %>%
      select(league, match_date, match_home, match_away, match_stadium, capacity, match_attendance)
    df_list[[file]] <- selected_data
  }

  # Combine all dataframes in the list into one dataframe
  combined_df <- bind_rows(df_list)

  return(combined_df)
}



### paired color palette
# [1] "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C"
# [7] "#FDBF6F" "#FF7F00"
