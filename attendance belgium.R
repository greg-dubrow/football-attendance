# belgium attendance figures...
library(tidyverse)
library(tidylog)
library(janitor)

# load if needed
library(ggtext)
library(ggrepel)
library(glue)
library(ggiraph)
library(patchwork)

source("~/Data/r/basic functions.R")
options(scipen=10000)

# functions for summary df and plots
source("attend_functions.R")

# load attendance data
bel_match_2023 <- readRDS("~/Data/r/football data projects/data/euro_mls_match_2023.rds") %>%
	filter(Country == "BEL") %>%
	mutate(wk_n = as.numeric(Wk)) %>%
	mutate(match_week = ifelse(between(wk_n, 1, 9), paste0("0", Wk), Wk)) %>%
  mutate(Home = ifelse(Home == "Mechelen", "KV Mechelen", Home)) %>%
  mutate(Away = ifelse(Away == "Mechelen", "KV Mechelen", Away)) %>%
  mutate(Home = ifelse(Home == "Union SG", "Union Saint-Gilloise", Home)) %>%
  mutate(Away = ifelse(Away == "Union SG", "Union Saint-Gilloise", Away)) %>%
  mutate(Venue = ifelse(Venue == "GHELAMCO-arena", "Planet Group Arena", Venue)) %>%
  mutate(Venue = ifelse(Venue == "King Power at Den Dreef Stadion", "Den Dreef", Venue)) %>%
  select(Competition_Name:Round, match_week, Wk, Day:Referee)

glimpse(bel_match_2023)


# get stadium info
bel_stad_df <- readRDS("~/Data/r/football data projects/data/stadiums_bel.rds") %>%
	mutate(stadium = ifelse(stadium == "Jan Breydel Stadium", "Jan Breydelstadion", stadium)) %>%
  mutate(stadium = ifelse(stadium == "Achter de Kazerne", "AFAS-stadion Achter de Kazerne", stadium)) %>%
  mutate(stadium = ifelse(stadium == "Stade Joseph Marien", "Stade Joseph Mariën", stadium)) %>%
  mutate(stadium = ifelse(stadium == "Kehrweg Stadion", "Stadion am Kehrweg", stadium))

glimpse(bel_stad_df)

den_stad_df %>%
	count(stadium) %>%
	view()

bel_att_23 <- bel_match_2023 %>%
	select(league = Competition_Name, season = Season_End_Year, round = Round,
				 match_date = Date, match_day = Day, match_time = Time,
				 match_home = Home, match_away = Away,
				 match_stadium = Venue, match_attendance = Attendance,
				 HomeGoals, Home_xG, AwayGoals, Away_xG, Referee) %>%
	left_join(bel_stad_df, by = c("match_stadium" = "stadium")) %>%
  mutate(match_attendance = ifelse(match_home == "Anderlecht" & match_date == "2023-01-18",
                                   0, match_attendance)) %>%
  mutate(match_pct_cap = match_attendance / capacity)

bel_att_23 %>%
  filter(match_home == "Cercle Brugge") %>%
  view()

saveRDS(bel_att_23, file = "~/Data/r/football data projects/data/att_2023_bel.rds")

bel_att_23 <- readRDS("~/Data/r/football data projects/data/att_2023_bel.rds")

attend_sum(bel_att_23, "bel_att_23")
glimpse(bel_att_23_sum)

# plot using plotting df
# run function
bel_attplot <- attend_plot1(bel_att_23_sum)
bel_attplot

bel_attplot_i1 <-
plotly::ggplotly(bel_attplot, tooltip = "text")


htmlwidgets::saveWidget(bel_attplot_i1, file = "images/plot_attendance_23_belgium_i.html")

# add title after reviewing plot for story highlights. CHANGE LEAGUE NAME!!
bel_attplot +
	geom_text(data = bel_att_23_sum %>% filter(stadium_capacity == capacity_max_league & team_name == "Club Brugge"),
						aes(x = stadium_capacity - 16000, y = team_name,
								label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
								hjust = .04, vjust = .02)) +
  geom_text(data = bel_att_23_sum %>% filter(stadium_capacity == capacity_max_league & team_name == "Cercle Brugge"),
            aes(x = stadium_capacity - 16000, y = team_name,
                label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
                hjust = .04, vjust = -.1)) +
  labs(
		title = glue::glue("<b>Belgian Jupiler League <span style='color: #FF7F00;'>Average attendance</span>,
	 		  <span style='color: #1F78B4;'>Stadium capacity</span></b>, and<b> avg pct capacity for season</b>, by club, 2022-23 season.</b><br>
				Belgian clubs overall at about 60% capacity, with many above 70% and a few between 15%-30%. <br>
		                   Anderlecht total includes 1 match behind closed doors, thus 0 attendance."))

ggsave("images/plot_attendance_23_belgium.jpg", width = 15, height = 8,
			 units = "in", dpi = 300)


## add interactivity
bel_attplot_i <- attend_plot1_i(bel_att_23_sum)
bel_attplot_i <- girafe(ggobj = bel_attplot_i)
bel_attplot_i <- girafe_options()
bel_attplot_i
htmltools::save_html(bel_attplot_i, "images/plot_attendance_23_belgium_i.html")


htmlwidgets::saveWidget(bel_attplot_i, "images/plot_attendance_23_belgium_i.html",
                        selfcontained = TRUE)


bel_scatter <- attend_scatter(bel_att_23_sum)
bel_scatter

ggsave("images/plot_att_scatter_23_belgium.jpg", width = 15, height = 8,
       units = "in", dpi = 300)


## alternate plot with arranging by capacity
bel_att_23_sum %>%
  arrange(desc(capacity_pct_team)) %>%
  mutate(team_name = fct_reorder(team_name, capacity_pct_team)) %>%
    ggplot(aes(stadium_capacity, capacity_pct_team)) +
    # points for avg attendance & capacity
    geom_point(aes(x=stadium_capacity, y= reorder(team_name, capacity_pct_team)),
               color="#1F78B4", size=10, alpha = .5 ) +
    geom_point(aes(x=attend_avg_team, y= reorder(team_name, capacity_pct_team)),
               color="#FF7F00", size=10, alpha = .5 ) +
    # data labels for points
    geom_text(data = bel_att_23_sum %>% filter(capacity_pct_team < .95),
              aes(x = attend_avg_team, y= reorder(team_name, capacity_pct_team),
                  label = format(round(attend_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
              color = "black", size = 2.5) +
    geom_text(data = bel_att_23_sum %>% filter(capacity_pct_team >= .95),
              aes(x = attend_avg_team, y= reorder(team_name, capacity_pct_team),
                  label = format(round(attend_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
              color = "black", size = 2.5, hjust = 1.5) +
    geom_text(aes(x = stadium_capacity, y= reorder(team_name, capacity_pct_team),
                  label = format(round(stadium_capacity, digits = 0),big.mark=",",scientific=FALSE)),
              color = "black", size = 2.5) +
    # line connecting the points.
    geom_segment(aes(x=attend_avg_team + 900 , xend=stadium_capacity - 900,
                     y=team_name, yend=team_name), color="lightgrey") +
    # sets league average in bold
    scale_y_discrete(labels= function(x) highlight(x, "League Average", "black")) +
    # text for avg season capacity
    geom_text(data = bel_att_23_sum %>% filter(stadium_capacity < capacity_max_league & team_name != "League Average"),
    					aes(x = -3000, y = team_name,
    							label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
    							hjust = -.02)) +
    geom_text(data = bel_att_23_sum %>% filter(team_name == "League Average"),
    					aes(x = stadium_capacity + 1100, y = team_name,
    							label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
    							hjust = -.02, fontface = "bold")) +
    scale_x_continuous(limits = c(-3000,
                                  max(bel_att_23_sum$stadium_capacity + 3000)),
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

bel_attplot +
  geom_text(data = bel_att_23_sum %>% filter(stadium_capacity == capacity_max_league & team_name == "Club Brugge"),
            aes(x = stadium_capacity - 16000, y = team_name,
                label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
                hjust = .04, vjust = .02)) +
  geom_text(data = bel_att_23_sum %>% filter(stadium_capacity == capacity_max_league & team_name == "Cercle Brugge"),
            aes(x = stadium_capacity - 16000, y = team_name,
                label = paste0("Pct of capacity for season = ", round(capacity_pct_team * 100, 1), "%"),
                hjust = .04, vjust = -.1)) +
  labs(
    title = glue::glue("<b>Belgian Jupiler League <span style='color: #FF7F00;'>Average attendance</span>,
	 		  <span style='color: #1F78B4;'>Stadium capacity</span></b>, and<b> avg pct capacity for season</b>, by club, 2022-23 season.</b><br>
				Belgian clubs overall at about 60% capacity, with many above 70% and a few between 15%-30%. <br>
		                   Anderlecht total includes 1 match behind closed doors, thus 0 attendance."))


### try two plots bar chart with pct capcity team, then dumbbell chart to right, combine with patchwork
## no y axis on dumbbell chart

bel_bar <-
bel_att_23_sum %>%
  arrange(desc(capacity_pct_team)) %>%
  mutate(team_name = fct_reorder(team_name, capacity_pct_team)) %>%
  ggplot(aes(capacity_pct_team, team_name)) +
  geom_col(fill = "#1F78B4") +
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

bel_bubble <-
bel_att_23_sum %>%
  arrange(desc(capacity_pct_team)) %>%
  mutate(team_name = fct_reorder(team_name, capacity_pct_team)) %>%
  ggplot(aes(stadium_capacity, capacity_pct_team)) +
  # points for avg attendance & capacity
  geom_point(aes(x=stadium_capacity, y= reorder(team_name, capacity_pct_team)),
             color="#1F78B4", size=15, alpha = .5 ) +
  geom_point(aes(x=attend_avg_team, y= reorder(team_name, capacity_pct_team)),
             color="#FF7F00", size=15, alpha = .5 ) +
  # data labels for points
  # geom_text(data = bel_att_23_sum %>% filter(capacity_pct_team < .95),
  #           aes(x = attend_avg_team, y= reorder(team_name, capacity_pct_team),
  #               label = format(round(attend_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
  #           color = "black", size = 3.5) +
  # geom_text(data = bel_att_23_sum %>% filter(capacity_pct_team >= .95),
  #           aes(x = attend_avg_team, y= reorder(team_name, capacity_pct_team),
  #               label = format(round(attend_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
  #           color = "black", size = 3.5, hjust = 1.5) +
  geom_text(aes(x = attend_avg_team, y= reorder(team_name, capacity_pct_team),
                label = format(round(attend_avg_team, digits = 0),big.mark=",",scientific=FALSE)),
            color = "black", size = 3.5) +
  geom_text(aes(x = stadium_capacity, y= reorder(team_name, capacity_pct_team),
                label = format(round(stadium_capacity, digits = 0),big.mark=",",scientific=FALSE)),
            color = "black", size = 3.5) +
  # line connecting the points.
  geom_segment(aes(x=attend_avg_team + 900 , xend=stadium_capacity - 900,
                   y=team_name, yend=team_name), color="lightgrey") +
  scale_x_continuous(limits = c(0, max(bel_att_23_sum$stadium_capacity + 500)),
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

bel_bar + bel_bubble

bel_bar + bel_bubble +
  plot_layout(widths = c(1.5, 2)) +
  plot_annotation(title = "<b>Belgian Jupiler League
  <span style='color: #1F78B4;'>Average percent of capacity for season</span></b><i> (left bar chart)</i>,
  <b><span style='color: #FF7F00;'>Average attendance</span></b>, and
  <b><span style='color: #1F78B4;'>Stadium capacity</span></b>, by club, 2022-23 season.<br>
  Belgian clubs overall at about 60% capacity, with many above 70% and a few between 15%-30%.
  Only Antwerp above 80% capacity.<br>
  Club Brugge & Cecle Brugge both play at Jan Breydelstadion.
                  Anderlecht figures include 1 match behind closed doors, thus 0 attendance.",
                  theme = theme(plot.title =
                                  ggtext::element_textbox_simple(
                                    size = 12, fill = "cornsilk",
                                    lineheight = 1.5,
                                    padding = margin(5.5, 5.5, 5.5, 2),
                                    margin = margin(0, 0, 5.5, 0))))
