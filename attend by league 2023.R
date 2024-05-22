## attendance comparisons by league
## load league files, run thru summary function, join together rbind

library(tidyverse)
library(tidylog)
library(janitor)
library(glue)
library(ggtext)
library(ggrepel)

source("~/Data/r/basic functions.R")
options(scipen=10000)

# functions for summary df and plots
source("attend_functions.R")

# load league attendanc files, drop region/state, autonomous community
att_2023_bundes <- readRDS("~/Data/r/football data projects/data/att_2023_bundes.rds") %>%
	select(-state)
att_2023_ligue1 <- readRDS("~/Data/r/football data projects/data/att_2023_fra.rds") %>%
	select(-region)

glimpse(att_2023_bundes)
glimpse(att_2023_ligue1)

att_2023_all <- att_2023_bundes %>%
	rbind(att_2023_ligue1)

