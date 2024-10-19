# source file to pull stadium info for england & wales clubs in EFL/EPL

library(tidyverse)
library(tidylog)
library(janitor)
library(rvest)
library(httr)
library(polite)


## england stadium info via wikipedia (data as of last edit May 14 2024)
eng_url <- "https://en.wikipedia.org/wiki/List_of_football_stadiums_in_England"

eng_url_bow <- polite::bow(eng_url)
eng_url_bow

eng_stad_html <-
	polite::scrape(eng_url_bow) %>%  # scrape web page
	rvest::html_nodes("table.wikitable.sortable") %>% # pull out specific table
	rvest::html_table(fill = TRUE)

eng_stad_df <-
	eng_stad_html[[1]] %>%
	clean_names() %>%
	mutate(stadium = str_split(stadium, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	mutate(capacity = ifelse(stadium == "Craven Cottage", 24500, capacity)) %>%
	mutate(stadium = ifelse(team == "Manchester City", "Etihad Stadium", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Falmer Stadium", "The American Express Community Stadium", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Dean Court", "Vitality Stadium", stadium)) %>%
	select(stadium, town_city:league) %>%
	# add welsh clubs in English leagues
	# from https://en.wikipedia.org/wiki/List_of_stadiums_in_Wales_by_capacity
	add_row(tibble_row(stadium = "Racecourse Ground", town_city = "Wrexham, Wales", capacity = 13060,
										 team = "Wrexham AFC", league = "EFL Championship")) %>%
	add_row(tibble_row(stadium = "Cardiff City Stadium", town_city = "Cardiff, Wales", capacity = 33316,
										 team = "Cardiff City FC", league = "EFL Championship")) %>%
	add_row(tibble_row(stadium = "Swansea.com Stadium", town_city = "Swansea, Wales", capacity = 21088,
										 team = "Swansea City AFC", league = "EFL Championship"))

glimpse(eng_stad_df)

saveRDS(eng_stad_df, file = "~/Data/r/football data projects/data/stadiums_eng.rds")

# mls via wikipedia data as of May 7 2024
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

saveRDS(mls_stad_df, file = "~/Data/r/football data projects/data/stadiums_mls.rds")

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
	rbind(ger_stad_df2) %>%
	mutate(stadium = ifelse(stadium == "Deutsche Bank Park  (Waldstadion)", "Waldstadion", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Borussia-Park", "Stadion im Borussia-Park", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Signal Iduna Park  (Westfalenstadion)", "Westfalenstadion", stadium)) %>%
	mutate(stadium = ifelse(stadium == "MHPArena  (Neckarstadion)", "Neckarstadion", stadium)) %>%
	mutate(stadium = ifelse(stadium == "RheinEnergieStadion  (Müngersdorfer Stadion)",
													"Müngersdorfer Stadion", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Veltins-Arena  (Arena AufSchalke)", "Arena AufSchalke", stadium)) %>%
	mutate(stadium = ifelse(stadium == "PreZero Arena  (Rhein-Neckar-Arena)", "Rhein-Neckar-Arena", stadium))


saveRDS(ger_stad_df, file = "~/Data/r/football data projects/data/stadiums_ger.rds")

## italy last edit april 22, 2024
ita_url <- "https://en.wikipedia.org/wiki/List_of_football_stadiums_in_Italy"

ita_url_bow <- polite::bow(ita_url)
ita_url_bow

ita_stad_html <-
	polite::scrape(ita_url_bow) %>%  # scrape web page
	rvest::html_nodes("table.wikitable.sortable") %>% # pull out specific table
	rvest::html_table(fill = TRUE)

ita_stad_df <-
	ita_stad_html[[1]] %>%
	clean_names() %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	mutate(stadium = ifelse(stadium == "Juventus Stadium", "Allianz Stadium", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Dacia Arena(Stadio Friuli)", "Stadio Friuli", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Stadio Artemio Franchi" & home_team == "AC Siena",
													"Stadio Artemio Franchi – Montepaschi Arena", stadium)) %>%
	select(stadium, city, region, capacity, team = home_team, opened, renovated)

glimpse(ita_stad_df)

saveRDS(ita_stad_df, file = "~/Data/r/football data projects/data/stadiums_ita.rds")

## spain

spa_url <- "https://en.wikipedia.org/wiki/List_of_stadiums_in_Spain"

spa_url_bow <- polite::bow(spa_url)
spa_url_bow

spa_stad_html <-
	polite::scrape(spa_url_bow) %>%  # scrape web page
	rvest::html_nodes("table.wikitable.sortable") %>% # pull out specific table
	rvest::html_table(fill = TRUE)

spa_stad_df1 <-
	spa_stad_html[[1]] %>%
	clean_names() %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = ifelse(stadium == "Camp Nou", "99,354", capacity)) %>%
	mutate(capacity = ifelse(stadium == "La Cartuja", "57,691", capacity))%>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	select(stadium, city, autonomous_community, capacity, team, opened = inaugurated)

glimpse(spa_stad_df1)

spa_stad_df2 <-
	spa_stad_html[[2]] %>%
	clean_names() %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	mutate(autonomous_community = "") %>%
	mutate(opened = as.integer(NA)) %>%
	select(stadium, city = location, autonomous_community, capacity, team = tenants, opened)

glimpse(spa_stad_df2)

# autonomous community & province codes, as matched to city names & codes
sp_city_comm_pro1 <- readxl::read_excel("~/Data/spain_city_community_province.xlsx", sheet = "dic24") %>%
	clean_names() %>%
	mutate(codauto = as.numeric(codauto)) %>%
	mutate(cpro = as.numeric(cpro))

glimpse(sp_city_comm_pro1)

sp_city_comm_pro1 %>%
	count(codauto)

# province codes and names, matched to autonomous communities
sp_comm_pro_codes <- readxl::read_excel("~/Data/spain_city_community_province.xlsx", sheet = "community region") %>%
	clean_names()

glimpse(sp_comm_pro_codes)

# autonomous community codes and names
sp_autcom <- sp_comm_pro_codes %>%
	select(codauto, autonomous_community) %>%
	distinct(codauto, .keep_all = T)

# province codes and names
sp_prov <- sp_comm_pro_codes %>%
	select(cpro, province)

# match autonomous community names & codes to city & autonomous comm
# then match province names and codes on porvince code
sp_city_comm_pro <- sp_city_comm_pro1 %>%
	left_join(sp_autcom) %>%
	left_join(sp_prov) %>%
	 # donostia is basque name, easier to simplify to San Sebastián
	mutate(city = ifelse(city == "Donostia/San Sebastián", "San Sebastián", city)) %>%
	# other fixes where better to have city as shown in stadium file
	mutate(city = ifelse(city == "Coruña, A", "A Coruña", city)) %>%
	mutate(city = ifelse(city == "Palmas de Gran Canaria, Las", "Las Palmas", city)) %>%
	mutate(city = ifelse(city == "Elx/Elche", "Elche", city)) %>%
	mutate(city = ifelse(city == "Alacant/Alicante", "Alicante", city)) %>%
	mutate(city = ifelse(city == "Pamplona/Iruña", "Pamplona", city)) %>%
	mutate(city = ifelse(city == "Vila-real", "Villarreal", city)) %>%
	mutate(city = ifelse(city == "Línea de la Concepción, La", "La Línea de la Concepción", city)) %>%
	mutate(city = ifelse(city == "Castelló de la Plana", "Castellón de la Plana", city)) %>%
	mutate(city = ifelse(city == "Hospitalet de Llobregat, L'", "L'Hospitalet de Llobregat", city)) %>%
	mutate(city = ifelse(city == "Orotava, La", "La Orotava", city)) %>%
	mutate(city = ifelse(city == "Eivissa", "Ibiza", city)) %>%

	mutate(province = ifelse(province == "Coruña, A", "A Coruña", province)) %>%
	mutate(province = ifelse(province == "Palmas, Las", "Las Palmas", province)) %>%
	mutate(province = ifelse(province == "Alicante/Alacant", "Alicante", province)) %>%
	mutate(province = ifelse(province == "Rioja, La", "La Rioja", province)) %>%
	mutate(province = ifelse(province == "Balears, Illes", "Balearic Islands", province)) %>%
	mutate(province = ifelse(province == "Valencia/València", "València", province)) %>%
	mutate(province = ifelse(province == "Castellón/Castelló", "Castellón", province)) %>%

	mutate(autonomous_community = ifelse(
		autonomous_community == "Navarra, Comunidad Foral de", "Navarrra", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community ==	"Asturias, Principado de", "Asturias", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community ==	"Madrid, Comunidad de", "Comunidad de Madrid", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
	autonomous_community ==	"País Vasco", "Basque Country", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community ==	"Rioja, La", "La Rioja", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
	autonomous_community ==	"Balears, Illes",	"Balearic Islands", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community ==	"Murcia, Región de", "Murcia", autonomous_community))

# join 2 stadium tables, then match with city/community/province data
spa_stad_df <- spa_stad_df1 %>%
	# 2nd stadium tables
	rbind(spa_stad_df2) %>%
	filter(!grepl("rugby",team)) %>%
	# fix any stadium, city, ac,  or province names to align
	mutate(stadium = ifelse(stadium == "El Sadar", "Estadio El Sadar", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Montilivi", "Estadi Municipal de Montilivi", stadium)) %>%
	mutate(stadium = ifelse(stadium == "José Zorrilla", "Estadio Municipal José Zorrilla", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Nuevo Mirandilla" ,"Estadio Nuevo Mirandilla", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Mestalla", "Estadio de Mestalla", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Coliseum" ,"Estadio Coliseum", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Benito Villamarín", "Estadio Benito Villamarín", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Ramón Sánchez Pizjuán" ,"Estadio Ramón Sánchez Pizjuán", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Metropolitano" ,"Estadio Cívitas Metropolitano", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Anoeta" ,"Estadio de Anoeta", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Martínez Valero" ,"Estadio Manuel Martínez Valero", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Santiago Bernabéu" ,"Estadio Santiago Bernabéu", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Stage Front Stadium" ,"Estadi Cornellà-El Prat", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Son Moix" ,"Mallorca Son Moix Stadium", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Ciutat de València" ,"Estadio Ciudad de Valencia", stadium)) %>%
	mutate(stadium = ifelse(stadium == 	"Balaídos", "Estadio de Balaídos", stadium)) %>%
	mutate(stadium = ifelse(stadium == 	"La Cerámica", "Estadio de la Cerámica", stadium)) %>%
	mutate(city = ifelse(city == "Seville", "Sevilla", city)) %>%
	mutate(city = ifelse(city == "Valencia", "València", city)) %>%
	mutate(city = ifelse(city == "Santa Cruz de La Palma", "Santa Cruz de la Palma", city)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community == "Valencia", "Comunitat Valenciana", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community == "Navarre", "Navarrra", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community ==	"Catalonia", "Cataluña", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community ==	"Andalusia", "Andalucía", autonomous_community)) %>%
		mutate(autonomous_community = ifelse(
			autonomous_community == "Aragon", "Aragón", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community ==	"Canary Islands", "Canarias", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community ==	"Madrid", "Comunidad de Madrid", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community ==	"Castile and León", "Castilla y León", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community ==	"Castile-La Mancha", "Castilla-La Mancha", autonomous_community)) %>%
	mutate(autonomous_community = ifelse(
		autonomous_community ==	"La Rioja (Spain)", "La Rioja", autonomous_community)) %>%
# join to city/ac/province.
	left_join(sp_city_comm_pro, by = c("city")) %>%
	mutate(autonomous_community.x = ifelse(autonomous_community.x == "", autonomous_community.y, autonomous_community.x)) %>%
	select(stadium, city, autonomous_community = autonomous_community.x, province, capacity:opened,
				 auton_comm_code = codauto, prov_code = cpro, city_code = cmun, dc)

glimpse(spa_stad_df)
saveRDS(spa_stad_df, file = "~/Data/r/football data projects/data/stadiums_spa.rds")

# checks on missing provinces & acs
spa_stad_df %>%
	count(autonomous_community) %>%
	view()

spa_stad_df %>%
	count(province) %>%
	view()

spa_stad_df %>%
	count(city) %>%
	view()

spa_stad_df %>%
	count(autonomous_community, province) %>%
	view()

spa_stad_df %>%
	count(autonomous_community, province, dc) %>%
	view()


# spa_stad_df %>%
# 	filter(is.na(province)) %>%
# 	view()
#
# spa_stad_df %>%
# 	filter(autonomous_community.x != autonomous_community.y) %>%
# 	select(stadium, city, team, autonomous_community.x, autonomous_community.y, province) %>%
# 	view()


## portugal last edit april 6 2024
por_url <- "https://en.wikipedia.org/wiki/List_of_football_stadiums_in_Portugal"

por_url_bow <- polite::bow(por_url)
por_url_bow

por_stad_html <-
	polite::scrape(por_url_bow) %>%  # scrape web page
	rvest::html_nodes("table.wikitable.sortable") %>% # pull out specific table
	rvest::html_table(fill = TRUE)

por_stad_df <-
	por_stad_html[[1]] %>%
	clean_names() %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	select(stadium, city, capacity, team = tenants)

glimpse(por_stad_df)

saveRDS(por_stad_df, file = "~/Data/r/football data projects/data/stadiums_por.rds")

## france

fra_url <- "https://en.wikipedia.org/wiki/List_of_football_stadiums_in_France"

fra_url_bow <- polite::bow(fra_url)
fra_url_bow

fra_stad_html <-
	polite::scrape(fra_url_bow) %>%  # scrape web page
	rvest::html_nodes("table.wikitable.sortable") %>% # pull out specific table
	rvest::html_table(fill = TRUE)

fra_stad_df1 <-
	fra_stad_html[[1]] %>%
	clean_names() %>%
	# keeps text up to parens
	mutate(city = str_split(city, "\\(", simplify=T)[,1]) %>%
	# keeps text up to [
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	# removes , and converts to number
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	mutate(opened = as.character(opened)) %>%
	select(stadium, city, region, capacity, team = home_team_s, opened)

glimpse(fra_stad_df1)

fra_stad_df2 <-
	fra_stad_html[[2]] %>%
	clean_names() %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = str_split(capacity, "\\(", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	mutate(city = str_split(city, "\\(", simplify=T)[,1]) %>%
	select(stadium, city, region, capacity, team = home_team_s, opened)

glimpse(fra_stad_df2)

fra_stad_df3 <-
	fra_stad_html[[3]] %>%
	clean_names() %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	mutate(region = case_when(location == "Rodez" ~ "Occitania",
														location == "Cholet" ~ "Pays de la Loire",
														location == "Concarneau" ~ "Brittany",
														location == "Orléans" ~ "Centre-Val de Loire")) %>%
	mutate(opened = as.character(NA)) %>%
	select(stadium = venue, city = location, region, capacity, team = home_team_s, opened)

glimpse(fra_stad_df3)

fra_stad_df <- fra_stad_df1 %>%
	rbind(fra_stad_df2) %>%
	rbind(fra_stad_df3) %>%
	mutate(stadium = ifelse(stadium == "Parc Olympique Lyonnais", "Groupama Stadium", stadium)) %>%
	mutate(team = ifelse(team == "Stade brestois 29", "Stade Brestois 29", team)) %>%
	add_row(tibble_row(stadium = "Stade Louis II", city = "Monaco", region = "Monaco",
										 capacity = 16360, team = "AS Monaco FC", opened = "1985"))

glimpse(fra_stad_df)

saveRDS(fra_stad_df, file = "~/Data/r/football data projects/data/stadiums_fra.rds")


## netherlands
ned_url <- "https://en.wikipedia.org/wiki/List_of_football_stadiums_in_the_Netherlands"

ned_url_bow <- polite::bow(ned_url)
ned_url_bow

ned_stad_html <-
	polite::scrape(ned_url_bow) %>%  # scrape web page
	rvest::html_nodes("table.wikitable.sortable") %>% # pull out specific table
	rvest::html_table(fill = TRUE)

ned_stad_df1 <-
	ned_stad_html[[1]] %>%
	clean_names() %>%
	mutate(stadium = ifelse(stadium == "Johan Cruijff ArenA", "Johan Cruijff Arena", stadium)) %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	select(stadium, city, province, capacity, team = home_team, opened)

glimpse(ned_stad_df1)

ned_stad_df2 <-
	ned_stad_html[[2]] %>%
	clean_names() %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	mutate(province = as.character(NA)) %>%
	mutate(opened = as.integer(NA)) %>%
	select(stadium = venue, city = location, province, capacity, team = club, opened)

glimpse(ned_stad_df2)

ned_stad_df <- ned_stad_df1 %>%
	rbind(ned_stad_df2)

glimpse(ned_stad_df)

saveRDS(ned_stad_df, file = "~/Data/r/football data projects/data/stadiums_ned.rds")

## denmark
den_url <- "https://en.wikipedia.org/wiki/List_of_football_stadiums_in_Denmark"

den_url_bow <- polite::bow(den_url)
den_url_bow

den_stad_html <-
	polite::scrape(den_url_bow) %>%  # scrape web page
	rvest::html_nodes("table.wikitable.sortable") %>% # pull out specific table
	rvest::html_table(fill = TRUE)

den_stad_df <-
	den_stad_html[[1]] %>%
	clean_names() %>%
	mutate(city = ifelse(city == "Østerbro, Copenhagen", "København", city)) %>%
	mutate(city = ifelse(city == "Valby, Copenhagen", "København", city)) %>%
	mutate(city = ifelse(city == "Brøndbyvester", "Brøndby", city)) %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	select(stadium, city, capacity, team = club)

glimpse(den_stad_df)

den_stad_df %>%
	count(city) %>%
	view()

saveRDS(den_stad_df, file = "~/Data/r/football data projects/data/stadiums_den.rds")

## sweden
swe_url <- "https://en.wikipedia.org/wiki/List_of_football_stadiums_in_Sweden"

swe_url_bow <- polite::bow(swe_url)
swe_url_bow

swe_stad_html <-
	polite::scrape(swe_url_bow) %>%  # scrape web page
	rvest::html_nodes("table.wikitable.sortable") %>% # pull out specific table
	rvest::html_table(fill = TRUE)

swe_stad_df1 <-
	swe_stad_html[[1]] %>%
	clean_names() %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	mutate(stadium = ifelse(stadium == "Stadion", "Eleda Stadion", stadium)) %>%
	mutate(stadium = ifelse(stadium == "Östgötaporten", "PlatinumCars Arena", stadium)) %>%
	select(stadium, capacity, team = club, division)

glimpse(swe_stad_df1)

# need to pull this filter on sweden, match on stadium name for city
# stadium with city names
swe_url2 <- "https://www.nordicstadiums.com/list-of-football-stadiums-in-sweden/"

swe_url2_bow <- polite::bow(swe_url2)
swe_url2_bow

swe_sta_html <- rvest::read_html("https://www.nordicstadiums.com/list-of-football-stadiums-in-sweden/")
class(swe_sta_html)

swe_stad_html2 <-
	polite::scrape(swe_url2_bow) %>%   # scrape web page
	rvest::html_node("tbody") %>%
	rvest::html_table(fill = TRUE)

swe_stad_df2 <-
	swe_stad_html2 %>%
	clean_names() %>%
	select(stadium = x2, city = x3, teams = x1) %>%
	filter(!stadium == "Nya Ullevi")

glimpse(swe_stad_df2)

swe_stad_df <- swe_stad_df1 %>%
	full_join(swe_stad_df2) %>%
	mutate(city = ifelse(stadium == "Vångavallen", "Trelleborg", city)) %>%
	mutate(city = ifelse(stadium == "Jämtkraft Arena", "Östersund", city)) %>%
	mutate(city = ifelse(stadium == "T3 Arena", "Umeå", city)) %>%
	mutate(city = ifelse(stadium == "Starke Arvid Arena", "Ljungskile", city)) %>%
	mutate(city = ifelse(stadium == "Tunavallen", "Eskilstuna", city)) %>%
	mutate(city = ifelse(stadium == "Norrporten Arena", "Sundsvall", city)) %>%
	mutate(city = ifelse(stadium == "Stora Valla", "Degerfors", city)) %>%
	mutate(city = ifelse(stadium == "Domnarvsvallen", "Borlänge", city)) %>%
	mutate(city = ifelse(stadium == "Södertälje Fotbollsarena", "Södertälje", city)) %>%
	mutate(city = ifelse(stadium == "Gutavallen", "Visby", city)) %>%
  mutate(city = ifelse(stadium == "Hitachi Energy Arena", "Västerås SK", city))

saveRDS(swe_stad_df, file = "~/Data/r/football data projects/data/stadiums_swe.rds")


## Belgium

bel_url <- "https://en.wikipedia.org/wiki/List_of_football_stadiums_in_Belgium"

bel_url_bow <- polite::bow(bel_url)
bel_url_bow

bel_stad_html <-
	polite::scrape(bel_url_bow) %>%  # scrape web page
	rvest::html_nodes("table.wikitable.sortable") %>% # pull out specific table
	rvest::html_table(fill = TRUE)

bel_stad_df <-
	bel_stad_html[[1]] %>%
	clean_names() %>%
	mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
	mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
	select(stadium, city, capacity, team = club)

glimpse(bel_stad_df)

bel_stad_df %>%
	count(city) %>%
	view()

saveRDS(bel_stad_df, file = "~/Data/r/football data projects/data/stadiums_bel.rds")


# switzerland

sui_url <- "https://en.wikipedia.org/wiki/List_of_football_stadiums_in_Switzerland"

sui_url_bow <- polite::bow(sui_url)
sui_url_bow

sui_stad_html <-
  polite::scrape(sui_url_bow) %>%  # scrape web page
  rvest::html_nodes("table.wikitable.sortable") %>% # pull out specific table
  rvest::html_table(fill = TRUE)

sui_stad_df <-
  sui_stad_html[[1]] %>%
  clean_names() %>%
  mutate(capacity = str_split(capacity, "\\[", simplify=T)[,1]) %>%
  mutate(capacity = as.numeric(gsub(",", "", as.character(capacity)))) %>%
  select(stadium, city, capacity, team = home_team_s)

glimpse(sui_stad_df)

sui_stad_df %>%
  count(city) %>%
  view()

saveRDS(sui_stad_df, file = "~/Data/r/football data projects/data/stadiums_sui.rds")

