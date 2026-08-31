library(tidyverse)

# library(devtools)
# 
# install_github("skoval/deuce")
# 
# library(deuce)
# help(package = "deuce")
# data("atp_elo") #Daten bis Ende 2024 
# data("atp_players")
# data("atp_tournaments")
# 
# HB: Ich nehme lieber die Daten von JeffSackmann, da hier bei deuce die Surface Daten nicht vorhanden sind. 
# Ausserdem sind dann die Challenger Daten kein Problem und es gibt keine Probleme bei der Dopplung der Daten.


# wir benoetigen Surface, Home Variabel, H2H 
# ToDo: Matchup Id einbauen; Idee: kleinere Nummer zuerst, egal, wer Player 1 oder 2 ist

## lese die Daten ein
data <- read.csv("C:/Users/bartm/Documents/GitHub/tennis_atp/atp_matches_1968.csv", na.strings = "")
# alle anderen Matches
for(i in 1969:2024){
  data <- rbind(data, read.csv(
    paste0("C:/Users/bartm/Documents/GitHub/tennis_atp/atp_matches_", i, ".csv"), na.strings = ""))
}
# Challenger Matches 
for(i in 1978:2024){
  data <- rbind(data, read.csv(
    paste0("C:/Users/bartm/Documents/GitHub/tennis_atp/atp_matches_qual_chall_", i, ".csv"), na.strings = ""))
}
rm(i)

colnames(data)

data <- data %>% select(tourney_id, tourney_name, surface, tourney_level, tourney_date, 
                        winner_id, winner_name, winner_ioc, loser_id, loser_name, loser_ioc)

table(data$surface, useNA = "ifany")
table(data[data$surface == "Carpet", ]$tourney_level) # nur A, C, D, F, M
summary(data[data$surface == "Carpet", ]$tourney_date) # von 1968 bis 2023

table(data[data$tourney_level == "A","tourney_name"])
table(data[data$tourney_level == "C","tourney_name"])
table(data[data$tourney_level == "D","tourney_name"])
table(data[data$tourney_level == "F","tourney_name"])
table(data[data$tourney_level == "G","tourney_name"])
table(data[data$tourney_level == "M","tourney_name"])
table(data[data$tourney_level == "O","tourney_name"])

# O als level teilweise in A eingetragen
for(i in 1:nrow(data)){
  string <- strsplit(data$tourney_name[i], " ")[[1]]
  if("Olympics" %in% string) data$tourney_level[i] <- "O"  
}

# F als level teilweise falsch eingetragen bei
data$tourney_level[data$tourney_name == "NextGen Finals"] <- "F"
data$tourney_level[data$tourney_name == "Tour Finals"] <- "F"




## Erstelle win Variable
set.seed(23012026)
random_winner <- rbinom(nrow(data), size = 1, prob = c(0.5, 0.5)) 

player_name_temp <- opponent_name_temp <- player_ioc_temp <- opponent_ioc_temp <- 
  player_id_temp <- opponent_id_temp <- rep(NA, nrow(data))

for(i in 1:nrow(data)){
  if(random_winner[i] == 1){
    player_name_temp[i] <- data$winner_name[i]
    opponent_name_temp[i] <- data$loser_name[i]
    player_ioc_temp[i] <- data$winner_ioc[i]
    opponent_ioc_temp[i] <- data$loser_ioc[i]
    player_id_temp[i] <- data$winner_id[i]
    opponent_id_temp[i] <- data$loser_id[i]
  }
  else{
    player_name_temp[i] <- data$loser_name[i]
    opponent_name_temp[i] <- data$winner_name[i]
    player_ioc_temp[i] <- data$loser_ioc[i]
    opponent_ioc_temp[i] <- data$winner_ioc[i]
    player_id_temp[i] <- data$loser_id[i]
    opponent_id_temp[i] <- data$winner_id[i]
  }
}

win_data <- data.frame(win = random_winner,
                       player_id = player_id_temp, 
                       player_name = player_name_temp, 
                       player_ioc = player_ioc_temp,
                       opponent_id = opponent_id_temp, 
                       opponent_name = opponent_name_temp,
                       opponent_ioc = opponent_ioc_temp)

data <- data %>% select(tourney_id, tourney_name, surface, tourney_level, tourney_date)

data <- cbind(data, win_data)

rm(win_data, player_id_temp, player_name_temp, player_ioc_temp, opponent_id_temp, opponent_name_temp, 
   opponent_ioc_temp, i, random_winner, string)

## Ergaenze Match Up Identifier
data$matchup_id <- NA
for(i in 1:nrow(data)){
  data$matchup_id[i] <- paste0(min(data$player_id[i], data$opponent_id[i]), ":", 
                               max(data$player_id[i], data$opponent_id[i]))
}

#setwd("C:/Users/bartm/Documents/HKS/Paper")
#save(data, file = "data.RData")
#load("data.RData")

# tourney_date in Datenformat umwandeln 
data$tourney_date <- as.Date(as.character(data$tourney_date), format = "%Y%m%d")

# surface als factor
data$surface <- as.factor(data$surface)

# einige Nationalitaeten nicht als NA eingetragen 
ind <- which(data$player_ioc == "UNK")
data$player_ioc[ind] <- NA
ind <- which(data$opponent_ioc == "UNK")
data$opponent_ioc[ind] <- NA

# was ist mit U Unknown als Name, haben die eine eigene ID? 
ind <- which(data$player_name == "U Unknown")
data$player_name[ind] <- NA
ind <- which(data$opponent_name == "U Unknown")
data$opponent_name[ind] <- NA
rm(ind)

#setwd("C:/Users/bartm/Documents/HKS/Paper")
#save(data, file = "data.RData")
#load("data.RData")

#####

## Implementierung der home Variabel 

#install.packages("rjson")
library(rjson)
countries_list <- fromJSON(file = "C:/Users/bartm/Documents/Uni/Bachelor/Bachelorarbeit/
                           Bachelorarbeit_Bartmann/Kap_2_Datensatzerstellung/countries+cities.json")

df_cities <- do.call(
  rbind,
  lapply(countries_list, function(x) {
    # Wenn keine Städte vorhanden sind → überspringen
    if (is.null(x$cities) || length(x$cities) == 0) return(NULL)
    
    # Stadtnamen extrahieren
    city_names <- sapply(x$cities, function(y) y$name)
    
    data.frame(
      country = x$name,
      iso3 = x$iso3,
      city = city_names,
      stringsAsFactors = FALSE
    )
  })
)

rm(countries_list)

library(stringi)
# Encoding aendern 
df_cities <- df_cities %>%
  mutate(city = stri_trans_general(city, "Latin-ASCII"))

df_cities <- distinct(df_cities)

## betrachte alle Turniernamen 
tourney_place <- data.frame("index" = 1:nrow(data), "Original_Tourney" = data$tourney_name,
                            "tourney" = data$tourney_name, 
                            "city" = NA, "country" = NA, "iso3" = NA)

# ## betrachte alle Davis Cup Turniere zu einem anderen Zeitpunkt 
# tourneys_DC <- tourney_place[grep("Davis Cup", tourney_place$tourney),]
# 
# ## alle nicht Davis Cup Turniere 
# tourney_place <- tourney_place[grep("Davis Cup", tourney_place$tourney, invert = TRUE),]

sort(unique(tourney_place$tourney))
options(max.print=6000)

# Grand Slams
tourney_place$tourney[grep("US Open", tourney_place$tourney)] <- "New York"
tourney_place$tourney[grep("Us Open", tourney_place$tourney)] <- "New York"
tourney_place$tourney[grep("Australian Open", tourney_place$tourney)] <- "Melbourne"
tourney_place$tourney[grep("Wimbledon", tourney_place$tourney)] <- "London"
tourney_place$tourney <- gsub("Roland Garros", "Paris", tourney_place$tourney)

# entferne WCT Ch etc
tourney_place$tourney <- gsub("M25 ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("M25+H ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("M15 ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("M15+H ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("-WCT", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" WCT", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" WTC", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" NTL", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" CH", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" Challenger", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Olympics - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" Olympics", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" Masters", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" Indoors", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" Indoor", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" Outdoors", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" Outdoor", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" II", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" III", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" IV", "", tourney_place$tourney)
tourney_place$tourney <- gsub("ATP ", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" Chps", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" [[:digit:]]", "", tourney_place$tourney)
tourney_place$tourney <- gsub("[[:digit:]]", "", tourney_place$tourney)
tourney_place$tourney <- gsub("M5 ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("M ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Winston-Salem Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Western & Southern Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Viking International - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("US Men's Clay Court Championship - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Truist Atlanta Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Sydney Tennis Classic - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("St. Petersburg Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" N.S.W.", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Swiss Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Stockholm Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Serbia Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Sardegna Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("San Diego Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Rolex Paris - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Rogers Cup - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Rio Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Republic Of China", "China", tourney_place$tourney)
tourney_place$tourney <- gsub("Qatar ExxonMobil Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Poprad Tatry", "Poprad", tourney_place$tourney)
tourney_place$tourney <- gsub("Poprad-Tatry", "Poprad", tourney_place$tourney)
tourney_place$tourney <- gsub("Plava Laguna Croatia Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Open Provence - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Open Sud de France - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Open Parc - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Noventi Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Nitto ATP Finals - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Next Gen ATP Finals - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Nature Valley International - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("National Bank Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Mutua Madrid Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Mubadala World Tennis Championship", "Abu Dhabi", tourney_place$tourney)
tourney_place$tourney <- gsub("Murray River Open", "Murray", tourney_place$tourney)
tourney_place$tourney <- gsub("Tata Open Maharashtra - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Salzburg-Anif", "Salzburg", tourney_place$tourney)
tourney_place$tourney <- gsub("Rome GA", "Rome", tourney_place$tourney)
tourney_place$tourney <- gsub("Nordea Open - Bastad", "Båstad", tourney_place$tourney)
tourney_place$tourney <- gsub("New DelhiI", "New Delhi", tourney_place$tourney)
tourney_place$tourney <- gsub("Moselle Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Montreal / Toronto", "Toronto", tourney_place$tourney)
tourney_place$tourney <- gsub("Montechiarugolo - Parma", "Montechiarugolo", tourney_place$tourney)
tourney_place$tourney <- gsub("Monte-Carlo Rolex - Monte-Carlo", "Monte Carlo", tourney_place$tourney)
tourney_place$tourney <- gsub("Millennium Estoril Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Miami Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Mercedes Cup - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Melbourne Summer Set - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Mallorca Championships - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" SoCal Chps", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" / Queen's Club", "", tourney_place$tourney)
tourney_place$tourney <- gsub(" PSW", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Libema Open - '", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Laver Cup - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Kremlin Cup - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Japanese Championships", "Japan", tourney_place$tourney)
tourney_place$tourney <- gsub("Internazionali BNL d'Italia - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Ikeja-Lagos", "Ikeja", tourney_place$tourney)
tourney_place$tourney <- gsub("Ho Chi Minh", "Ho Chi Minh City", tourney_place$tourney)
tourney_place$tourney <- gsub("Ho Chi Minh City City", "Ho Chi Minh City", tourney_place$tourney)
tourney_place$tourney <- gsub("Hamburg European Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Hamburg European Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Hall of Fame Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Grand Prix Hassan - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Gonet Geneva Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Generali Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("French Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("European Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Emilia-Romagna Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Dubai Duty Free Tennis Championships - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Delray Beach Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Dallas Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Cordoba Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Citi Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("cinch Championships - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Chile Dove Men+Care Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Champaign-Urbana", "Illinois", tourney_place$tourney)
tourney_place$tourney <- gsub("BNP Paribas Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("BMW Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Belgrade - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("ATP Cup - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Astana Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Argentina Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Anytech Andalucia Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Adelaide International - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("ABN AMRO World Tennis Tournament - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Abierto Mexicano de Tenis Mifel - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Abierto de Tenis Mifel - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Abierto Mexicano Telcel - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Sofia Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Newport Beach", "Newport", tourney_place$tourney)
tourney_place$tourney <- gsub("North Miami Beach", "Miami", tourney_place$tourney)
tourney_place$tourney <- gsub("Mouilleron Le Captif", "Mouilleron-le-Captif", tourney_place$tourney)
tourney_place$tourney <- gsub("Mouilleron-Le-Captif", "Mouilleron-le-Captif", tourney_place$tourney)
tourney_place$tourney <- gsub("Toyota City", "Japan", tourney_place$tourney)
tourney_place$tourney <- gsub("Toyota", "Japan", tourney_place$tourney)
tourney_place$tourney <- gsub("s-Hertogenbosch", "Netherlands", tourney_place$tourney)
tourney_place$tourney <- gsub("s Hertogenbosch", "Netherlands", tourney_place$tourney)
tourney_place$tourney <- gsub("Barcelona Open Banc Sabadell - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("M[[:punct:]]H ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Chile Dove Men+Care Open - ", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Johannesburg / Ellispark", "Johannesburg", tourney_place$tourney)
tourney_place$tourney <- gsub("Johannesburg / Markspark", "Johannesburg", tourney_place$tourney)
tourney_place$tourney <- gsub("Sylt", "Germany", tourney_place$tourney)
tourney_place$tourney <- gsub("Toulouse-Balma", "Balma", tourney_place$tourney)
tourney_place$tourney <- gsub("Warmbad-Villach", "Villach", tourney_place$tourney)
tourney_place$tourney <- gsub("Esch/Alzette", "Esch", tourney_place$tourney)
tourney_place$tourney <- gsub("Maebashi City", "Maebashi", tourney_place$tourney)
tourney_place$tourney <- gsub(" Nationals", "", tourney_place$tourney)
tourney_place$tourney <- gsub("Aix-En-Provence", "Aix-en-Provence", tourney_place$tourney)
tourney_place$tourney <- gsub("Aix En Provence", "Aix-en-Provence", tourney_place$tourney)
tourney_place$tourney <- gsub("Aix en Provence", "Aix-en-Provence", tourney_place$tourney)
tourney_place$tourney <- gsub("Chicago-", "Chicago", tourney_place$tourney)

tourney_place$tourney <- sub("-$", "", tourney_place$tourney)
tourney_place$tourney <- sub(" $", "", tourney_place$tourney)

tourney_place$tourney[grep("Alphen", tourney_place$tourney)] <- "Alphen"
tourney_place$tourney[grep("Australia", tourney_place$tourney)] <- "Australia"
tourney_place$tourney[grep("Braunchweig", tourney_place$tourney)] <- "Braunschweig"
tourney_place$tourney[grep("Doha", tourney_place$tourney)] <- "Doha" 
tourney_place$tourney[grep("Gothenberg", tourney_place$tourney)] <- "Gothenburg" 
tourney_place$tourney[grep("Great Ocean Road Open", tourney_place$tourney)] <- "Melbourne"
tourney_place$tourney[grep("King's CupR", tourney_place$tourney)] <- "Kings Cup"
tourney_place$tourney[grep("Mauritius", tourney_place$tourney)] <- "Mauritius"
tourney_place$tourney[grep("Mouilleron", tourney_place$tourney)] <- "Mouilleron-le-Captif"
tourney_place$tourney[grep("Naples", tourney_place$tourney)] <- "Naples"
tourney_place$tourney[grep("Puerta Vallarta", tourney_place$tourney)] <- "Puerto Vallarta"
tourney_place$tourney[grep("Ribeiro Preta", tourney_place$tourney)] <- "Ribeirao Preto"
tourney_place$tourney[grep("Ribeiro Preto", tourney_place$tourney)] <- "Ribeirao Preto"
tourney_place$tourney[grep("Ribeirao Preta", tourney_place$tourney)] <- "Ribeirao Preto"
tourney_place$tourney[grep("Rio De Janeiro", tourney_place$tourney)] <- "Rio de Janeiro"
tourney_place$tourney[grep("Roland Garros", tourney_place$tourney)] <- "Paris"
tourney_place$tourney[grep("Saint Brieuc", tourney_place$tourney)] <- "Saint-Brieuc"
tourney_place$tourney[grep("Saint Tropez", tourney_place$tourney)] <- "Saint-Tropez"
tourney_place$tourney[grep("San Benedetto Del Tronto", tourney_place$tourney)] <- "San Benedetto del Tronto"
tourney_place$tourney[grep("Santa Cruz De La Sierra", tourney_place$tourney)] <- "Santa Cruz de la Sierra"
tourney_place$tourney[grep("Shenzhen", tourney_place$tourney)] <- "Shenzhen"
tourney_place$tourney[grep("St Remy", tourney_place$tourney)] <- "St. Remy"
tourney_place$tourney[grep("Brieuc", tourney_place$tourney)] <- "St. Brieuc"
tourney_place$tourney[grep("Stockholm", tourney_place$tourney)] <- "Stockholm"
tourney_place$tourney[grep("Us Open", tourney_place$tourney)] <- "USA"
tourney_place$tourney[grep("US Open", tourney_place$tourney)] <- "USA"
tourney_place$tourney[grep("Vina Del Mar", tourney_place$tourney)] <- "Vina del Mar"
tourney_place$tourney[grep("Zell", tourney_place$tourney)] <- "Zell am See"

# sort(unique(tourney_place$tourney))[-c(1:850)]
# table(tourney_place$tourney[grep("Zell", tourney_place$tourney)])
# df_cities[df_cities$city == "Zell am See",]


###
## betrachte zunaechst die eindeutigen Staedte

# Häufigkeit der Städte zählen
city_counts <- table(df_cities$city)

# Nur eindeutige Städte behalten
unique_cities <- names(city_counts[city_counts == 1])

# Datenrahmen auf eindeutige Städte reduzieren
df_cities_unique <- df_cities[df_cities$city %in% unique_cities, ]

# Jetzt den Match nur für diese eindeutigen Städte durchführen
tourney_place$city <- df_cities_unique$city[match(tourney_place$tourney, df_cities_unique$city)]
tourney_place$country <- df_cities_unique$country[match(tourney_place$tourney, df_cities_unique$city)]
tourney_place$iso3 <- df_cities_unique$iso3[match(tourney_place$tourney, df_cities_unique$city)]

###
## alte Ueberschreibung der Laender

tourney_place[tourney_place$tourney == "Melbourne", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Philadelphia", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Rome", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Paris", "country"] <- "France"
tourney_place[tourney_place$tourney == "Barcelona", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "London", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Dublin", "country"] <- "Ireland"
tourney_place[tourney_place$tourney == "Hamburg", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Los Angeles", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Buenos Aires", "country"] <- "Argentina"
tourney_place[tourney_place$tourney == "Hobart", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Perth", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Sydney", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Brussels", "country"] <- "Belgium"
tourney_place[tourney_place$tourney == "Bristol", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Washington", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Boston", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Toronto", "country"] <- "Canada"
tourney_place[tourney_place$tourney == "Las Vegas", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Wembley", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Miami", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Richmond", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Salisbury", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Hampton", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Cambridge", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "San Juan", "country"] <- "Argentina"
tourney_place[tourney_place$tourney == "Dallas", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Houston", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Manchester", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Casablanca", "country"] <- "Morocco"
tourney_place[tourney_place$tourney == "Nottingham", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Newport", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Leicester", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Berkeley", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Vancouver", "country"] <- "Canada"
tourney_place[tourney_place$tourney == "Midland", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Nice", "country"] <- "France"
tourney_place[tourney_place$tourney == "Palermo", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Madrid", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Berlin", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Sacramento ", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Cologne ", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Kingston", "country"] <- "Jamaica"
tourney_place[tourney_place$tourney == "Adelaide", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Cleveland", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Albany", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Essen", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Rotterdam", "country"] <- "Netherlands"
tourney_place[tourney_place$tourney == "Brisbane", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Birmingham", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Milan", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Valencia", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Florence", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "San Francisco", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Manila", "country"] <- "Philippines"
tourney_place[tourney_place$tourney == "Prague", "country"] <- "Czech Republic"
tourney_place[tourney_place$tourney == "Christchurch", "country"] <- "New Zealand"
tourney_place[tourney_place$tourney == "Dayton", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Tempe", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Cedar Grove", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Vienna", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "San Antonio", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Fairfield", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Cairo", "country"] <- "Egypt"
tourney_place[tourney_place$tourney == "Monterrey", "country"] <- "Mexico"
tourney_place[tourney_place$tourney == "Lagos", "country"] <- "Nigeria"
tourney_place[tourney_place$tourney == "Palma", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Santiago", "country"] <- "Chile"
tourney_place[tourney_place$tourney == "Springfield", "country"] <- "United States"
tourney_place[tourney_place$tourney == "San Jose", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Murcia", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Oviedo", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Guadalajara", "country"] <- "Mexico"
tourney_place[tourney_place$tourney == "Stuttgart", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Lancaster", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Lincoln", "country"] <- "United States"
tourney_place[tourney_place$tourney == "San Ramon", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Pasadena", "country"] <- "United States"
tourney_place[tourney_place$tourney == "New Haven", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Concord", "country"] <- "United States"
tourney_place[tourney_place$tourney == "San Diego", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Salvador", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Royan", "country"] <- "France"
tourney_place[tourney_place$tourney == "Beckenham", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Lugo", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "China", "country"] <- "China"
tourney_place[tourney_place$tourney == "Venice", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "San Remo", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Bara", "country"] <- "Romania"
tourney_place[tourney_place$tourney == "Tarragona", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Bari", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Athens", "country"] <- "Greece"
tourney_place[tourney_place$tourney == "Lisbon", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Amsterdam", "country"] <- "Netherlands"
tourney_place[tourney_place$tourney == "Vigo", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Sutton", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Neunkirchen", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Livingston", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Treviso", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Jerusalem", "country"] <- "Israel"
tourney_place[tourney_place$tourney == "Bergen", "country"] <- "Norway"
tourney_place[tourney_place$tourney == "Scottsdale", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Munster", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Port Elizabeth", "country"] <- "South Africa"
tourney_place[tourney_place$tourney == "Wellington", "country"] <- "New Zealand"
tourney_place[tourney_place$tourney == "San Marino", "country"] <- "San Marino"
tourney_place[tourney_place$tourney == "Verona", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Brest", "country"] <- "France"
tourney_place[tourney_place$tourney == "Telford", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Croydon", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Zaragoza", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Nicosia", "country"] <- "Cyprus"
tourney_place[tourney_place$tourney == "Moscow", "country"] <- "Russia"
tourney_place[tourney_place$tourney == "Sevilla", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Newcastle", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Warsaw", "country"] <- "Poland"
tourney_place[tourney_place$tourney == "Segovia", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Halle", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Halifax", "country"] <- "Canada"
tourney_place[tourney_place$tourney == "Launceston", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Dresden", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Bochum", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Porto", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Montebello", "country"] <- "Canada"
tourney_place[tourney_place$tourney == "Malta", "country"] <- "Malta"
tourney_place[tourney_place$tourney == "Seville", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Lima", "country"] <- "Peru"
tourney_place[tourney_place$tourney == "Montevideo", "country"] <- "Uruguay"
tourney_place[tourney_place$tourney == "Glendale", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Nantes", "country"] <- "France"
tourney_place[tourney_place$tourney == "Mendoza", "country"] <- "Argentina"
tourney_place[tourney_place$tourney == "Asuncion", "country"] <- "Paraguay"
tourney_place[tourney_place$tourney == "Granby", "country"] <- "Canada"
tourney_place[tourney_place$tourney == "Charleroi", "country"] <- "Belgium"
tourney_place[tourney_place$tourney == "Salinas", "country"] <- "Ecuador"
tourney_place[tourney_place$tourney == "Alicante", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Madras", "country"] <- "India"
tourney_place[tourney_place$tourney == "Urbana", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Santa Cruz", "country"] <- "Bolivia"
tourney_place[tourney_place$tourney == "Edinburgh", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Rio Grande", "country"] <- "Puerto Rico"
tourney_place[tourney_place$tourney == "Burbank", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Sopot", "country"] <- "Poland"
tourney_place[tourney_place$tourney == "Belgrade", "country"] <- "Serbia"
tourney_place[tourney_place$tourney == "Maia", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Toluca", "country"] <- "Mexico"
tourney_place[tourney_place$tourney == "Lucknow", "country"] <- "India"
tourney_place[tourney_place$tourney == "Hamilton", "country"] <- "New Zealand"
tourney_place[tourney_place$tourney == "Brighton", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Bolton", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Leon", "country"] <- "Mexico"
tourney_place[tourney_place$tourney == "Donetsk", "country"] <- "Ukraine"
tourney_place[tourney_place$tourney == "Fresno", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Zell", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "Valladolid", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Mandeville", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Groningen", "country"] <- "Netherlands"
tourney_place[tourney_place$tourney == "Tiburon", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Torrance", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Manta", "country"] <- "Ecuador"
tourney_place[tourney_place$tourney == "Santa Cruz de la Sierra", "country"] <- "Bolivia"
tourney_place[tourney_place$tourney == "Cuenca", "country"] <- "Ecuador"
tourney_place[tourney_place$tourney == "Pamplona", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Orleans", "country"] <- "France"
tourney_place[tourney_place$tourney == "Mons", "country"] <- "Belgium"
tourney_place[tourney_place$tourney == "Southampton", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Sunderland", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Cardiff", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Joinville", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Como", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Naples", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Shrewsbury", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Rabat", "country"] <- "Morocco"
tourney_place[tourney_place$tourney == "Trnava", "country"] <- "Slovakia"
tourney_place[tourney_place$tourney == "Humacao", "country"] <- "Puerto Rico"
tourney_place[tourney_place$tourney == "San Sebastian", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Arad", "country"] <- "Romania"
tourney_place[tourney_place$tourney == "Bath", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Happy Valley", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Glasgow", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Santo Domingo", "country"] <- "Dominican Republic"
tourney_place[tourney_place$tourney == "Stockton", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Koblenz", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Floridablanca", "country"] <- "Colombia"
tourney_place[tourney_place$tourney == "Lille", "country"] <- "France"
tourney_place[tourney_place$tourney == "Braga", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Pau", "country"] <- "France"
tourney_place[tourney_place$tourney == "Parma", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Murray", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Las Palmas", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Monastir", "country"] <- "Tunisia"
tourney_place[tourney_place$tourney == "Oeiras", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Weston", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Esch", "country"] <- "Luxembourg"
tourney_place[tourney_place$tourney == "Forbach", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Harlingen", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Manama", "country"] <- "Bahrain"
tourney_place[tourney_place$tourney == "Trento", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Faro", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Palmanova", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Rosario", "country"] <- "Argentina"
tourney_place[tourney_place$tourney == "Oran", "country"] <- "Algeria"
tourney_place[tourney_place$tourney == "Kamen", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Malaga", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Lakewood", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Colombo", "country"] <- "Colombia"
tourney_place[tourney_place$tourney == "Aldershot", "country"] <- "Sri Lanka"
tourney_place[tourney_place$tourney == "Haren", "country"] <- "Netherlands"
tourney_place[tourney_place$tourney == "Corpus Christi", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Kimberley", "country"] <- "South Africa"
tourney_place[tourney_place$tourney == "La Paz", "country"] <- "Bolivia"
tourney_place[tourney_place$tourney == "Bremen", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Woodside", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Narberth", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Freeport", "country"] <- "Bahamas"
tourney_place[tourney_place$tourney == "Genoa", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Santa Fe", "country"] <- "Argentina"
tourney_place[tourney_place$tourney == "Palo Alto", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Dusseldorf", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "New York", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Kitzbuhel", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "Queen's Club", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Bastad", "country"] <- "Sweden"
tourney_place[tourney_place$tourney == "Stockholm Open", "country"] <- "Sweden"
tourney_place[tourney_place$tourney == "Chorpus Christi", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Merion", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Masters", "country"] <- NA
tourney_place[tourney_place$tourney == "Tanglewood", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Quebec", "country"] <- "Canada"
tourney_place[tourney_place$tourney == "Sacramento", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Cologne", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Hong Kong", "country"] <- "Hong Kong S.A.R."
tourney_place[tourney_place$tourney == "Bretton Woods", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Montreal", "country"] <- "Canada"
tourney_place[tourney_place$tourney == "Gothenberg", "country"] <- "Sweden"
tourney_place[tourney_place$tourney == "Lacosta", "country"] <- NA
tourney_place[tourney_place$tourney == "Djkarta", "country"] <- "Indonesia"
tourney_place[tourney_place$tourney == "Sao Paulo", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Tuscon", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Acapulco", "country"] <- "Mexico"
tourney_place[tourney_place$tourney == "Maui", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Jakarta", "country"] <- "Indonesia"
tourney_place[tourney_place$tourney == "Bahamas", "country"] <- "Bahamas"
tourney_place[tourney_place$tourney == "La Costa", "country"] <- NA
tourney_place[tourney_place$tourney == "Istanbul", "country"] <- "Turkey"
tourney_place[tourney_place$tourney == "Aviles", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "WCT Challenge Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Nuremberg", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Mexico City", "country"] <- "Mexico"
tourney_place[tourney_place$tourney == "Pepsi Grand Slam", "country"] <- NA
tourney_place[tourney_place$tourney == "Bangalore", "country"] <- "India"
tourney_place[tourney_place$tourney == "Tournament of Champions", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Zurich", "country"] <- "Switzerland"
tourney_place[tourney_place$tourney == "Nations Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Wall", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Cape Cod", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Colombus", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Dorado Beach", "country"] <- "Puerto Rico"
tourney_place[tourney_place$tourney == "Parioli", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Ribeiro Preto", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Le Touquet", "country"] <- "France"
tourney_place[tourney_place$tourney == "WCT Invitational", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Washington-", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Frankfurt", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "San Luis Potosi", "country"] <- "Mexico"
tourney_place[tourney_place$tourney == "Shimizu City", "country"] <- "Japan"
tourney_place[tourney_place$tourney == "Cozenza", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Guaruja", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Vina del Mar", "country"] <- "Chile"
tourney_place[tourney_place$tourney == "Mar Del Plata", "country"] <- "Argentina"
tourney_place[tourney_place$tourney == "Ogun", "country"] <- "Nigeria"
tourney_place[tourney_place$tourney == "Rio De La Plata", "country"] <- "Argentina"
tourney_place[tourney_place$tourney == "West Worthing", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Chitchester", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Travemunde", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Ostende", "country"] <- "Belgium"
tourney_place[tourney_place$tourney == "San Benedetto", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Brasilia", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Layetano", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Bahia", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Adelaide-", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Genova", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Lee-On-Solent", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Cap D'Adge", "country"] <- "France"
tourney_place[tourney_place$tourney == "Knokke", "country"] <- "Belgium"
tourney_place[tourney_place$tourney == "Los Angeles-", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Thessaloniki", "country"] <- "Greece"
tourney_place[tourney_place$tourney == "Naples Finals", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Oporto", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Vina Del Mar", "country"] <- "Chile"
tourney_place[tourney_place$tourney == "Boca West", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Stratton Mountain", "country"] <- "United States"
tourney_place[tourney_place$tourney == "West Palm", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Loipersdorf", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "Masters Dec", "country"] <- NA
tourney_place[tourney_place$tourney == "Martinique", "country"] <- "Martinique"
tourney_place[tourney_place$tourney == "Guadeloupe", "country"] <- "Guadeloupe"
tourney_place[tourney_place$tourney == "Bossonnens", "country"] <- "Switzerland"
tourney_place[tourney_place$tourney == "Liege", "country"] <- "Belgium"
tourney_place[tourney_place$tourney == "Itu-Sao Paulo", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Crans Montana", "country"] <- "Switzerland"
tourney_place[tourney_place$tourney == "Rumikon", "country"] <- "Switzerland"
tourney_place[tourney_place$tourney == "Azores", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Nugra Santana", "country"] <- "Singapore"
tourney_place[tourney_place$tourney == "Tasmania", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Ogbe", "country"] <- "Nigeria"
tourney_place[tourney_place$tourney == "Okada", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Troia Setubal", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Hossegor", "country"] <- "France"
tourney_place[tourney_place$tourney == "Chicoutimi", "country"] <- "Canada"
tourney_place[tourney_place$tourney == "Odrimont", "country"] <- "Belgium"
tourney_place[tourney_place$tourney == "Goiania", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Rosmalen", "country"] <- "Netherlands"
tourney_place[tourney_place$tourney == "Canada", "country"] <- "Canada"
tourney_place[tourney_place$tourney == "Long Island", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Ilheus", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Ponte Vedra", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Tour Finals", "country"] <- NA
tourney_place[tourney_place$tourney == "Guam", "country"] <- "Guam"
tourney_place[tourney_place$tourney == "Grand Slam Cup", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Ribeirao", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Maderia", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Reggio Calabri", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Buzios", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Maceio", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Poznan", "country"] <- "Poland"
tourney_place[tourney_place$tourney == "Brunei", "country"] <- "Brunei Darussalam"
tourney_place[tourney_place$tourney == "Sao Luis", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Garmisch", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Riemerling", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Kosice", "country"] <- "Slovakia"
tourney_place[tourney_place$tourney == "Oostende", "country"] <- "Belgium"
tourney_place[tourney_place$tourney == "Reunion Island", "country"] <- "Reunion"
tourney_place[tourney_place$tourney == "Rogaska", "country"] <- "Slovenia"
tourney_place[tourney_place$tourney == "Bermuda", "country"] <- "Bermuda"
tourney_place[tourney_place$tourney == "Oahu", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Annenheim", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "St. Poelten", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "Campos Do Jordao", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Pilzen", "country"] <- "Czech Republic"
tourney_place[tourney_place$tourney == "Ribeirao Preta", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Prostejov", "country"] <- "Czech Republic"
tourney_place[tourney_place$tourney == "Hamburen", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Aruba", "country"] <- "Aruba"
tourney_place[tourney_place$tourney == "Puerta Vallarta", "country"] <- "Mexico"
tourney_place[tourney_place$tourney == "West Bloomfield", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Andijan", "country"] <- "Uzbekistan"
tourney_place[tourney_place$tourney == "Brasov", "country"] <- "Romania"
tourney_place[tourney_place$tourney == "Mallorca", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Tanagura", "country"] <- "Japan"
tourney_place[tourney_place$tourney == "Neumunster", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Mauritius Island", "country"] <- "Mauritius"
tourney_place[tourney_place$tourney == "Portoroz", "country"] <- "Slovenia"
tourney_place[tourney_place$tourney == "Flushing Meadow", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Contrexeville", "country"] <- "France"
tourney_place[tourney_place$tourney == "Portschach", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "Nettingsdorf", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "Eckental", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Kiev", "country"] <- "Ukraine"
tourney_place[tourney_place$tourney == "Netherlands", "country"] <- "Netherlands"
tourney_place[tourney_place$tourney == "Pribram", "country"] <- "Czech Republic"
tourney_place[tourney_place$tourney == "Florianopolis", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Nuembrecht", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Besancon", "country"] <- "France"
tourney_place[tourney_place$tourney == "Cordoba", "country"] <- "Argentina"
tourney_place[tourney_place$tourney == "Germany", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Wroclaw", "country"] <- "Poland"
tourney_place[tourney_place$tourney == "Togliatti", "country"] <- "Russia"
tourney_place[tourney_place$tourney == "Monchengladbach", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Masters Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Luebeck", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Andrezieux", "country"] <- "France"
tourney_place[tourney_place$tourney == "Budaors", "country"] <- "Hungary"
tourney_place[tourney_place$tourney == "Ribeirao Preto", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Costa Do Sauipe", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Tarzana", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Reggio Emilia", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Mordovia", "country"] <- "Russia"
tourney_place[tourney_place$tourney == "St. Jean De Luz", "country"] <- "France"
tourney_place[tourney_place$tourney == "Tumkur", "country"] <- "India"
tourney_place[tourney_place$tourney == "Belgaum", "country"] <- "India"
tourney_place[tourney_place$tourney == "Dnepropetrovsk", "country"] <- "Ukraine"
tourney_place[tourney_place$tourney == "New Caledonia", "country"] <- "New Caledonia"
tourney_place[tourney_place$tourney == "St. Brieuc", "country"] <- "France"
tourney_place[tourney_place$tourney == "Timisoara", "country"] <- "Romania"
tourney_place[tourney_place$tourney == "Dubrovnik", "country"] <- "Croatia"
tourney_place[tourney_place$tourney == "Kish Island", "country"] <- "Iran"
tourney_place[tourney_place$tourney == "Mauritius", "country"] <- "Mauritius"
tourney_place[tourney_place$tourney == "Noumea", "country"] <- "New Caledonia"
tourney_place[tourney_place$tourney == "Southampton", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Chikmagalur", "country"] <- "India"
tourney_place[tourney_place$tourney == "Lanzarote", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Poertschach", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "Constanta", "country"] <- "Romania"
tourney_place[tourney_place$tourney == "Astana", "country"] <- "Kazakhstan"
tourney_place[tourney_place$tourney == "Karshi", "country"] <- "Uzbekistan"
tourney_place[tourney_place$tourney == "Cherkassy", "country"] <- "Ukraine"
tourney_place[tourney_place$tourney == "Tanger", "country"] <- "Morocco"
tourney_place[tourney_place$tourney == "Izmir", "country"] <- "Turkey"
tourney_place[tourney_place$tourney == "Medjugorje", "country"] <- "Bosnia and Herzegovina"
tourney_place[tourney_place$tourney == "Jersey", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Cancun", "country"] <- "Mexico"
tourney_place[tourney_place$tourney == "Japan", "country"] <- "Japan"
tourney_place[tourney_place$tourney == "Khorat", "country"] <- "Thailand"
tourney_place[tourney_place$tourney == "St. Remy", "country"] <- "France"
tourney_place[tourney_place$tourney == "Marburg", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Le Gosier", "country"] <- "Guadeloupe"
tourney_place[tourney_place$tourney == "Pingguo", "country"] <- "China"
tourney_place[tourney_place$tourney == "Sao Jose Do Rio Preto", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Sao Leopoldo", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "ATP Tour Finals", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Mersin", "country"] <- "Turkey"
tourney_place[tourney_place$tourney == "An-Ning", "country"] <- "China"
tourney_place[tourney_place$tourney == "Bercuit", "country"] <- "Belgium"
tourney_place[tourney_place$tourney == "Petange", "country"] <- "Luxembourg"
tourney_place[tourney_place$tourney == "Eskisehir", "country"] <- "Turkey"
tourney_place[tourney_place$tourney == "Guimaraes", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Yeongwol", "country"] <- "South Korea"
tourney_place[tourney_place$tourney == "Cortina", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Batman", "country"] <- "Turkey"
tourney_place[tourney_place$tourney == "St Brieuc", "country"] <- "France"
tourney_place[tourney_place$tourney == "Slovakia", "country"] <- "Slovakia"
tourney_place[tourney_place$tourney == "Braunchweig", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "St Remy", "country"] <- "France"
tourney_place[tourney_place$tourney == "Agri", "country"] <- "Turkey"
tourney_place[tourney_place$tourney == "Potro Alegre", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Challenger Tour Finals", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Jonkoping", "country"] <- "Sweden"
tourney_place[tourney_place$tourney == "Turino", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Anning", "country"] <- "China"
tourney_place[tourney_place$tourney == "St Remy de Provence", "country"] <- "France"
tourney_place[tourney_place$tourney == "Nongbo", "country"] <- "China"
tourney_place[tourney_place$tourney == "Tigre", "country"] <- "Argentina"
tourney_place[tourney_place$tourney == "Sophia Antipolis", "country"] <- "France"
tourney_place[tourney_place$tourney == "Rome ", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Shymkent", "country"] <- "Kazakhstan"
tourney_place[tourney_place$tourney == "Monte Carlo", "country"] <- "Monaco"
tourney_place[tourney_place$tourney == "Masters", "country"] <- NA
tourney_place[tourney_place$tourney == "Lacosta", "country"] <- NA
tourney_place[tourney_place$tourney == "Little Rock", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Bahamas", "country"] <- "Bahamas"
tourney_place[tourney_place$tourney == "La Costa", "country"] <- NA
tourney_place[tourney_place$tourney == "WCT Challenge Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Pepsi Grand Slam", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Nations Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Lafayette", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Stratton Mountain", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Masters Dec", "country"] <- NA
tourney_place[tourney_place$tourney == "Guadeloupe", "country"] <- "Guadeloupe"
tourney_place[tourney_place$tourney == "Chicoutimi", "country"] <- NA
tourney_place[tourney_place$tourney == "Tour Finals", "country"] <- ""
tourney_place[tourney_place$tourney == "Brunei", "country"] <- "Brunei"
tourney_place[tourney_place$tourney == "Aruba", "country"] <- "Aruba"
tourney_place[tourney_place$tourney == "Masters Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Aguascalientes", "country"] <- "Mexico"
tourney_place[tourney_place$tourney == "Suzhou", "country"] <- "China"
tourney_place[tourney_place$tourney == "Nanjing", "country"] <- "China"
tourney_place[tourney_place$tourney == "Antalya", "country"] <- "Turkey"
tourney_place[tourney_place$tourney == "St.Brieuc", "country"] <- "France"
tourney_place[tourney_place$tourney == "Pullach", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Florence ", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Liuzhou", "country"] <- "China"
tourney_place[tourney_place$tourney == "ATP Next Gen Finals", "country"] <- NA
tourney_place[tourney_place$tourney == "Lisboa", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Ludwigshafen", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Yokkaichi", "country"] <- "Japan"
tourney_place[tourney_place$tourney == "Vancouver ", "country"] <- "Canada"
tourney_place[tourney_place$tourney == "Illinois", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Atp Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Koblenz ", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Iasi", "country"] <- "Romania"
tourney_place[tourney_place$tourney == "Forli", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Sardinia", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Doha Aus Open Qualies", "country"] <- NA
tourney_place[tourney_place$tourney == "Great Ocean Road Open", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Villa Maria", "country"] <- "Argentina"
tourney_place[tourney_place$tourney == "Sibenik", "country"] <- "Croatia"
tourney_place[tourney_place$tourney == "Valldoreix", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Brcko", "country"] <- "Bosnia and Herzegovina"
tourney_place[tourney_place$tourney == "Novomoskovsk", "country"] <- "Ukraine"
tourney_place[tourney_place$tourney == "Klosters", "country"] <- "Switzerland"
tourney_place[tourney_place$tourney == "Casinalbo", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Velenje", "country"] <- "Slovenia"
tourney_place[tourney_place$tourney == "Uriage", "country"] <- "France"
tourney_place[tourney_place$tourney == "Pitesti", "country"] <- "Romania"
tourney_place[tourney_place$tourney == "Xativa", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Parnu", "country"] <- "Estonia"
tourney_place[tourney_place$tourney == "Curtea de Arges", "country"] <- "Romania"
tourney_place[tourney_place$tourney == "Luedenscheid", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Bacau", "country"] <- "Romania"
tourney_place[tourney_place$tourney == "Lodz", "country"] <- "Poland"
tourney_place[tourney_place$tourney == "Ueberlingen", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "St. Tropez", "country"] <- "France"
tourney_place[tourney_place$tourney == "Bagneres-De-Bigorre", "country"] <- "France"
tourney_place[tourney_place$tourney == "Ricany", "country"] <- "Czech Republic"
tourney_place[tourney_place$tourney == "Zilina", "country"] <- "Slovakia"
tourney_place[tourney_place$tourney == "Ibague", "country"] <- "Colombia"
tourney_place[tourney_place$tourney == "Davis Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Setubal", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Platja D'Aro", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Loule", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Losinj", "country"] <- "Croatia"
tourney_place[tourney_place$tourney == "Quinta Do Lago", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Erste Bank Open - Vienna", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "Selva Gardena", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Portimao", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Meitar", "country"] <- "Israel"
tourney_place[tourney_place$tourney == "Torello", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Villers Les Nancy", "country"] <- "France"
tourney_place[tourney_place$tourney == "Aparecida de Goiania", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Benicarlo", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Cundinamarca", "country"] <- "Colombia"
tourney_place[tourney_place$tourney == "Guatemala", "country"] <- "Guatemala"
tourney_place[tourney_place$tourney == "Gurugram", "country"] <- "India"
tourney_place[tourney_place$tourney == "Rio Cuarto", "country"] <- "Argentina"
tourney_place[tourney_place$tourney == "Lambare", "country"] <- "Paraguay"
tourney_place[tourney_place$tourney == "Abu Dhabi", "country"] <- "United Arab Emirates"
tourney_place[tourney_place$tourney == "Campos do Jordao", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Bengalaru", "country"] <- "India"
tourney_place[tourney_place$tourney == "Chile Dove Men+Care Open - Santiago", "country"] <- "Chile"
tourney_place[tourney_place$tourney == "Vale do Lobo", "country"] <- "Portugal"
tourney_place[tourney_place$tourney == "Gran Canaria", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Porec", "country"] <- "Croatia"
tourney_place[tourney_place$tourney == "Creteil", "country"] <- "France"
tourney_place[tourney_place$tourney == "Sanremo", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Santa Margherita di Pula", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Mauthausen", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "Salvador De Bahia", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Francavilla", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Warmbad Villach", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "Mataro", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Harmon", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Tay Ninh", "country"] <- "Vietnam"
tourney_place[tourney_place$tourney == "Den Haag", "country"] <- "Netherlands"
tourney_place[tourney_place$tourney == "Herzlia", "country"] <- "Israel"
tourney_place[tourney_place$tourney == "Malmo", "country"] <- "Sweden"
tourney_place[tourney_place$tourney == "Nonthaburi", "country"] <- "Thailand"
tourney_place[tourney_place$tourney == "Lambermont", "country"] <- "Belgium"
tourney_place[tourney_place$tourney == "Szabolcsveresmart", "country"] <- "Hungary"
tourney_place[tourney_place$tourney == "Curacao", "country"] <- "Curacao"
tourney_place[tourney_place$tourney == "WCT World Cup", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Haverford", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Kimberley", "country"] <- "South Africa"
tourney_place[tourney_place$tourney == "Kitzbuehel", "country"] <- "Austria"
tourney_place[tourney_place$tourney == "La Coruna", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Aberavon", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Saltsjoebaden", "country"] <- "Sweden"
tourney_place[tourney_place$tourney == "San Antonio Collegiate", "country"] <- "United States"
tourney_place[tourney_place$tourney == "King's CupR", "country"] <- NA
tourney_place[tourney_place$tourney == "Montana Vermala", "country"] <- "Switzerland"
tourney_place[tourney_place$tourney == "Quebec City", "country"] <- "Canada"
tourney_place[tourney_place$tourney == "Mamaia", "country"] <- "Romania"
tourney_place[tourney_place$tourney == "Madrid Real", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Kings Cup SF", "country"] <- NA
tourney_place[tourney_place$tourney == "Kings Cup Final", "country"] <- NA
tourney_place[tourney_place$tourney == "Cannes Chps", "country"] <- "France"
tourney_place[tourney_place$tourney == "Hanau", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "London-", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "Champions Classic", "country"] <- NA
tourney_place[tourney_place$tourney == "World Invitational Classic", "country"] <- NA
tourney_place[tourney_place$tourney == "NextGen Finals", "country"] <- NA
tourney_place[tourney_place$tourney == "Laver Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "ATP Rio de Janeiro", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Belgrade ", "country"] <- "Serbia"
tourney_place[tourney_place$tourney == "Gijon", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "United Cup", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Santa Cruz De La Sierra", "country"] <- "Bolivia"
tourney_place[tourney_place$tourney == "El Espinar", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Matsuyama", "country"] <- "Japan"
tourney_place[tourney_place$tourney == "Ottignies Louvain", "country"] <- "Belgium"
tourney_place[tourney_place$tourney == "Szekesfehervar", "country"] <- "Hungary"
tourney_place[tourney_place$tourney == "Les Franqueses Del Valles", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Palmas Del Mar", "country"] <- "Puerto Rico"
tourney_place[tourney_place$tourney == "Palo Alto", "country"] <- "United States"
tourney_place[tourney_place$tourney == "Danderyd", "country"] <- "Sweden"
tourney_place[tourney_place$tourney == "Ottignies-Louvain-la-Neuve", "country"] <- "Belgium"
tourney_place[tourney_place$tourney == "San Miguel De Tucuman", "country"] <- "Argentina"
tourney_place[tourney_place$tourney == "Wuxi", "country"] <- "China"
tourney_place[tourney_place$tourney == "Kachreti", "country"] <- "Georgia"

###
## betrachte jetzt noch alle Staedte, die doppelt in df_cities vorkommen

still_na <- sort(unique((tourney_place$tourney[is.na(tourney_place$country)])))

for(i in 1:length(still_na)){
  cat('tourney_place[tourney_place$tourney == "', paste0(still_na[i]), '", "country"] <- ""/n', sep = "")
}

tourney_place[tourney_place$tourney == "Alamo", "country"] <- "USA"
tourney_place[tourney_place$tourney == "Australia", "country"] <- "Australia"
tourney_place[tourney_place$tourney == "Bahrain", "country"] <- "Bahrain"
tourney_place[tourney_place$tourney == "Belem", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Biel-Bienne", "country"] <- "Switzerland"
tourney_place[tourney_place$tourney == "Capetown", "country"] <- "South Africa"
tourney_place[tourney_place$tourney == "Champions Classic", "country"] <- NA
tourney_place[tourney_place$tourney == "Chicoutimi", "country"] <- "Canada"
tourney_place[tourney_place$tourney == "Chitre", "country"] <- "Panama"
tourney_place[tourney_place$tourney == "Clermont Ferrand", "country"] <- "France"
tourney_place[tourney_place$tourney == "Concepcion", "country"] <- "Chile"
tourney_place[tourney_place$tourney == "Francavilla Al Mare", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Kings Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Kings Cup Final", "country"] <- NA
tourney_place[tourney_place$tourney == "Kings Cup SF", "country"] <- NA
tourney_place[tourney_place$tourney == "Kun-Ming", "country"] <- "China"
tourney_place[tourney_place$tourney == "La Costa", "country"] <- "USA"
tourney_place[tourney_place$tourney == "Lacosta", "country"] <- "USA"
tourney_place[tourney_place$tourney == "Laver Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Lubeck", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Macon", "country"] <- "USA"
tourney_place[tourney_place$tourney == "Manzanillo", "country"] <- "Mexico"
tourney_place[tourney_place$tourney == "Mas Palomas", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Masters", "country"] <- NA
tourney_place[tourney_place$tourney == "Masters Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Masters Dec", "country"] <- NA
tourney_place[tourney_place$tourney == "Medellin", "country"] <- "Colombia"
tourney_place[tourney_place$tourney == "Merida", "country"] <- "Mexico"
tourney_place[tourney_place$tourney == "Montemar", "country"] <- "Spain"
tourney_place[tourney_place$tourney == "Nations Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Neu Ulm", "country"] <- "Germany"
tourney_place[tourney_place$tourney == "Next Gen Finals", "country"] <- NA
tourney_place[tourney_place$tourney == "NextGen Finals", "country"] <- NA
tourney_place[tourney_place$tourney == "Punta Del Este", "country"] <- "Uruguay"
tourney_place[tourney_place$tourney == "Raanana", "country"] <- "Israel"
tourney_place[tourney_place$tourney == "Ramat Hasharon", "country"] <- "Israel"
tourney_place[tourney_place$tourney == "Rio", "country"] <- "Brazil"
tourney_place[tourney_place$tourney == "Roseto Degli Abruzzi", "country"] <- "Italy"
tourney_place[tourney_place$tourney == "Spring", "country"] <- "USA"
tourney_place[tourney_place$tourney == "St Petersburg", "country"] <- "Russia"
tourney_place[tourney_place$tourney == "Tampa", "country"] <- "USA"
tourney_place[tourney_place$tourney == "USA", "country"] <- "USA"
tourney_place[tourney_place$tourney == "WCT Challenge Cup", "country"] <- NA
tourney_place[tourney_place$tourney == "Wimbledon", "country"] <- "United Kingdom"
tourney_place[tourney_place$tourney == "World Invitational Classic", "country"] <- NA

#####

# letzte iso3 Codes hinzufuegen
unique_country_codes <- distinct(df_cities, iso3, .keep_all = TRUE)

tourney_place$iso3 <- unique_country_codes$iso3[
  match(tourney_place$country, unique_country_codes$country)
]

table(tourney_place$country[is.na(tourney_place$iso3)])

tourney_place$iso3[tourney_place$country == "Aruba"] <- "ABW"
tourney_place$iso3[tourney_place$country == "Bahamas"] <- "BHS"
tourney_place$iso3[tourney_place$country == "Curacao"] <- "CUW"
tourney_place$iso3[tourney_place$country == "Guadeloupe"] <- "GLP"
tourney_place$iso3[tourney_place$country == "Guam"] <- "GUM"
tourney_place$iso3[tourney_place$country == "Martinique"] <- "MTQ"
tourney_place$iso3[tourney_place$country == "Monaco"] <- "MCO"
tourney_place$iso3[tourney_place$country == "Reunion"] <- "REU"
tourney_place$iso3[tourney_place$country == "USA"] <- "USA"

##

data$tourney_ioc <- tourney_place$iso3

##

ioc_codes <- read.csv("C:/Users/bartm/Documents/Uni/Bachelor/Bachelorarbeit/Bachelorarbeit_Bartmann/Kap_2_Datensatzerstellung/Summer Olympic medalists 1896 to 2008 - IOC COUNTRY CODES.csv")
ioc_codes <- ioc_codes[,c(1,2)]

country_codes <- merge(ioc_codes, unique_country_codes, by.x = "Country", by.y = "country")

colnames(country_codes) <- c("country", "ioc", "iso3", "city")

keep <- rep(TRUE, 177)
for(i in 1:177){
  if(country_codes$ioc[i] == country_codes$iso3[i])
    keep[i] <- FALSE
}

country_codes <- country_codes[keep,] #now only different country_codes 

rownames(country_codes) <- NULL

for(i in 1:76){
  data[which(data$tourney_ioc == country_codes$iso3[i]), "tourney_ioc"] <- country_codes$ioc[i]
}


#####

#setwd("C:/Users/bartm/Documents/HKS/Paper")
#save(data, file = "data.RData")
#load("data.RData")

## To Do: einzelne Turniere, die mehrere Austragungsorte haben nochmal einzeln durchgehen


#### 

## Davis Cup

data[data$tourney_level == "D", ][,]

strsplit(data[data$tourney_level == "D", "tourney_name"][1], " ")[[1]][3] == "Finals"
strsplit(data[data$tourney_level == "D", "tourney_name"][1], " ")[[1]][5]

sort(unique(data[data$tourney_level == "D", "tourney_name"]))

for(i in 1:nrow(data)){
  if(data$tourney_level[i] == "D"){
    if(strsplit(data$tourney_name[i], " ")[[1]][3] == "Finals"){
      # Spanien: ESP
      if(year(data$tourney_date[i]) %in% c(2019:2024, 2011, 2009, 2004, 2000))
        data$tourney_ioc[i] <- "ESP"
      # Frankreich: FRA
      if(year(data$tourney_date[i]) %in% c(2018, 2017, 2014, 2002, 1999, 1991, 1982))
        data$tourney_ioc[i] <- "FRA"
      # Kroatien: CRO
      if(year(data$tourney_date[i]) %in% c(2016))
        data$tourney_ioc[i] <- "CRO"
      # Belgien: BEL
      if(year(data$tourney_date[i]) %in% c(2015))
        data$tourney_ioc[i] <- "BEL"
      # Serbien: SRB
      if(year(data$tourney_date[i]) %in% c(2013, 2010))
        data$tourney_ioc[i] <- "SRB"
      # Tschechien: CZE
      if(year(data$tourney_date[i]) %in% c(2012, 1980))
        data$tourney_ioc[i] <- "CZE"
      # Argentinien: ARG
      if(year(data$tourney_date[i]) %in% c(2008))
        data$tourney_ioc[i] <- "ARG"
      # USA: USA
      if(year(data$tourney_date[i]) %in% c(2007, 1992, 1990, 1981, 1979, 1978, 1973, 1969:1971))
        data$tourney_ioc[i] <- "USA"
      # Russland: RUS
      if(year(data$tourney_date[i]) %in% c(2006, 1995, 1994))
        data$tourney_ioc[i] <- "RUS"
      # Slowakei: SVK
      if(year(data$tourney_date[i]) %in% c(2005))
        data$tourney_ioc[i] <- "SVK"
      # Australien: AUS
      if(year(data$tourney_date[i]) %in% c(2003, 2001, 1986, 1983, 1977, 1968))
        data$tourney_ioc[i] <- "AUS"
      # Italien: ITA
      if(year(data$tourney_date[i]) %in% c(1998))
        data$tourney_ioc[i] <- "ITA"
      # Schweden: SWE
      if(year(data$tourney_date[i]) %in% c(1997, 1996, 1988, 1987, 1984, 1975))
        data$tourney_ioc[i] <- "SWE"
      # Deutschland: GER
      if(year(data$tourney_date[i]) %in% c(1993, 1989, 1985))
        data$tourney_ioc[i] <- "GER"
      # Chile: CHI
      if(year(data$tourney_date[i]) %in% c(1976))
        data$tourney_ioc[i] <- "CHI"
      # Rumänien: ROU
      if(year(data$tourney_date[i]) %in% c(1972))
        data$tourney_ioc[i] <- "ROU"
    } else data$tourney_ioc[i] <- strsplit(data$tourney_name[i], " ")[[1]][5]
  }
}

##HB: es hat sich herausgestellt, dass diese zentrale Finals Location erst 
## ab 2019 eingefuehrt wurde. es sollte aber trotzdem so (ueberwiegend) richtig sein

#####

still_na <- sort(unique(data$tourney_name[is.na(data$tourney_ioc)]))

unique(data$tourney_name[data$tourney_name %in% still_na & data$tourney_date > as.Date("2016-01-01")])
## hier sind auch einige vor 2017; da wir ja wahrscheinlich eh nur Spiele nach 2017 betrachten
## kuemmere ich mich erstmal gesondert um diese 

#  NextGen Finals   
year(data[grep("NextGen Finals", data$tourney_name), "tourney_date"])

data$tourney_ioc[data$tourney_name == "NextGen Finals" & year(data$tourney_date) %in% c(2017:2022)] <- "ITA"
data$tourney_ioc[data$tourney_name == "NextGen Finals" & year(data$tourney_date) %in% c(2023:2025)] <- "KSA"

# Next Gen Finals
year(data[grep("Next Gen Finals", data$tourney_name), "tourney_date"])
data$tourney_ioc[data$tourney_name == "Next Gen Finals"] <- "KSA"

# Laver Cup 
year(data[grep("Laver Cup", data$tourney_name), "tourney_date"])

data$tourney_ioc[data$tourney_name == "Laver Cup" & year(data$tourney_date) == 2017] <- "CZE"
data$tourney_ioc[data$tourney_name == "Laver Cup" & year(data$tourney_date) == 2019] <- "SUI"
data$tourney_ioc[data$tourney_name == "Laver Cup" & year(data$tourney_date) == 2022] <- "GBR"
data$tourney_ioc[data$tourney_name == "Laver Cup" & year(data$tourney_date) == 2023] <- "CAN"
data$tourney_ioc[data$tourney_name == "Laver Cup" & year(data$tourney_date) == 2024] <- "GER"
data$tourney_ioc[data$tourney_name == "Laver Cup" & year(data$tourney_date) %in% c(2018, 2021, 2025)] <- "USA"

# Atp Cup
year(data[grep("Atp Cup", data$tourney_name), "tourney_date"])
data$tourney_ioc[data$tourney_name == "Atp Cup"] <- "AUS"

# Tour Finals
sort(unique(year(data[grep("Tour Finals", data$tourney_name), "tourney_date"])))
# 1990-1999, 2009-2024
data$tourney_ioc[data$tourney_name == "Tour Finals" & year(data$tourney_date) %in% 1990:1999] <- "GER"
data$tourney_ioc[data$tourney_name == "Tour Finals" & year(data$tourney_date) %in% 2009:2020] <- "GBR"
data$tourney_ioc[data$tourney_name == "Tour Finals" & year(data$tourney_date) %in% 2021:2025] <- "ITA"

## HB: Die locations ab 2017 haben jetzt alle einen IOC, falls irgendwann die Zeit ist, kann ich noch die anderen hinzufuegen
## falls hier noch was dran geaendert wird, muss der nachfolgende Code nochmal ausgefuerhrt werden!


#####

#setwd("C:/Users/bartm/Documents/HKS/Paper")
#save(data, file = "data.RData")
#load("data.RData")

#####

## fuege home effect Variable hinzu!
# 0 wird auch angenommen, wenn eine der Nationalitaeten oder der Ort nicht bekannt war

# Initialisiere die Spalte
data$home_tourney <- 0

# Player hat Heimvorteil
player_home <- !is.na(data$player_ioc) & !is.na(data$tourney_ioc) &
  data$player_ioc == data$tourney_ioc
data$home_tourney[player_home] <- 1

# Opponent hat Heimvorteil
opponent_home <- !is.na(data$opponent_ioc) & !is.na(data$tourney_ioc) &
  data$opponent_ioc == data$tourney_ioc
data$home_tourney[opponent_home] <- -1

# # Beide haben denselben Heimatcode → überschreiben mit 0
# both_home <- !is.na(data$player_ioc) & !is.na(data$opponent_ioc) &
#   data$player_ioc == data$opponent_ioc
# data$home_tourney[both_home] <- 0

table(data$home_tourney, useNA = "ifany") 


#####

#setwd("C:/Users/bartm/Documents/HKS/Paper")
#save(data, file = "data.RData")
#load("data.RData")

#####

still_na <- sort(unique(data$tourney_name[is.na(data$tourney_ioc)]))

unique(data$tourney_name[data$tourney_name %in% still_na])

# die koennte man noch irgendwann hinzufuegen 








#####

#setwd("C:/Users/bartm/Documents/HKS/Paper")
#save(data, file = "data.RData")
#load("data.RData")

######

## entferne alle Matches, wo das Surface unbekannt ist 
data <- data[!is.na(data$surface),]


## Implementiere H2H ueber alle surfaces

# Sortiere nach Datum (chronologische Reihenfolge)
data <- data[order(data$tourney_date), ]

# Für jeden Matchup eine Richtung definieren:
# "forward" bedeutet player_id < opponent_id
forward <- data$player_id < data$opponent_id

# Gewinnwert aus Sicht des kleineren IDs:
score_val <- ifelse(forward, 2 * data$win - 1,
                    1 - 2 * data$win)

# Jetzt gruppenweise kumulative Summen bilden innerhalb jeder matchup_id
data$H2H <- ave(
  score_val,
  data$matchup_id,
  FUN = function(x) cumsum(x) - x   # alle bisherigen Spiele summieren (l < i)
)

# Wenn "forward" FALSE ist (also Spielerrolle vertauscht),
# muss das Vorzeichen umgedreht werden, damit die Perspektive stimmt:
data$H2H[!forward] <- -data$H2H[!forward]

#View(data[sort(c(which(data$player_name == "Jannik Sinner"), which(data$opponent_name == "Jannik Sinner"))),])
#View(data[data$matchup_id == "106421:206173",])


### H2H für Surface
levels(data$surface)

## Carpet
# Filter: nur Spiele auf Carpet
idx_carpet <- data$surface == "Carpet"

# Kumulative Summe innerhalb jeder matchup_id für Carpet-Matches
h2h_carpet <- ave(
  score_val[idx_carpet],
  data$matchup_id[idx_carpet],
  FUN = function(x) cumsum(x) - x   # nur frühere Spiele derselben Paarung & Surface
)

# Perspektive anpassen: wenn 'forward' FALSE ist → Vorzeichen umdrehen
h2h_carpet[!forward[idx_carpet]] <- -h2h_carpet[!forward[idx_carpet]]

# Neue Spalte hinzufügen (Standardwert = 0)
data$H2H_carpet <- 0

# Werte für Carpet einfügen
data$H2H_carpet[idx_carpet] <- h2h_carpet

## Clay
idx_clay <- data$surface == "Clay"

h2h_clay <- ave(
  score_val[idx_clay],
  data$matchup_id[idx_clay],
  FUN = function(x) cumsum(x) - x 
)

h2h_clay[!forward[idx_clay]] <- -h2h_clay[!forward[idx_clay]]

data$H2H_clay <- 0

data$H2H_clay[idx_clay] <- h2h_clay


## Grass
idx_grass <- data$surface == "Grass"

h2h_grass <- ave(
  score_val[idx_grass],
  data$matchup_id[idx_grass],
  FUN = function(x) cumsum(x) - x 
)

h2h_grass[!forward[idx_grass]] <- -h2h_grass[!forward[idx_grass]]

data$H2H_grass <- 0

data$H2H_grass[idx_grass] <- h2h_grass


## Hard
idx_hard <- data$surface == "Hard"

h2h_hard <- ave(
  score_val[idx_hard],
  data$matchup_id[idx_hard],
  FUN = function(x) cumsum(x) - x  
)

h2h_hard[!forward[idx_hard]] <- -h2h_hard[!forward[idx_hard]]

data$H2H_hard <- 0

data$H2H_hard[idx_hard] <- h2h_hard

#####

#setwd("C:/Users/bartm/Documents/HKS/Paper")
#save(data, file = "data.RData")
#load("data.RData")

#####

## fuehre H2H fuer die verschiedenen Untergruende in eine Variable zusammen 

data$H2H_surface <- NA

data[data$surface == "Carpet", "H2H_surface"] <- data[data$surface == "Carpet", "H2H_carpet"]
data[data$surface == "Clay", "H2H_surface"] <- data[data$surface == "Clay", "H2H_clay"]
data[data$surface == "Grass", "H2H_surface"] <- data[data$surface == "Grass", "H2H_grass"]
data[data$surface == "Hard", "H2H_surface"] <- data[data$surface == "Hard", "H2H_hard"]

data$H2H_carpet <- NULL
data$H2H_clay <- NULL
data$H2H_grass <- NULL
data$H2H_hard <- NULL

#####

#setwd("C:/Users/bartm/Documents/HKS/Paper")
#save(data, file = "data.RData")
#load("data.RData")
