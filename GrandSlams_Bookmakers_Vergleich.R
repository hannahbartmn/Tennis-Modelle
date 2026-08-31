# fuer die Sachen hier brauche ich nur die Daten von 2024

## lade Bookmaker Odds
library("readxl")
library("tidyverse")

load("data.RData")

odds <- read_excel("2024.xlsx")
odds <- odds %>% select(Location, Tournament, Date, Series, Surface, Winner, Loser,  
                                  B365W, B365L, PSW, PSL, MaxW, MaxL, AvgW, AvgL) 
odds <- odds[odds$Series == "Grand Slam",]

################################################################################

## lade Vorhersagen 
setwd(paste0(getwd(), "/GS_O_new_model_prediction"))
temp <- data.frame(matrix(NA, 1, 8))
colnames(temp) <- c("true_outcome", "pred_outcome1", "pred_outcome2", "pred_outcome3", 
                    "pred_outcome4", "pred_outcome5", "pred_outcome6", "pred_outcome7")

#for(i in 1:71){
for(i in 1:60){
  load(paste0("GS_O_new_model_prediction_", i, ".RData"))
  temp <- rbind(temp, prediction)
}
prediction <- temp[-1,]
rm(temp, i)

#all_tourneys <- unique(data[data$tourney_date >= max(data$tourney_date) - 365, 1])
data <- data[data$tourney_level %in% c("O", "G"),]
all_tourneys <- unique(data[data$tourney_date >= as.Date("2011-01-01"), 1])
all_tourney_dates <- unique(data[data$tourney_id %in% all_tourneys, "tourney_date"])

# nrow(data[data$tourney_date %in% all_tourney_dates, ])
# nrow(data[data$tourney_id %in% all_tourneys, ])

data$pred5_newModel <- data$pred2_newModel <- NA

data[data$tourney_date %in% all_tourney_dates, "pred2_newModel"] <- prediction$pred_outcome2
data[data$tourney_date %in% all_tourney_dates, "pred5_newModel"] <- prediction$pred_outcome5

## auf diese Weise koennen noch die Vorhersagen der anderen Modelle mit aufgenommen werden 


################################################################################

## Verbeinde odds Datensatz und Grandslam Datensatz

#lade Tennis Datensatz und berechne die fuenf Modelle fuer die Grandslams 

grandslams <- data[data$tourney_level == "G" & data$tourney_date >= as.Date("2024-01-01"),]

library(dplyr)
library(stringr)

clean_name <- function(x){
  x <- tolower(x)
  x <- gsub("[^a-z ]", "", x)   # entfernt Punkte, Apostrophe usw.
  x <- str_squish(x)
  return(x)
}

# Funktion: "Alexei Popyrin" -> "popyrin a"
short_name <- function(name){
  sapply(strsplit(name, " "), function(x){
    paste(tail(x,1), substr(x[1],1,1))
  })
}

# Player-Info erstellen
player_info <- data.frame(
  id = c(grandslams$player_id, grandslams$opponent_id),
  name = c(grandslams$player_name, grandslams$opponent_name)
) %>%
  distinct()

# Normalisierte Namen erzeugen
player_info$short <- short_name(player_info$name)
player_info$short_clean <- clean_name(player_info$short)

# Odds-Namen normalisieren
odds$Winner_clean <- clean_name(gsub("\\.", "", odds$Winner))
odds$Loser_clean  <- clean_name(gsub("\\.", "", odds$Loser))

# Winner IDs zuordnen
odds <- odds %>%
  left_join(player_info[,c("id","short_clean")],
            by = c("Winner_clean" = "short_clean")) %>%
  rename(Winner_ID = id)

# Loser IDs zuordnen
odds <- odds %>%
  left_join(player_info[,c("id","short_clean")],
            by = c("Loser_clean" = "short_clean")) %>%
  rename(Loser_ID = id)

odds <- as.data.frame(odds)

# Problemfälle anzeigen
# sort(unique(odds[is.na(odds$Winner_ID), "Winner"]))
# sort(unique(odds[is.na(odds$Loser_ID), "Loser"]))

odds[odds$Winner == "O Connell C.", "Winner_ID"] <- player_info[player_info$name == "Christopher Oconnell", "id"]
odds[odds$Loser == "O Connell C.", "Loser_ID"] <- player_info[player_info$name == "Christopher Oconnell", "id"]

odds[odds$Winner == "De Jong J.", "Winner_ID"] <- player_info[player_info$name == "Jesper De Jong", "id"]
odds[odds$Loser == "De Jong J.", "Loser_ID"] <- player_info[player_info$name == "Jesper De Jong", "id"]

odds[odds$Winner == "Van De Zandschulp B.", "Winner_ID"] <- player_info[player_info$name == "Botic Van De Zandschulp", "id"]
odds[odds$Loser == "Van De Zandschulp B.", "Loser_ID"] <- player_info[player_info$name == "Botic Van De Zandschulp", "id"]

odds[odds$Winner == "Van De Zandschulp B.", "Winner_ID"] <- player_info[player_info$name == "Botic Van De Zandschulp", "id"]
odds[odds$Loser == "Van De Zandschulp B.", "Loser_ID"] <- player_info[player_info$name == "Botic Van De Zandschulp", "id"]

odds[odds$Winner == "De Minaur A.", "Winner_ID"] <- player_info[player_info$name == "Alex De Minaur", "id"]
odds[odds$Loser == "De Minaur A.", "Loser_ID"] <- player_info[player_info$name == "Alex De Minaur", "id"]

odds[odds$Winner == "Van Assche L.", "Winner_ID"] <- player_info[player_info$name == "Luca Van Assche", "id"]
odds[odds$Loser == "Van Assche L.", "Loser_ID"] <- player_info[player_info$name == "Luca Van Assche", "id"]

odds[odds$Winner == "Auger-Aliassime F.", "Winner_ID"] <- player_info[player_info$name == "Felix Auger Aliassime", "id"] 
odds[odds$Loser == "Auger-Aliassime F.", "Loser_ID"] <- player_info[player_info$name == "Felix Auger Aliassime", "id"] 

odds[odds$Winner == "Bautista Agut R.", "Winner_ID"] <- player_info[player_info$name == "Roberto Bautista Agut", "id"] 
odds[odds$Loser == "Bautista Agut R.", "Loser_ID"] <- player_info[player_info$name == "Roberto Bautista Agut", "id"] 

odds[odds$Loser == "Bu Y.", "Loser_ID"] <- player_info[player_info$name == "Bu Yunchaokete", "id"] 

odds[odds$Winner == "Carballes Baena R.", "Winner_ID"] <- player_info[player_info$name == "Roberto Carballes Baena", "id"] 
odds[odds$Loser == "Carballes Baena R.", "Loser_ID"] <- player_info[player_info$name == "Roberto Carballes Baena", "id"] 

odds[odds$Loser == "Carreno Busta P.", "Loser_ID"] <- player_info[player_info$name == "Pablo Carreno Busta", "id"] 

odds[odds$Winner == "Davidovich Fokina A.", "Winner_ID"] <- player_info[player_info$name == "Alejandro Davidovich Fokina", "id"] 
odds[odds$Loser == "Davidovich Fokina A.", "Loser_ID"] <- player_info[player_info$name == "Alejandro Davidovich Fokina", "id"] 

odds[odds$Winner == "Diaz Acosta F.", "Winner_ID"] <- player_info[player_info$name == "Facundo Diaz Acosta", "id"] 
odds[odds$Loser == "Diaz Acosta F.", "Loser_ID"] <- player_info[player_info$name == "Facundo Diaz Acosta", "id"] 

odds[odds$Winner == "Galan D.E.", "Winner_ID"] <- player_info[player_info$name == "Daniel Elahi Galan", "id"] 
odds[odds$Loser == "Galan D.E.", "Loser_ID"] <- player_info[player_info$name == "Daniel Elahi Galan", "id"] 

odds[odds$Loser == "Herbert P.H.", "Loser_ID"] <- player_info[player_info$name == "Pierre Hugues Herbert", "id"] 

odds[odds$Winner == "Kwon S.W.", "Winner_ID"] <- player_info[player_info$name == "Soon Woo Kwon", "id"] 
odds[odds$Loser == "Kwon S.W.", "Loser_ID"] <- player_info[player_info$name == "Soon Woo Kwon", "id"] 

odds[odds$Loser == "Meligeni Alves F.", "Loser_ID"] <- player_info[player_info$name == "Felipe Meligeni Alves", "id"] 

odds[odds$Loser == "Moreno De Alboran N.", "Loser_ID"] <- player_info[player_info$name == "Nicolas Moreno De Alboran", "id"] 

odds[odds$Loser == "Moro Canas A.", "Loser_ID"] <- player_info[player_info$name == "Alejandro Moro Canas", "id"] 

odds[odds$Winner == "Mpetshi G.", "Winner_ID"] <- player_info[player_info$name == "Giovanni Mpetshi Perricard", "id"] 
odds[odds$Loser == "Mpetshi G.", "Loser_ID"] <- player_info[player_info$name == "Giovanni Mpetshi Perricard", "id"] 

odds[odds$Loser == "Ramos-Vinolas A.", "Loser_ID"] <- player_info[player_info$name == "Albert Ramos", "id"] 

odds[odds$Winner == "Seyboth Wild T.", "Winner_ID"] <- player_info[player_info$name == "Thiago Seyboth Wild", "id"] 
odds[odds$Loser == "Seyboth Wild T.", "Loser_ID"] <- player_info[player_info$name == "Thiago Seyboth Wild", "id"] 

odds[odds$Winner == "Struff J.L.", "Winner_ID"] <- player_info[player_info$name == "Jan Lennard Struff", "id"] 
odds[odds$Loser == "Struff J.L.", "Loser_ID"] <- player_info[player_info$name == "Jan Lennard Struff", "id"] 

odds[odds$Loser == "Tirante T.A.", "Loser_ID"] <- player_info[player_info$name == "Thiago Agustin Tirante", "id"] 

odds[odds$Loser == "Ugo Carabelli C.", "Loser_ID"] <- player_info[player_info$name == "Camilo Ugo Carabelli", "id"] 

odds[odds$Loser == "Varillas J. P.", "Loser_ID"] <- player_info[player_info$name == "Juan Pablo Varillas", "id"] 

odds[odds$Loser == "Wolf J.J.", "Loser_ID"] <- player_info[player_info$name == "J J Wolf", "id"] 

odds[odds$Loser == "Zapata Miralles B.", "Loser_ID"] <- player_info[player_info$name == "Bernabe Zapata Miralles", "id"] 

odds[odds$Winner == "Zhang Zh.", "Winner_ID"] <- player_info[player_info$name == "Zhizhen Zhang", "id"] 
odds[odds$Loser == "Zhang Zh.", "Loser_ID"] <- player_info[player_info$name == "Zhizhen Zhang", "id"] 

# winner <- sort(unique(odds[is.na(odds$Winner_ID), "Winner"]))
# loser <- sort(unique(odds[is.na(odds$Loser_ID), "Loser"]))
# 
# for(i in loser){
#   if(i %in% winner)
#     cat(paste0('odds[odds$Winner == "', i, '", "Winner_ID"] <- player_info[player_info$name == , "id"]'), "\n")
#   
#   cat(paste0('odds[odds$Loser == "', i, '", "Loser_ID"] <- player_info[player_info$name == , "id"]'), "\n\n")
# }

rm(i, loser, winner, clean_name, short_name, player_info)
odds$Winner_clean <- NULL
odds$Loser_clean <- NULL


################################################################################

grandslams$Avg_opponent <- grandslams$Avg_player <- grandslams$B365_opponent <- grandslams$B365_player <- numeric(956)

unique(odds$Tournament)
unique(grandslams$tourney_name)
odds$Tournament <- gsub("French Open", "Roland Garros", odds$Tournament)
odds$Tournament <- gsub("US Open", "Us Open", odds$Tournament)



grandslams <- grandslams %>%
  mutate(
    Winner_ID = ifelse(win == 1, player_id, opponent_id),
    Loser_ID  = ifelse(win == 1, opponent_id, player_id)
  ) %>%
  left_join(odds,
            by = c("tourney_name" = "Tournament",
                   "Winner_ID",
                   "Loser_ID")) %>%
  mutate(
    Avg_player = ifelse(win == 1, AvgW, AvgL),
    Avg_opponent = ifelse(win == 1, AvgL, AvgW),
    B365_player = ifelse(win == 1, B365W, B365L),
    B365_opponent = ifelse(win == 1, B365L, B365W)
  ) %>%
  select(-Winner_ID, -Loser_ID)


################################################################################

grandslams <- na.omit(grandslams)

grandslams$Winner <- NULL
grandslams$Loser <- NULL
grandslams$Location <- NULL
grandslams$Date <- NULL
grandslams$Series <- NULL
grandslams$Surface <- NULL
grandslams$B365W <- NULL
grandslams$B365L <- NULL
grandslams$PSW <- NULL
grandslams$PSL <- NULL
grandslams$MaxW <- NULL
grandslams$MaxL <- NULL
grandslams$AvgW <- NULL
grandslams$AvgL <- NULL
grandslams$AvgL <- NULL
grandslams$AvgW <- NULL
grandslams$PSW <- NULL

################################################################################

# berechne Gewinnwk aus den Bookmaker Odds -> Gewinnwk fuer player

grandslams$pred_odds <- (1 / grandslams$Avg_player) / ((1 / grandslams$Avg_player) + (1 / grandslams$Avg_opponent)) 

################################################################################

### Betting Profit
grandslams$betting_return_opponent <- grandslams$betting_return_player <- numeric(508)

grandslams$betting_return_player <- grandslams$pred5_newModel * grandslams$B365_player - 1
grandslams$betting_return_opponent <- (1 - grandslams$pred5_newModel) * grandslams$B365_opponent - 1

grandslams$betting_on <- "neither"
for(i in 1:nrow(grandslams)){
  if(grandslams$betting_return_player[i] > grandslams$betting_return_opponent[i] & grandslams$betting_return_player[i] > 0)
    grandslams$betting_on[i] <- "player"
  if(grandslams$betting_return_player[i] < grandslams$betting_return_opponent[i] & grandslams$betting_return_opponent[i] > 0)
    grandslams$betting_on[i] <- "opponent"
}

betting_profit <- 0
for(i in 1:nrow(grandslams)){ 
  if(grandslams$betting_on[i] != "neither"){
    if(grandslams$betting_on[i] == "player" & grandslams$win[i] == 1){
      betting_profit <- betting_profit + grandslams$B365_player[i] - 1
    }
    if(grandslams$betting_on[i] == "player" & grandslams$win[i] == 0){
      betting_profit <- betting_profit - 1
    }
    if(grandslams$betting_on[i] == "opponent" & grandslams$win[i] == 0){
      betting_profit <- betting_profit + grandslams$B365_opponent[i] - 1
    }
    if(grandslams$betting_on[i] == "opponent" & grandslams$win[i] == 1){
      betting_profit <- betting_profit  - 1
    }
  }
}
betting_profit
#-59.94

#number of bets:
sum(grandslams$betting_on != "neither") #417

#number of positive bets
sum(grandslams$betting_on == "player" & grandslams$win[i] == 1) + sum(grandslams$betting_on == "opponent" & grandslams$win[i] == 0)
#210

#number of negative bets
sum(grandslams$betting_on == "player" & grandslams$win[i] == 0) + sum(grandslams$betting_on == "opponent" & grandslams$win[i] == 1)
#207

######

grandslams$betting_return_opponent <- grandslams$betting_return_player <- numeric(508)

grandslams$betting_return_player <- grandslams$pred2_newModel * grandslams$B365_player - 1
grandslams$betting_return_opponent <- (1 - grandslams$pred2_newModel) * grandslams$B365_opponent - 1

grandslams$betting_on <- "neither"
for(i in 1:nrow(grandslams)){
  if(grandslams$betting_return_player[i] > grandslams$betting_return_opponent[i] & grandslams$betting_return_player[i] > 0)
    grandslams$betting_on[i] <- "player"
  if(grandslams$betting_return_player[i] < grandslams$betting_return_opponent[i] & grandslams$betting_return_opponent[i] > 0)
    grandslams$betting_on[i] <- "opponent"
}

betting_profit <- 0
for(i in 1:nrow(grandslams)){ 
  if(grandslams$betting_on[i] != "neither"){
    if(grandslams$betting_on[i] == "player" & grandslams$win[i] == 1){
      betting_profit <- betting_profit + grandslams$B365_player[i] - 1
    }
    if(grandslams$betting_on[i] == "player" & grandslams$win[i] == 0){
      betting_profit <- betting_profit - 1
    }
    if(grandslams$betting_on[i] == "opponent" & grandslams$win[i] == 0){
      betting_profit <- betting_profit + grandslams$B365_opponent[i] - 1
    }
    if(grandslams$betting_on[i] == "opponent" & grandslams$win[i] == 1){
      betting_profit <- betting_profit  - 1
    }
  }
}
betting_profit
#-68.18

#number of bets:
sum(grandslams$betting_on != "neither") #422

#number of positive bets
sum(grandslams$betting_on == "player" & grandslams$win[i] == 1) + sum(grandslams$betting_on == "opponent" & grandslams$win[i] == 0)
#212

#number of negative bets
sum(grandslams$betting_on == "player" & grandslams$win[i] == 0) + sum(grandslams$betting_on == "opponent" & grandslams$win[i] == 1)
#210


## Fazit: bezueglich der Betting Strategie ist das fuenfte Modell ein bisschen besser, aber auch nur ein bisschen

################################################################################

library(xtable)

pred_table <- matrix(NA, ncol = 3, nrow = 3)
pred_table <- data.frame(pred_table)
colnames(pred_table) <- c("model2", "model5", "Bookmaker_Odds")
rownames(pred_table) <- c("Class._Rate", "Likelihood", "Brier_Score")

true_outcome <- grandslams$win

#classification rate
pred_vec <- ifelse(grandslams$pred2_newModel > 0.5, 1, 0)
pred_table[1, 1] <- mean(true_outcome == pred_vec) 
pred_vec <- ifelse(grandslams$pred5_newModel > 0.5, 1, 0)
pred_table[1, 2] <- mean(true_outcome == pred_vec) 
pred_vec <- ifelse(grandslams$pred_odds > 0.5, 1, 0)
pred_table[1, 3] <- mean(true_outcome == pred_vec)


#predictive bernoulli likelihood 
pred_vec <- grandslams$pred2_newModel
pred_table[2, 1] <- mean(pred_vec^true_outcome * (1 - pred_vec)^(1 - true_outcome), na.rm = TRUE)
pred_vec <- grandslams$pred5_newModel
pred_table[2, 2] <- mean(pred_vec^true_outcome * (1 - pred_vec)^(1 - true_outcome), na.rm = TRUE)
pred_vec <- grandslams$pred_odds
pred_table[2, 3] <- mean(pred_vec^true_outcome * (1 - pred_vec)^(1 - true_outcome), na.rm = TRUE)


#Brier Score
pred_vec <- grandslams$pred2_newModel
pred_table[3, 1] <- mean((pred_vec - true_outcome)^2)
pred_vec <- grandslams$pred5_newModel
pred_table[3, 2] <- mean((pred_vec - true_outcome)^2)
pred_vec <- grandslams$pred_odds
pred_table[3, 3] <- mean((pred_vec - true_outcome)^2)

#build latex table
xtable(t(pred_table), digits = 4)

################################################################################

