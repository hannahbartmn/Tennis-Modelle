# library(devtools)
# install_github("hannahbartmn/ATP.ranking")
library(ATP.ranking)

data("tennis_data")

######################################
# Cleanup Datensatz 

library(dplyr)

#tennis_data[tennis_data$tourney_id == "2022-8-5517" & 
#              tennis_data$player_id == 309032 & tennis_data$opponent_id == 310653,]

tennis_data <- tennis_data %>%
  distinct(tourney_id, player_id, opponent_id, .keep_all = TRUE)


tennis_data <- tennis_data[tennis_data$tourney_date >= as.Date("2024-01-01"),]


##### H2H implementieren 

# # Beispielstruktur: df mit player1, player2, winner, match_id oder date
# df <- data.frame(
#   match_id = 1:7,
#   player1 = c("A","A","B","C","A","B", "B"), #player_id
#   player2 = c("B","C","C","A","B","A", "A"), #opponent_id
#   winner  = c("A","A","C","A","B","A", "A") #muss noch implementiert werden!
# )

#fuege match_id dem Datensatz hinzu 
tennis_data$match_id <- 1:nrow(tennis_data)

#fuege winner dem Datensatz hinzu
tennis_data <- tennis_data %>%
  mutate(
    winner_id = if_else(win == 1, player_id, opponent_id)
  )

#bestimme H2H
tennis_data_h2h <- tennis_data %>%
  rowwise() %>%
  mutate(
    wins_player_id = sum(
      (tennis_data$winner == player_id) &
        ((tennis_data$player_id == opponent_id & tennis_data$opponent_id == player_id) |
           (tennis_data$player_id == player_id & tennis_data$opponent_id == opponent_id)) &
        tennis_data$match_id < match_id
    ),
    wins_opponent_id = sum(
      (tennis_data$winner == opponent_id) &
        ((tennis_data$player_id == player_id & tennis_data$opponent_id == opponent_id) |
           (tennis_data$player_id == opponent_id & tennis_data$opponent_id == player_id)) &
        tennis_data$match_id < match_id
    )
  ) %>%
  ungroup()
