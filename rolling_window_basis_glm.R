## Gewichtsfunktionen

time_weight <- function(d, halfperiod) return(0.5^(d / halfperiod))

level_weight <- function(lv){
  weight <- NA
  if(lv == "G") weight <- 4
  if(lv == "O") weight <- 3.5
  if(lv %in% c("F", "M")) weight <- 3
  if(lv == "D") weight <- 2
  if(lv %in% c("A", "C")) weight <- 1
  return(weight)
}

#####

## Pakete

library(tidyverse)
# library(glmnet)


## Daten

load("data.RData")

##### 

## Datenvorbereitung 

# zu vorhersagende Turniere aus dem letzten Jahr 
all_tourneys <- unique(data[data$tourney_date >= max(data$tourney_date) - 365, 1])
# unique(data[data$tourney_id %in% all_tourneys, "tourney_name"]) # 366 Turniere 

# table(data[data$tourney_id %in% all_tourneys, "tourney_level"]) # alle Turnierlevel sind vorhanden

# table(data[data$tourney_id %in% all_tourneys, "tourney_date"]) 
all_tourney_dates <- unique(data[data$tourney_id %in% all_tourneys, "tourney_date"])
# 71 Turnierdaten 

#####

## Modellvorbereitung

# j <- 5
# j <- as.integer(Sys.getenv("PBS_ARRAYID"))
J <- c(42, 44, 47, 53, 56)

j <- J[5]

for(j in J){
  
  cat(j, "at", paste0(Sys.time()), "\n")

  # Datum des j-ten Turniers 
  temp_date <- all_tourney_dates[j]
  
  # Zeitdifferenz Update
  data$date_difference <- as.numeric(temp_date - data$tourney_date)
  
  
  #####  models with 4 years training data #######################################
  
  ## alle Turniere 4 Jahre vor dem zu schaetzenden Turnier auswaehlen
  temp_id_4_years <- data[(data$tourney_date < temp_date & 
                             data$tourney_date > temp_date - 365*4), 1] 
  temp_subset_4_years <- data[data$tourney_id %in% temp_id_4_years, ]
  na_surface <- is.na(temp_subset_4_years$surface)
  temp_subset_4_years <- temp_subset_4_years[!na_surface,]
  
  
  ## Outlier bestimmen
  
  # alle Spiele aus Perspektive von Spieler 1
  df_player <- temp_subset_4_years[, c("player_name", "win")]
  names(df_player) <- c("name", "win")
  
  # alle Spiele aus der Perspektive von Spieler 2 (win = inverted)
  df_opponent <- temp_subset_4_years[, c("opponent_name", "win")]
  df_opponent$win <- 1 - df_opponent$win
  names(df_opponent) <- c("name", "win")
  
  # Merge beide Datensaetze
  all_matches <- rbind(df_player, df_opponent)
  
  # fuer jeden Spieler ueberpruefen, ob er gewinnt UND verliert 
  player_stats <- aggregate(win ~ name, data = all_matches, 
                            FUN = function(x) length(unique(x)))
  
  # Outlier haben nur gewonnen ODER nur verloren
  outlier <- player_stats$name[player_stats$win == 1]
  
  
  ## Outlier im Datensatz ueberschreiben mit einer gemeinsamen ID
  temp_subset_4_years[temp_subset_4_years$player_name %in% outlier, "player_id"] <- 400000 
  temp_subset_4_years[temp_subset_4_years$player_name %in% outlier, "player_name"] <- "Outlier" 
  
  temp_subset_4_years[temp_subset_4_years$opponent_name %in% outlier, "opponent_id"] <- 400000
  temp_subset_4_years[temp_subset_4_years$opponent_name %in% outlier, "opponent_name"] <- "Outlier"
  

  ## alle einzigartigen Spieler  
  player_info_4_years <- data.frame(id = temp_subset_4_years$player_id, 
                                    name = temp_subset_4_years$player_name)
  player_info_4_years <- rbind(player_info_4_years, data.frame(id = temp_subset_4_years$opponent_id, 
                                                               name = temp_subset_4_years$opponent_name))
  player_info_4_years <- distinct(player_info_4_years)
  
  
  ## Designmatrix vorbereiten 
  temp_subset_4_years$player_id <- factor(temp_subset_4_years$player_id, 
                                          levels = player_info_4_years$id, 
                                          labels = player_info_4_years$name)
  temp_subset_4_years$player_id <- relevel(temp_subset_4_years$player_id, "Outlier")
  temp_subset_4_years$opponent_id <- factor(temp_subset_4_years$opponent_id, 
                                            levels = player_info_4_years$id, 
                                            labels = player_info_4_years$name)
  temp_subset_4_years$opponent_id <-  relevel(temp_subset_4_years$opponent_id, "Outlier")
  
  # Spiele entfernen, in denen Outlier gegen outlier spielt
  index <- which(temp_subset_4_years$player_id == "Outlier" & temp_subset_4_years$opponent_id == "Outlier")
  if(length(index) > 0){
    temp_subset_4_years <- temp_subset_4_years[-index,]
  }
  
  
  ## Designmatrix aufstellen
  X_1 <- model.matrix(~ -1 + player_id, data = temp_subset_4_years)
  X_2 <- model.matrix(~ -1 + opponent_id, data = temp_subset_4_years) 
  X <- cbind(temp_subset_4_years$home_tourney, X_1-X_2)
  colnames(X)[1] <- "home"
  
  y <- as.numeric(temp_subset_4_years$win) # numeric fuer glmnet
  
  # Outlier SPalte entfernen
  X <- X[,-2]
  
  # in matrix umwandeln fuer glmnet
  # X <- as.matrix(X)
  
  X_4_years_colnames <- colnames(X)
  
  X <- as.data.frame(X)
  
  ######
  
  ## Vorbereitung von Schaetzungen mit 4 Jahren Trainingsdaten
  
  pred_subset <- data[data$tourney_date == all_tourney_dates[j],]
  
  # true outcome abspeichern
  true_outcome <- as.numeric(pred_subset$win)
  
  # Spieler ohne Parameterschaetzung werden mit Outlier Parameter geschaetzt
  not_listed_player <- which(!(pred_subset$player_id %in% player_info_4_years$id))
  not_listed_player <- pred_subset$player_id[not_listed_player]
  
  if(length(not_listed_player) > 0){
    pred_subset[pred_subset$player_id %in% not_listed_player, "player_name"] <- "Outlier" 
    pred_subset[pred_subset$player_id %in% not_listed_player, "player_id"] <- 400000 
  }
  
  not_listed_opponent <- which(!(pred_subset$opponent_id %in% player_info_4_years$id))
  not_listed_opponent <- pred_subset$opponent_id[not_listed_opponent]
  
  if(length(not_listed_opponent) > 0){
    pred_subset[pred_subset$opponent_id %in% not_listed_opponent, "opponent_name"] <- "Outlier"
    pred_subset[pred_subset$opponent_id %in% not_listed_opponent, "opponent_id"] <- 400000 
  }
  
  pred_subset$player_id <- factor(pred_subset$player_id, 
                                  levels = player_info_4_years$id, 
                                  labels = player_info_4_years$name)
  pred_subset$player_id <- relevel(pred_subset$player_id, "Outlier")
  pred_subset$opponent_id <- factor(pred_subset$opponent_id, 
                                    levels = player_info_4_years$id, 
                                    labels = player_info_4_years$name)
  pred_subset$opponent_id <-  relevel(pred_subset$opponent_id, "Outlier")
  
  ## Designmatrix aufstellen
  X_1_pred <- model.matrix(~ -1 + player_id, data = pred_subset)
  X_2_pred <- model.matrix(~ -1 + opponent_id, data = pred_subset) 
  X_pred <- cbind(pred_subset$home_tourney, X_1_pred-X_2_pred)
  colnames(X_pred)[1] <- "home"
  
  index <- which(!(colnames(X_pred) %in%  X_4_years_colnames))
  if(length(index) > 0){
    X_pred <- X_pred[,-index]
  }
  
  # X_pred <- as.matrix(X_pred)
  X_pred <- as.data.frame(X_pred)
  
  ######
  
  ## weights bestimmen
  w_level_4_years <- sapply(temp_subset_4_years$tourney_level, level_weight)
  
  w_time_4_years_1_halfperiod <- time_weight(d = temp_subset_4_years$date_difference, halfperiod = 365)
  w_time_4_years_2_halfperiod <- time_weight(d = temp_subset_4_years$date_difference, halfperiod = 365 * 2)
  w_time_4_years_3_halfperiod <- time_weight(d = temp_subset_4_years$date_difference, halfperiod = 365 * 3)
  
  w_2 <- w_level_4_years * w_time_4_years_1_halfperiod
  w_3 <- w_level_4_years * w_time_4_years_2_halfperiod
  w_4 <- w_level_4_years * w_time_4_years_3_halfperiod
  
  # data, pred_subset nicht entfernen!
  rm(na_surface, temp_id_4_years, df_player, df_opponent, all_matches, player_stats,
     X_1, X_2, index, outlier, temp_subset_4_years, w_level_4_years, 
     w_time_4_years_1_halfperiod, w_time_4_years_2_halfperiod, w_time_4_years_3_halfperiod,
     X_1_pred, X_2_pred, not_listed_opponent, not_listed_player, 
     X_4_years_colnames, player_info_4_years)
  
  gc()
  
  ##### 
  
  ## Modelle mit 4 Jahren Trainingsdaten
  
  model1 <- glm(y ~ -1 + ., data = X, family = binomial)
  pred_outcome1 <- predict(model1, newdata = X_pred, type = 'response')
  rm(model1)
  
  gc()
  
  model2 <- glm(y ~ -1 + ., data = X, family = binomial, weights = w_2)
  pred_outcome2 <- predict(model2, newdata = X_pred, type = 'response')
  rm(model2, w_2)
  
  gc()
  
  model3 <- glm(y ~ -1 + ., data = X, family = binomial, weights = w_3)
  pred_outcome3 <- predict(model3, newdata = X_pred, type = 'response')
  rm(model3, w_3)
  
  gc()
  
  model4 <- model4 <- glm(y ~ -1 + ., data = X, family = binomial, weights = w_4)
  pred_outcome4 <- predict(model4, newdata = X_pred, type = 'response')
  rm(model4, w_4, X, y, X_pred)
  
  gc()
  
  
  
  #####  models with 6 years training data #######################################
  
  ## alle Turniere 6 Jahre vor dem zu schaetzenden Turnier auswaehlen
  temp_id_6_years <- data[(data$tourney_date < temp_date &
                            data$tourney_date > temp_date - 365*6), 1] 
  temp_subset_6_years <- data[data$tourney_id %in% temp_id_6_years, ]
  na_surface <- is.na(temp_subset_6_years$surface)
  temp_subset_6_years <- temp_subset_6_years[!na_surface,]
  
  
  ## Outlier bestimmen
  
  # alle Spiele aus Perspektive von Spieler 1
  df_player <- temp_subset_6_years[, c("player_name", "win")]
  names(df_player) <- c("name", "win")
  
  # alle Spiele aus der Perspektive von Spieler 2 (win = inverted)
  df_opponent <- temp_subset_6_years[, c("opponent_name", "win")]
  df_opponent$win <- 1 - df_opponent$win
  names(df_opponent) <- c("name", "win")
  
  # Merge beide Datensaetze
  all_matches <- rbind(df_player, df_opponent)
  
  # fuer jeden Spieler ueberpruefen, ob er gewinnt UND verliert 
  player_stats <- aggregate(win ~ name, data = all_matches, 
                            FUN = function(x) length(unique(x)))
  
  # Outlier haben nur gewonnen ODER nur verloren
  outlier <- player_stats$name[player_stats$win == 1]
  
  
  ## Outlier im Datensatz ueberschreiben mit einer gemeinsamen ID
  temp_subset_6_years[temp_subset_6_years$player_name %in% outlier, "player_id"] <- 400000 
  temp_subset_6_years[temp_subset_6_years$player_name %in% outlier, "player_name"] <- "Outlier" 
  
  temp_subset_6_years[temp_subset_6_years$opponent_name %in% outlier, "opponent_id"] <- 400000
  temp_subset_6_years[temp_subset_6_years$opponent_name %in% outlier, "opponent_name"] <- "Outlier"
  
  
  ## alle einzigartigen Spieler  
  player_info_6_years <- data.frame(id = temp_subset_6_years$player_id, 
                                    name = temp_subset_6_years$player_name)
  player_info_6_years <- rbind(player_info_6_years, data.frame(id = temp_subset_6_years$opponent_id, 
                                                               name = temp_subset_6_years$opponent_name))
  player_info_6_years <- distinct(player_info_6_years)
  
  
  ## Designmatrix vorbereiten 
  temp_subset_6_years$player_id <- factor(temp_subset_6_years$player_id, 
                                          levels = player_info_6_years$id, 
                                          labels = player_info_6_years$name)
  temp_subset_6_years$player_id <- relevel(temp_subset_6_years$player_id, "Outlier")
  temp_subset_6_years$opponent_id <- factor(temp_subset_6_years$opponent_id, 
                                            levels = player_info_6_years$id, 
                                            labels = player_info_6_years$name)
  temp_subset_6_years$opponent_id <-  relevel(temp_subset_6_years$opponent_id, "Outlier")
  
  # Spiele entfernen, in denen Outlier gegen outlier spielt
  index <- which(temp_subset_6_years$player_id == "Outlier" & temp_subset_6_years$opponent_id == "Outlier")
  if(length(index) > 0){
    temp_subset_6_years <- temp_subset_6_years[-index,]
  }
  
  
  ## Designmatrix aufstellen
  X_1 <- model.matrix(~ -1 + player_id, data = temp_subset_6_years)
  X_2 <- model.matrix(~ -1 + opponent_id, data = temp_subset_6_years) 
  X <- cbind(temp_subset_6_years$home_tourney, X_1-X_2)
  colnames(X)[1] <- "home"
  
  y <- as.numeric(temp_subset_6_years$win) # numeric fuer glmnet
  
  # Outlier SPalten entfernen
  X <- X[,-2]
  
  # in matrix umwandeln fuer glmnet
  # X <- as.matrix(X)
  
  X_6_years_colnames <- colnames(X)
  
  X <- as.data.frame(X)
  
  ######
  
  ## Vorbereitung von Schaetzungen mit 6 Jahren Trainingsdaten
  
  pred_subset <- data[data$tourney_date == all_tourney_dates[j],]
  
  # true outcome abspeichern
  true_outcome <- as.numeric(pred_subset$win)
  
  # Spieler ohne Parameterschaetzung werden mit Outlier Parameter geschaetzt
  not_listed_player <- which(!(pred_subset$player_id %in% player_info_6_years$id))
  not_listed_player <- pred_subset$player_id[not_listed_player]
  
  if(length(not_listed_player) > 0){
    pred_subset[pred_subset$player_id %in% not_listed_player, "player_name"] <- "Outlier" 
    pred_subset[pred_subset$player_id %in% not_listed_player, "player_id"] <- 400000 
  }
  
  not_listed_opponent <- which(!(pred_subset$opponent_id %in% player_info_6_years$id))
  not_listed_opponent <- pred_subset$opponent_id[not_listed_opponent]
  
  if(length(not_listed_opponent) > 0){
    pred_subset[pred_subset$opponent_id %in% not_listed_opponent, "opponent_name"] <- "Outlier"
    pred_subset[pred_subset$opponent_id %in% not_listed_opponent, "opponent_id"] <- 400000 
  }
  
  pred_subset$player_id <- factor(pred_subset$player_id, 
                                  levels = player_info_6_years$id, 
                                  labels = player_info_6_years$name)
  pred_subset$player_id <- relevel(pred_subset$player_id, "Outlier")
  pred_subset$opponent_id <- factor(pred_subset$opponent_id, 
                                    levels = player_info_6_years$id, 
                                    labels = player_info_6_years$name)
  pred_subset$opponent_id <-  relevel(pred_subset$opponent_id, "Outlier")
  
  ## Designmatrix aufstellen
  X_1_pred <- model.matrix(~ -1 + player_id, data = pred_subset)
  X_2_pred <- model.matrix(~ -1 + opponent_id, data = pred_subset) 
  X_pred <- cbind(pred_subset$home_tourney, X_1_pred-X_2_pred)
  colnames(X_pred)[1] <- "home"
  
  index <- which(!(colnames(X_pred) %in%  X_6_years_colnames))
  if(length(index) > 0){
    X_pred <- X_pred[,-index]
  }

  # X_pred <- as.matrix(X_pred)
  
  X_pred <- as.data.frame(X_pred)
  
  ######
  
  ## weights bestimmen
  w_level_6_years <- sapply(temp_subset_6_years$tourney_level, level_weight)
  
  w_time_6_years_1_halfperiod <- time_weight(d = temp_subset_6_years$date_difference, halfperiod = 365)
  w_time_6_years_3_halfperiod <- time_weight(d = temp_subset_6_years$date_difference, halfperiod = 365 * 3)
  w_time_6_years_5_halfperiod <- time_weight(d = temp_subset_6_years$date_difference, halfperiod = 365 * 5)
  
  w_5 <- w_level_6_years * w_time_6_years_1_halfperiod
  w_6 <- w_level_6_years * w_time_6_years_3_halfperiod
  w_7 <- w_level_6_years * w_time_6_years_5_halfperiod
  
  # data, pred_subset nicht entfernen!
  rm(na_surface, temp_id_6_years, df_player, df_opponent, all_matches, player_stats,
     X_1, X_2, index, outlier, temp_subset_6_years, w_level_6_years, 
     w_time_6_years_1_halfperiod, w_time_6_years_3_halfperiod, w_time_6_years_5_halfperiod,
     X_1_pred, X_2_pred, not_listed_opponent, not_listed_player, 
     X_6_years_colnames, player_info_6_years)
  
  gc()
  
  #####
  
  ## Modelle fuer 6 Jahre Trainingsdaten
  
  model5 <- glm(y ~ -1 + ., data = X, family = binomial, weights = w_5)
  pred_outcome5 <- predict(model5, newdata = X_pred, type = 'response')
  rm(w_5, model5)
  
  gc()
  
  model6 <- glm(y ~ -1 + ., data = X, family = binomial, weights = w_6)
  pred_outcome6 <- predict(model6, newdata = X_pred, type = 'response')
  rm(w_6, model6)
  
  gc()
  
  model7 <- glm(y ~ -1 + ., data = X, family = binomial, weights = w_7)
  pred_outcome7 <- predict(model7, newdata = X_pred, type = 'response')
  rm(w_7, model7, X, y)
  
  gc()
  
  ################################################################################
  
  prediction <- data.frame(true_outcome = true_outcome,
                           pred_outcome1 = pred_outcome1,
                           pred_outcome2 = pred_outcome2,
                           pred_outcome3 = pred_outcome3,
                           pred_outcome4 = pred_outcome4,
                           pred_outcome5 = pred_outcome5,
                           pred_outcome6 = pred_outcome6,
                           pred_outcome7 = pred_outcome7)
  
  save(prediction, file = paste0("original_model_prediction_", j, ".RData")) #id hier hinzufuegen
  
  gc()
}
  
  
  
  
  
  
  
  
  ##############################################################################


  
  
