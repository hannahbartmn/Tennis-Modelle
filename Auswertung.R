library(rprojroot)

load("data.RData")

root <- find_root(is_git_root)

##### load the predictions #################################################
setwd(paste0(root, "/prediction_basis_glm"))
temp <- data.frame(matrix(NA, 1, 8))
colnames(temp) <- c("true_outcome", "pred_outcome1", "pred_outcome2", "pred_outcome3", 
                          "pred_outcome4", "pred_outcome5", "pred_outcome6", "pred_outcome7")

#for(i in 1:60){
for(i in 1:71){
  load(paste0("original_model_prediction_", i, ".RData"))
  temp <- rbind(temp, prediction)
}
prediction <- temp[-1,]
rm(temp, i)

#####

## tourney_level hinzufuegen 
#data <- data[data$tourney_level %in% c("O", "G"),]
#tourney_levels <- data[data$tourney_date >= as.Date("2011-01-01"), "tourney_level"]
tourney_levels <- data[data$tourney_date >= max(data$tourney_date) - 365, "tourney_level"]
prediction <- cbind(prediction, tourney_levels)
rm(tourney_levels)

##### calculate performance measures for all levels ############################

library(xtable)

nrow(prediction) #14266 #13289

pred_table <- matrix(NA, ncol = 7, nrow = 3)
pred_table <- data.frame(pred_table)
colnames(pred_table) <- c("model1", "model2", "model3" ,"model4" ,"model5" ,"model6" ,"model7")
rownames(pred_table) <- c("Class._Rate", "Likelihood", "Brier_Score")

n <- nrow(prediction)
true_outcome <- prediction$true_outcome

#classification rate
for(i in 1:7){
  pred_vec <- ifelse(prediction[, i+1] > 0.5, 1, 0)
  pred_table[1, i] <- mean(true_outcome == pred_vec)
}

#predictive bernoulli likelihood 
for(i in 1:7){
  pred_vec <- prediction[, i + 1]
  pred_table[2, i] <- mean(pred_vec^true_outcome * (1 - pred_vec)^(1 - true_outcome))
}

#Brier Score
for(i in 1:7){
  pred_vec <- prediction[, i + 1]
  pred_table[3, i] <- mean((pred_vec - true_outcome)^2)
}

#build latex table
xtable(t(pred_table), digits = 4)

################################################################################

apply(pred_table, 1, max)
apply(pred_table, 1, min)

#lvl <- levels[1]

################################################################################

prediction_GO <- prediction[prediction$tourney_levels %in% c("G", "O"),]

pred_table_GO <- matrix(NA, ncol = 7, nrow = 3)
pred_table_GO <- data.frame(pred_table_GO)
colnames(pred_table_GO) <- c("model1", "model2", "model3" ,"model4" ,"model5" ,"model6" ,"model7")
rownames(pred_table_GO) <- c("Class._Rate", "Likelihood", "Brier_Score")

n <- nrow(prediction_GO)
true_outcome <- prediction_GO$true_outcome

#classification rate
for(i in 1:7){
  pred_vec <- ifelse(prediction_GO[, i+1] > 0.5, 1, 0)
  pred_table_GO[1, i] <- mean(true_outcome == pred_vec)
}

#predictive bernoulli likelihood 
for(i in 1:7){
  pred_vec <- prediction_GO[, i + 1]
  pred_table_GO[2, i] <- mean(pred_vec^true_outcome * (1 - pred_vec)^(1 - true_outcome))
}

#Brier Score
for(i in 1:7){
  pred_vec <- prediction_GO[, i + 1]
  pred_table_GO[3, i] <- mean((pred_vec - true_outcome)^2)
}

#build latex table
xtable(t(pred_table_GO), digits = 4)

apply(pred_table_GO, 1, max)
apply(pred_table_GO, 1, min)


################################################################################

levels <- c("G", "O", "A", "C", "D", "F", "M")
# levels <- c("G", "O")

for(lvl in levels){
  
  pred_sub <- prediction[prediction$tourney_levels == lvl, ]
  n <- nrow(pred_sub)
  true_outcome <- pred_sub$true_outcome
  
  pred_table <- matrix(NA, ncol = 7, nrow = 3)
  pred_table <- data.frame(pred_table)
  colnames(pred_table) <- c("model1","model2","model3","model4","model5","model6","model7")
  rownames(pred_table) <- c("Class._Rate","Likelihood","Brier_Score")
  
  # Classification rate
  for(i in 1:7){
    pred_vec <- ifelse(pred_sub[, i+1] > 0.5, 1, 0)
    pred_table[1, i] <- mean(true_outcome == pred_vec)
  }
  
  # Predictive Bernoulli likelihood
  for(i in 1:7){
    pred_vec <- pred_sub[, i+1]
    pred_table[2, i] <- mean(pred_vec^true_outcome * (1 - pred_vec)^(1 - true_outcome))
  }
  
  # Brier score
  for(i in 1:7){
    pred_vec <- pred_sub[, i+1]
    pred_table[3, i] <- mean((pred_vec - true_outcome)^2)
  }
  
  tab <- xtable(
    t(pred_table),
    digits = 5,
    caption = paste0("Prediction performance for tourney level ", lvl, 
                     " (n = ", n, ").")
  )
  
  print(tab, include.rownames = TRUE)
}

# levels <- sort(unique(prediction$tourney_levels))
# 
# result_table <- data.frame()
# 
# for(lvl in levels){
#   
#   pred_sub <- prediction[prediction$tourney_levels == lvl, ]
#   n <- nrow(pred_sub)
#   true_outcome <- pred_sub$true_outcome
#   
#   for(i in 1:7){
#     
#     pred_prob <- pred_sub[, i+1]
#     pred_class <- ifelse(pred_prob > 0.5, 1, 0)
#     
#     class_rate <- mean(true_outcome == pred_class)
#     likelihood <- mean(pred_prob^true_outcome * (1 - pred_prob)^(1 - true_outcome))
#     brier <- mean((pred_prob - true_outcome)^2)
#     
#     result_table <- rbind(
#       result_table,
#       data.frame(
#         Level = lvl,
#         Model = paste0("model", i),
#         N = n,
#         Class_Rate = class_rate,
#         Likelihood = likelihood,
#         Brier_Score = brier
#       )
#     )
#   }
# }
# 
# tab <- xtable(
#   result_table,
#   digits = 4,
#   caption = "Prediction performance by tournament level"
# )
# 
# print(tab, include.rownames = FALSE)
# 

################################################################################

setwd(paste0(root, "/prediction_full_model_glmnet_lambda_sequence"))
library(abind)

##### load predictions #####

load("prediction_full_model_glmnet_lambda_sequence_1.RData")
temp <- prediction
for(i in 2:42){
  load(paste0("prediction_full_model_glmnet_lambda_sequence_", i, ".RData"))
  temp <- abind(temp, prediction, along = 1)
}
prediction <- temp
rm(temp, i)
dim(prediction)


##### compare predictive bernoulli likelihood for different lambdas #####

lambda_seq <- seq(1e-3, 1e-10, length.out = 150)

# look at model2
prediction2 <- data.frame(prediction[,,2])

# true_outcome
true_outcome <- data[data$tourney_date >= max(data$tourney_date) - 365, "win"]
true_outcome <- true_outcome[1:10859]

pred_likelihood <- apply(prediction2, 2, function(x) mean(x^true_outcome * (1 - x)^(1 - true_outcome)))
# Ordnung der lambda von gross zu klein

plot(lambda_seq, pred_likelihood)


