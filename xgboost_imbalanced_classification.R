library(data.table)
library(h2o)
library(GGally)
library(caret)
library(ROSE)
library(DMwR)
library(dummies)
library(xgboost)


rm(list=ls())

ml_model <- function(x){
  
  #rm(list = setdiff(ls(), c('dtrain')))
  setDT(dtrain)
  
  dtrain[dtrain == "NULL"] <- NA
  dtrain <- dtrain[complete.cases(dtrain), ]
  
  dtrain <- dtrain[, unplanned_adm_flag := as.factor(unplanned_adm_flag)]
  
  
  
  
  
  cols.num <- c("period","patient_age", "total_services_count", "total_expense", "total_length_of_stay", "er_visits_count",
                "out_proc_count", "out_gen_count", "general_visit_count",
                "nurse_visit_count", "inpatient_visit_count", "snf_hospice_count", "inpatient_planned_count", "inpatient_unplanned_count",
                "trauma_center_count", "urgent_adm_count", "diag_score", "prelim_cond", "major_cond", "MDC1", "MDC12", "MDC19", "MDC2", "MDC22",
                "MDC23", "MDC25", "MDC34", "MDC35", "MDC37", "MDC38", "MDC4", "MDC48", "MDC5", "MDC55", "MDC6", "MDC66", "MDC8")
  dtrain <- as.data.frame(dtrain)
  dtrain[cols.num] <- sapply(dtrain[cols.num], as.numeric)
  sapply(dtrain, class)
  
  # One-hot-encoding features:
  ohe_feats = c("gender",
                "race",
                "medicare_status")
  dtrain <- dummy.data.frame(dtrain, names = ohe_feats, sep = "_")
  
  
  #rm(list = setdiff(ls(), c('dtrain')))
  names(dtrain)<-gsub("\\s","_",names(dtrain))
  
  
  table(dtrain$unplanned_adm_flag)
  # 97.6 := 0
  # 2.92 := 1
  #setDT(dtrain)
  #dtrain[, weight := ifelse(unplanned_adm_flag == 1, 1, 0.02991803)]
  
  
  nrows <- nrow(dtrain)
  index <- sample(1:nrows, 0.8 * nrows)  
  train <- dtrain[index,]                 
  test <<- dtrain[-index,]
  #dtrain <- as.data.frame(dtrain)
  #dtrain[41] <- dtrain[41] == "s"
  zero <- table(train$unplanned_adm_flag)[[1]]
  one <- table(train$unplanned_adm_flag)[[2]]
  total <- zero+one
  zero <- zero/total * 100 
  one <- one/total * 100 
  weight <- one/zero
  setDT(train)[, weight := ifelse(unplanned_adm_flag == 1, 1, weight)]
  train <- as.data.frame(train)
  #dtrain[52] <- dtrain[52] == "s"
  x <- ncol(train) - 2
  y <- ncol(train) - 1
  z <- ncol(train)
  label <- as.numeric(train[[y]]) - 1
  data <- as.matrix(train[1:x])

  #data <- as.matrix(train[x])
  testsize <- nrow(test)
  weight <- as.numeric(train[[z]]) * testsize / length(label)
  #weight <- as.numeric(dtrain[[32]]) * testsize / length(label)
  
  sumwpos <- sum(weight * (label == 1))
  sumwneg <- sum(weight * (label == 0))
  print(paste("weight statistics: wpos=", sumwpos, "wneg=", sumwneg, "ratio=", sumwneg / sumwpos))
  
  
  xgmat <- xgb.DMatrix(data, label = label, weight = weight)#, missing = -999.0)
  param <- list("objective" = "binary:hinge",
                "scale_pos_weight" = sumwneg / sumwpos,
                "bst:eta" = 0.1,
                "bst:max_depth" = 6,
                "eval_metric" = "auc",
                #"eval_metric" = "ams@0.15",
                "silent" = 1,
                "nthread" = 16)
  #param <- list("booster" = "gbtree",
  #             "objective" = "binary:logistic",
  #             "scale_pos_weight" = sumwneg / sumwpos,
  #            "bst:eta" = 1.0,
  #            "gamma" = 1.0,
  #           "min_child_weight" = 1,
  #           "bst:max_depth" = 3,
  #          "eval_metric" = "auc",
  #"eval_metric" = "ams@0.15",
  #          "silent" = 1,
  #         "nthread" = 16)
  watchlist <- list("train" = xgmat)
  nrounds = 120
  print ("loading data end, start to boost trees")
  bst = xgb.train(param, xgmat, nrounds, watchlist )
  
  
  
  
  # predict values in test set
  y_pred <- predict(bst, data.matrix(test[,1:x]))
  y_pred <<- as.data.frame(y_pred)
}


idx <- c(1,2,3,4,5,6,7)
client <- c(".csv","input.xlsx",".csv",".csv",
            ".csv",".csv", "all_six")
name <- c("ida", "ama", "cco", "spst", "nna", "ruth", "all_six")
flag<-c(0,0,1,1,1,1,2)
clients<-data.frame(idx,client,name,flag,stringsAsFactors=FALSE)
setDT(clients)

setwd("~/Unplanned_Admissions")


for (i in 1:nrow(clients)) {
  
  if(clients[, client][[i]] == "hida.csv" & clients[, flag][[i]] == 0){
    
    df1 <- read.csv(clients[, client][[i]], header = T, stringsAsFactors = F)
    setDT(df1)[, c("dob", "period_date", "ï..patient_id") := NULL]
    dtrain <- copy(df1)
    ml_model(dtrain)
    df1 <- confusionMatrix(as.factor(y_pred$y_pred), test$unplanned_adm_flag)
    assign(paste0("conf_matrix_", clients[, name][[i]]), df1, envir=.GlobalEnv)
    #rm(list = setdiff(ls(), c('clients', 'ml_model')))
    
  }
  
  else if(clients[, client][[i]] == "ht.xlsx" & clients[, flag][[i]] == 0){
    
    df1 <- readxl::read_excel(clients[, client][[i]])
    setDT(df1)[, c("period_date", "patient_id") := NULL]
    dtrain <- copy(df1)
    ml_model(dtrain)
    df1 <- confusionMatrix(as.factor(y_pred$y_pred), test$unplanned_adm_flag)
    assign(paste0("conf_matrix_", clients[, name][[i]]), df1, envir=.GlobalEnv)
  }
  
  else if(clients[, flag][[i]] == 1){
    
    df1 <- read.csv(clients[, client][[i]], header = T, stringsAsFactors = F)
    setDT(df1)[, c("dob", "period_date", "patient_id") := NULL]
    dtrain <- copy(df1)
    ml_model(dtrain)
    df1 <- confusionMatrix(as.factor(y_pred$y_pred), test$unplanned_adm_flag)
    assign(paste0("conf_matrix_", clients[, name][[i]]), df1, envir=.GlobalEnv)
  }
  
  else if(clients[, flag][[i]] == 2){
    
    df1 <- read.csv("hda.csv", header = T, stringsAsFactors = F)
    df2 <- readxl::read_excel("h.xlsx") # alma data prepared for model
    df3 <- read.csv("unplaal.csv", header = T, stringsAsFactors = F)
    df4 <- read.csv("unplina.csv", header = T, stringsAsFactors = F)
    df5 <- read.csv("unploast.csv", header = T, stringsAsFactors = F)
    df6 <- read.csv("unplanealth.csv", header = T, stringsAsFactors = F)
    
    setDT(df1)[, c("dob", "period_date", "ï..patient_id") := NULL]
    setDT(df2)[, c("period_date", "patient_id") := NULL]
    setDT(df3)[, c("dob", "period_date", "patient_id") := NULL]
    setDT(df4)[, c("dob", "period_date", "patient_id") := NULL]
    setDT(df5)[, c("dob", "period_date", "patient_id") := NULL]
    setDT(df6)[, c("dob", "period_date", "patient_id") := NULL]
    
    #df <- rbind(df1, df2, df3)
    dtrain <- rbind(df1, df2, df3, df4, df5, df6)
    
    ml_model(dtrain)
    df1 <- confusionMatrix(as.factor(y_pred$y_pred), test$unplanned_adm_flag)
    assign(paste0("conf_matrix_", clients[, name][[i]]), df1, envir=.GlobalEnv)
  }
  
}



par(mfrow = c(3, 3))

fourfoldplot(conf_matrix_netrina$table, margin = 1,
             main = paste0("Netrina(", round(conf_matrix_netrina$byClass[2] * 100, 2), "%)", sep = ""))

fourfoldplot(conf_matrix_rural_health$table, margin = 1,
             main = paste0("Rural Health(", round(conf_matrix_rural_health$byClass[2] * 100, 2), "%)", sep = ""))

fourfoldplot(conf_matrix_space_coast$table, margin = 1,
             main = paste0("Space Coast(", round(conf_matrix_space_coast$byClass[2] * 100, 2), "%)", sep = ""))

fourfoldplot(conf_matrix_central_flaco$table, margin = 1,
             main = paste0("Central Flaco(", round(conf_matrix_central_flaco$byClass[2] * 100, 2), "%)", sep = ""))

fourfoldplot(conf_matrix_all_six$table, margin = 1,
             main = paste0("All six clients(", round(conf_matrix_all_six$byClass[2] * 100, 2), "%)", sep = ""))

fourfoldplot(conf_matrix_alma$table, margin = 1,
             main = paste0("Alma(", round(conf_matrix_alma$byClass[2] * 100, 2), "%)", sep = ""))

fourfoldplot(conf_matrix_west_florida$table, margin = 1,
             main = paste0("West Florida(", round(conf_matrix_west_florida$byClass[2] * 100, 2), "%)", sep = ""))



conf_matrix_all_six <- data.frame(cbind(t(conf_matrix_all_six$overall),t(conf_matrix_all_six$byClass)))
setDT(conf_matrix_all_six)[, client_name := "all_six"]

conf_matrix_alma <- data.frame(cbind(t(conf_matrix_alma$overall),t(conf_matrix_alma$byClass)))
setDT(conf_matrix_alma)[, client_name := "alma"]

conf_matrix_central_flaco <- data.frame(cbind(t(conf_matrix_central_flaco$overall),t(conf_matrix_central_flaco$byClass)))
setDT(conf_matrix_central_flaco)[, client_name := "central_flaco"]

conf_matrix_netrina <- data.frame(cbind(t(conf_matrix_netrina$overall),t(conf_matrix_netrina$byClass)))
setDT(conf_matrix_netrina)[, client_name := "netrina"]

conf_matrix_rural_health <- data.frame(cbind(t(conf_matrix_rural_health$overall),t(conf_matrix_rural_health$byClass)))
setDT(conf_matrix_rural_health)[, client_name := "rural_health"]

conf_matrix_space_coast <- data.frame(cbind(t(conf_matrix_space_coast$overall),t(conf_matrix_space_coast$byClass)))
setDT(conf_matrix_space_coast)[, client_name := "space_coast"]

conf_matrix_west_florida <- data.frame(cbind(t(conf_matrix_west_florida$overall),t(conf_matrix_west_florida$byClass)))
setDT(conf_matrix_west_florida)[, client_name := "west_florida"]


df <- rbind(conf_matrix_all_six, conf_matrix_alma, conf_matrix_central_flaco, conf_matrix_netrina, conf_matrix_rural_health, conf_matrix_space_coast, conf_matrix_west_florida)
write.csv(df, "conf_matrix_unplanned_xgboost.csv", row.names = F)
