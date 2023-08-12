library(gRain)
library(Hmisc)
library(bnlearn)
library(nnet)
library(magic)
library(msgl)
library("dplyr")

data <- read.csv("C:\\VA PTSD Prediction\\code\\data.csv", head = T)[,-1]
blacklist <- read.csv("C:\\VA PTSD Prediction\\code\\blacklist.csv", head = T)

for (i in 1:ncol(data)){
  data[,i] <- as.factor(data[,i])
}

set.seed(0)

# Split the data into train, validation, and test set.

n_test <- sample(nrow(data), 1000)
test <- data[n_test,]
train0 <- data[-n_test,]
n_valid <- sample(nrow(train0), 1000)
valid <- train0[n_valid,]
train <- train0[-n_valid,]

# BIC-based BN wth EM algorithm.

bn_bic <- structural.em(train, maximize = "hc", maximize.args = list(score = "bic", blacklist = blacklist), fit = "mle", fit.args = list(), impute = "bayes-lw")
fit_bic <- bn.fit(bn_bic, train)
fit_bic <- as.grain(fit_bic)

# Multiple imputation using BIC-based BN.

MaritalStatus_imp <- predict(fit_bic, response = "MaritalStatus", newdata = train, type = "distribution")
Ethnicity_imp <- predict(fit_bic, response = "Ethnicity", newdata = train, type = "distribution")
Race_imp <- predict(fit_bic, response = "Race", newdata = train, type = "distribution")
Religion_imp <- predict(fit_bic, response = "Religion", newdata = train, type = "distribution")

train_imp <- list()
D = 5
for (d in 1:D){
  set.seed(d)
  impute <- train
  
  MaritalStatus <- rMultinom(MaritalStatus_imp$pred$MaritalStatus,1)
  Ethnicity <- rMultinom(Ethnicity_imp$pred$Ethnicity,1)
  Race <- rMultinom(Race_imp$pred$Race,1)
  Religion <- rMultinom(Religion_imp$pred$Religion,1)
  
  impute$MaritalStatus[is.na(impute$MaritalStatus)] <- MaritalStatus[is.na(impute$MaritalStatus)]
  impute$Ethnicity[is.na(impute$Ethnicity)] <- Ethnicity[is.na(impute$Ethnicity)]
  impute$Race[is.na(impute$Race)] <- Race[is.na(impute$Race)]
  impute$Religion[is.na(impute$Religion)] <- Religion[is.na(impute$Religion)]
  
  train_imp[[d]] <- impute 
}
