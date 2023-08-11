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

# Ordering-based search based on a given lambda value (here lambda = 0.01 as an example), initialize with BIC-based BN order, ordering search space is restricted within "CombatFlag, ServiceConnectedFlag,
#    MaritalStatus, SWAsiaConditionsFlag, Religion, MilitarySexualTraumaFlag, AgentOrangeFlag, and IonizingRadiationFlag".

l = 0.001
order_init <- c("Age_group", "Gender", "Ethnicity", "Race", "CombatFlag", "ServiceConnectedFlag","MaritalStatus", "SWAsiaConditionsFlag", "Religion",
                "MilitarySexualTraumaFlag", "AgentOrangeFlag", "IonizingRadiationFlag", "TBI", "PTSD", "PC5")
obj <- rep(0, length(order_init))
model <- vector("list", length(order_init))

gl2 <- function(p, order){
  x <- model.matrix(~., train_imp[[1]][,order[1:p-1]])[,-1]
  y <- as.matrix(train_imp[[1]][,order[p]])
  for (i in 2:D){
    x <- adiag(x, model.matrix(~., train_imp[[i]][,order[1:p-1]]))
    y <- rbind(y, as.matrix(train_imp[[i]][,order[p]]))
  }
  
  group <- rep(1, length(levels(train_imp[[1]][,c(order[1])]))-1)
  for (i in 2:(p-1)){
    group <- c(group, rep(i, length(levels(train_imp[[1]][,c(order[i])]))-1))
  }
  grouping <- group
  for (j in 2:D){
    grouping <- c(grouping, p+j-2, group)
  }
  
  groupwt <- sqrt(length(levels(train_imp[[1]][,c(order[p])]))*D*(length(levels(train_imp[[1]][,c(order[1])]))-1))
  for (i in 2:(p-1)){
    groupwt <- c(groupwt, sqrt(length(levels(train_imp[[1]][,c(order[p])]))*D*(length(levels(train_imp[[1]][,c(order[i])]))-1)))
  }
  groupwt <- c(groupwt, rep(0, D-1))
  
  fit <- msgl::fit(x, y, alpha = 0, lambda = c(seq(l+0.02, l, by = -0.001)), grouping = grouping, groupWeights = groupwt) 
  return(list(fit, fit$objective[21]))
}


for (p in 5:15){
  r <- gl2(p, order_init)
  model[[p]] <- r[[1]]
  obj[p] <- r[[2]]
}

p <- 5
order <- order_init
while (p<12){
  order0 <- order
  order0[c(p, p+1)] <- order0[c(p+1, p)]
  w <- obj[p] + obj[p+1]
  r01 <- gl2(p, order0)
  r02 <- gl2(p+1, order0)
  w0 <- r01[[2]] + r02[[2]]
  if (w>w0){
    order <- order0
    model[[p]] <- r01[[1]]
    model[[p+1]] <- r02[[1]]
    obj[c(p, p+1)] <-c(r01[[2]], r02[[2]])
    p <- max(p-1, 5)
  } else{
    p <- p+1
  }
}

# Constructing the final GL2-BN model by creating the conditional probability table for each node.

cpt <- vector("list", length(order_init))
level <- vector("list", length(order_init))
names(level) <- order

for (p in 1:15){
  level[[p]] <- levels(pull(train_imp[[1]][order[p]]))
}

for (p in 1:4){
  cpt[[p]] <- parray(order[p], levels =level, values = prop.table(table(train[order[p]])) )
}

for (p in 5:15){
  feature <- apply(sapply(order, grepl, names(features(model[[p]])[[21]][2:(length(features(model[[p]])[[21]])/D)])),2,max)
  feature <- names(feature[feature==1])
  if (length(feature)==0){
    cpt[[p]] <- parray(order[p], levels =level, values = prop.table(table(train[order[p]])) )
  } else {
  cptDiag <- expand.grid(level[feature])
  dt <- as.matrix(model.matrix(~.,cptDiag))
  beta <- coef(model[[p]])[[21]][,1:(length(features(model[[p]])[[21]])/D)]%*%diag(c(1, rep(1/D, length(features(model[[p]])[[21]])/D-1)))
  for (i in 2:D){
    beta <- beta + coef(model[[p]])[[21]][,((length(features(model[[p]])[[21]])/D)*(i-1)+1):((length(features(model[[p]])[[21]])/D)*i)]/D
  }
  cpt[[p]] <- parray(c(order[p], feature), levels =level, values = t(t(exp(beta%*%t(dt)))/colSums(exp(beta%*%t(dt)))) )
  }
}

# Compile the conditional probability tables for the final model, and predict on the validation set.

compile_cpt <- compileCPT(cpt)
fit_gl2 <- grain(compile_cpt)

pred <- predict(fit_gl2, response = "PTSD", newdata = valid, type = "distribution")

# Feature importance analysis

var <- order[!order == "PTSD"]
dev <- matrix(0, length(var), 500)

for (n in 1:500){
  n_test <- sample(nrow(test),100,replace=F)
  sub_test <- subset(test[n_test, ], select = -c(PTSD))
  y <- ifelse(test[n_test, "PTSD"] == "Yes", 1, 0)
  pred <- predict(fit_gl2, response = "PTSD", newdata = sub_test, type = "distribution")$pred$PTSD[,2]
  rmse <- mean((y-pred)^2)
  for (p in 1:14){
    pred0 <- predict(fit_gl2, response = "PTSD", newdata = sub_test[,var[-p]], type = "distribution")$pred$PTSD[,2]
    rmse0 <- mean((y-pred0)^2)
    dev[p,n] <- rmse0 - rmse
  }
}

