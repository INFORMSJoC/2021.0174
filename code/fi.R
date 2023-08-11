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
