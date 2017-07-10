# EE239AS Project 1
# Regression Analysis
# Yuang Chen, Jiaoyang li, Gena Xie, Qiyue Zhao

# Boston Hosing Dataset
# Problem 4,5

MyData <- read.csv("/Users/Yuang/Documents/UCLA/EE 239/Project1/housing_data.csv")
colnames(MyData) <-
  c(
    "CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV"
  )

library(MASS)
library(ISLR)
library(Metrics)
library(lars)


MyData <- MyData[sample(nrow(MyData)),]
folds <- cut(seq(1,nrow(MyData)),breaks = 10,labels = FALSE)
rmse_poly <- matrix(nrow = 10,ncol = 10)

for (i in 1:10) {
  testIndexes <- which(folds == i,arr.ind = TRUE)
  testData <- MyData[testIndexes,]
  print(i)
  trainData <- MyData[-testIndexes,]
  x <- trainData
  fit <-
    lm(MEDV ~ 0 + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD +
         TAX + PTRATIO + B + LSTAT, data = trainData)
  prediction <- predict(fit,data.frame(testData))
  rmse <- rmse(testData$MEDV,prediction)
  print("rmse for linear regression is")
  print(rmse)
  x = -20:50
  y = x
  
  plot(testData$MEDV,prediction)
  par(new = TRUE)
  lines(x,y,col = "red")
  
  res = testData$MEDV - prediction
  
  x = -20:50
  y = 0 * x
  
  plot(prediction,res)
  par(new = TRUE)
  lines(x,y,col = "red")
  
  # Polynomial Function Model
  for (j in 1:10) {
    fit_poly <-
      lm(
        MEDV ~ poly(
          0 + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + B + LSTAT,j,raw =
            TRUE
        ), data = trainData
      )
    prediction_poly <- predict(fit_poly,data.frame(testData))
    rmse_poly[i,j] <- rmse(testData$MEDV, prediction_poly)
  }
  
  #ridge
  model.ridge <-
    lm.ridge(
      MEDV ~ 0 + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX +
        PTRATIO + B + LSTAT, data = trainData, lambda = seq(0.01,0.1,0.001)
    )
  lambda.ridge <- seq(0.01,0.1,0.001)[which.min(model.ridge$GCV)]
  print("best alpha")
  print(lambda.ridge)
  prediction = scale(testData[,1:13],center = F, scale = model.ridge$scales) %*% model.ridge$coef[,which.min(model.ridge$GCV)]
  rmseridge <- rmse(testData$MEDV,prediction)
  print("rmse for ridge regression is")
  print(rmseridge)

  #lasso
  m.lasso <- lars(as.matrix(trainData[,1:13]),trainData$MEDV)
  lambda = seq(0.01,0.1,0.001)[which.min(m.lasso$Cp)]
  print("best alpha")
  print(lambda)
  
  r <-
    cv.lars(as.matrix(trainData[,1:13]),trainData$MEDV,plot.it = FALSE)
  
  bestfraction <- r$index[which.min(r$cv)]
  
  # Observe coefficients
  coef.lasso <-
    predict(
      m.lasso,as.matrix(testData[,1:13]),s = bestfraction,type = "coefficient",mode =
        "fraction"
    )
  
  # Prediction
  prediction <-
    predict(
      m.lasso,as.matrix(testData[,1:13]),s = bestfraction,type = "fit",mode =
        "fraction"
    )$fit
  
  rmselasso <- rmse(testData$MEDV,prediction)
  print("rmse for lasso regression is")
  
  print(rmselasso)

  
}

rmse_avg <- colMeans(rmse_poly)
print(rmse_avg)
plot(1:10,rmse_avg)
lines(1:10,rmse_avg)
