# EE239AS Project 1
# Regression Analysis
# Yuang Chen, Jiaoyang li, Gena Xie, Qiyue Zhao

# Network backup Dataset
# Problem 1,2,3

# Load the Dataset
MyData <- read.csv("/Users/Yuang/Documents/UCLA/EE 239/Project1/network_backup_dataset.csv")

# Add libraires
library(MASS)
library(ISLR)
library(caret)
library(mlbench)
library(randomForest)
library(nnet)
library(Metrics)

# Change string to numbers in data
MyData$Day.of.Week <- as.factor(MyData$Day.of.Week)
levels(MyData$Day.of.Week) <- 1:length(levels(MyData$Day.of.Week))
MyData$Day.of.Week <- as.numeric(MyData$Day.of.Week)

MyData$Work.Flow.ID <- as.factor(MyData$Work.Flow.ID)
levels(MyData$Work.Flow.ID) <- 1:length(levels(MyData$Work.Flow.ID))
MyData$Work.Flow.ID <- as.numeric(MyData$Work.Flow.ID)

MyData$File.Name <- as.factor(MyData$File.Name)
levels(MyData$File.Name) <- 1:length(levels(MyData$File.Name))
MyData$File.Name <- as.numeric(MyData$File.Name)

# Plot the actual copy size of file
# workflow 0 file 0
A <- MyData$Work.Flow.ID=='1'
wf0 <- MyData[A,]
B <- wf0[,5]=='1'
wf0f0 <- wf0[B,]
x=c(1:120)
plot(x,wf0f0[1:120,6],"n",main="work flow 0 file 0 actual copy sizes",xlab="hour index",ylab="backup size in GB")
lines(x,wf0f0[1:120,6],"l")

# workflow 1 file 2
A <- MyData$Work.Flow.ID=='2'
wf1 <- MyData[A,]
B <- wf1[,5]=='3'
wf1f2 <- wf1[B,]
x=c(1:120)
plot(x,wf1f2[1:120,6],"n",main="work flow 1 file 2 actual copy sizes",xlab="hour index",ylab="backup size in GB")
lines(x,wf1f2[1:120,6],"l")

# workflow 2 file 9
A <- MyData$Work.Flow.ID=='3'
wf2 <- MyData[A,]
B <- wf2[,5]=='10'
wf2f9 <- wf2[B,]
x=c(1:120)
plot(x,wf2f9[1:120,6],"n",main="work flow 2 file 9 actual copy sizes",xlab="hour index",ylab="backup size in GB")
lines(x,wf2f9[1:120,6],"l")

# workflow 3 file 19
A <- MyData$Work.Flow.ID=='4'
wf3 <- MyData[A,]
B <- wf3[,5]=='15'
wf3f14 <- wf3[B,]
x=c(1:120)
plot(x,wf3f14[1:120,6],"n",main="work flow 3 file 14 actual copy sizes",xlab="hour index",ylab="backup size in GB")
lines(x,wf3f14[1:120,6],"l")

# workflow 4 file 24
A <- MyData$Work.Flow.ID=='5'
wf4 <- MyData[A,]
B <- wf4[,5]=='20'
wf4f19 <- wf4[B,]
x=c(1:120)
plot(x,wf4f19[1:120,6],"n",main="work flow 4 file 19 actual copy sizes",xlab="hour index",ylab="backup size in GB")
lines(x,wf4f19[1:120,6],"l")


MyData<-MyData[sample(nrow(MyData)),]
folds <- cut(seq(1,nrow(MyData)),breaks=10,labels=FALSE)
rms3 <- matrix(nrow=10,ncol=10)
for(i in 1:10){
  print(i)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- MyData[testIndexes, ]
  trainData <- MyData[-testIndexes, ]
  
  # Linear Regression Model
  fit <- lm(Size.of.Backup..GB. ~ 0+Week..+Day.of.Week+Backup.Start.Time...Hour.of.Day+Work.Flow.ID+File.Name+Backup.Time..hour.,data = trainData)
  prediction <- predict(fit,data.frame(testData))
  rmse <- rmse(testData$Size.of.Backup..GB., prediction)
  print(rmse)
  plot(testData$Size.of.Backup..GB.,prediction,main="fitted values versus actual values",xlab="actual values",ylab="fitted values")
  par(new = TRUE)
  x <- c(0,0.1,0.2,0.3,0.4)
  y <- x
  lines(x,y,col="red")
  res <- testData$Size.of.Backup..GB. - prediction
  plot(prediction,res,main="residuals versus fitted values",xlab="fitted values",ylab="residuals")
  par(new = TRUE)
  a <- c(-0.1,0,0.1,0.2,0.3)
  b <- c(0,0,0,0,0)
  lines(a,b,col="red")
  
  # Random Forest Model
  fit1 <- randomForest(Size.of.Backup..GB. ~ 0+Week..+Day.of.Week+Backup.Start.Time...Hour.of.Day+Work.Flow.ID+File.Name+Backup.Time..hour.,data = trainData,ntree=40)
  prediction1 <- predict(fit1,data.frame(testData))
  rms1 <- rmse(testData$Size.of.Backup..GB., prediction1)
  print(rms1)
  
  # Neural Network Model
  fit2 <- nnet(Size.of.Backup..GB. ~ Week..+Day.of.Week+Backup.Start.Time...Hour.of.Day+Work.Flow.ID+File.Name+Backup.Time..hour.,data = trainData,size=6,matix=1000,decay=5e-4)
  prediction2 <- predict(fit2,data.frame(testData))
  rms2 <- rmse(testData$Size.of.Backup..GB., prediction2)
  print(rms2)
  
  # Polynomial Function Model
  for(j in 1:10){
    fit3 <- lm(Size.of.Backup..GB. ~ poly(Week..+Day.of.Week+Backup.Start.Time...Hour.of.Day+Work.Flow.ID+File.Name+Backup.Time..hour.,j,raw=TRUE),data = trainData)
    prediction3 <- predict(fit3,data.frame(testData))
    rms3[i,j] <- rmse(testData$Size.of.Backup..GB., prediction3)
  }
}
print(rms3)
rms_avg <- colMeans(rms3)
print(rms_avg)
plot(1:10,rms_avg,main="average rmse by 10 fold versus degree",xlab="polynomial degree",ylab="average rmse")
lines(1:10,rms_avg)
 
trainData_fix <- MyData[1:round(nrow(MyData)*0.9),]
testData_fix <- MyData[16730:18588,]
rms_fix <- matrix(nrow=1, ncol=10)
for(k in 1:10){
  fit_fix <- lm(Size.of.Backup..GB. ~ poly(Week..+Day.of.Week+Backup.Start.Time...Hour.of.Day+Work.Flow.ID+File.Name+Backup.Time..hour.,k,raw=TRUE),data = trainData_fix)
  prediction_fix <- predict(fit_fix,data.frame(testData_fix))
  rms_fix[1,k] <- rmse(testData_fix$Size.of.Backup..GB., prediction_fix)
}
print(rms_fix)
plot(1:10,rms_fix,main="average rmse by fixed data versus degree",xlab="polynomial degree",ylab="average rmse")
lines(1:10,rms_fix)

# Workflow 0
print('workflow0')
wf0<-wf0[sample(nrow(wf0)),]
folds <- cut(seq(1,nrow(wf0)),breaks=10,labels=FALSE)
for(i in 1:10){
  print(i)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- wf0[testIndexes, ]
  trainData <- wf0[-testIndexes, ]
  # Linear Regression Model
  fit_wf0 <- lm(Size.of.Backup..GB. ~ 0+Week..+Day.of.Week+Backup.Start.Time...Hour.of.Day+Work.Flow.ID+File.Name+Backup.Time..hour.,data = wf0)
  prediction_wf0 <- predict(fit_wf0,data.frame(testData))
  rmse_wf0 <- rmse(testData$Size.of.Backup..GB., prediction_wf0)
  print(rmse_wf0)
}

# Workflow 1
print('workflow1')
wf1<-wf1[sample(nrow(wf1)),]
folds <- cut(seq(1,nrow(wf1)),breaks=10,labels=FALSE)
for(i in 1:10){
  print(i)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- wf1[testIndexes, ]
  trainData <- wf1[-testIndexes, ]
  # Linear Regression Model
  fit_wf1 <- lm(Size.of.Backup..GB. ~ 0+Week..+Day.of.Week+Backup.Start.Time...Hour.of.Day+Work.Flow.ID+File.Name+Backup.Time..hour.,data = wf1)
  prediction_wf1 <- predict(fit_wf1,data.frame(testData))
  rmse_wf1 <- rmse(testData$Size.of.Backup..GB., prediction_wf1)
  print(rmse_wf1)
}

# Workflow 2
print("workflow2")
wf2<-wf2[sample(nrow(wf2)),]
folds <- cut(seq(1,nrow(wf2)),breaks=10,labels=FALSE)
for(i in 1:10){
  print(i)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- wf2[testIndexes, ]
  trainData <- wf2[-testIndexes, ]
  # Linear Regression Model
  fit_wf2 <- lm(Size.of.Backup..GB. ~ 0+Week..+Day.of.Week+Backup.Start.Time...Hour.of.Day+Work.Flow.ID+File.Name+Backup.Time..hour.,data = wf2)
  prediction_wf2 <- predict(fit_wf2,data.frame(testData))
  rmse_wf2 <- rmse(testData$Size.of.Backup..GB., prediction_wf2)
  print(rmse_wf2)
}

# Workflow 3
print('workflow3')
wf3<-wf3[sample(nrow(wf3)),]
folds <- cut(seq(1,nrow(wf3)),breaks=10,labels=FALSE)
for(i in 1:10){
  print(i)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- wf3[testIndexes, ]
  trainData <- wf3[-testIndexes, ]
  # Linear Regression Model
  fit_wf3 <- lm(Size.of.Backup..GB. ~ 0+Week..+Day.of.Week+Backup.Start.Time...Hour.of.Day+Work.Flow.ID+File.Name+Backup.Time..hour.,data = wf3)
  prediction_wf3 <- predict(fit_wf3,data.frame(testData))
  rmse_wf3 <- rmse(testData$Size.of.Backup..GB., prediction_wf3)
  print(rmse_wf3)
}

# Workflow 4
print('workflow4')
wf4<-wf4[sample(nrow(wf4)),]
folds <- cut(seq(1,nrow(wf4)),breaks=10,labels=FALSE)
for(i in 1:10){
  print(i)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- wf4[testIndexes, ]
  trainData <- wf4[-testIndexes, ]
  # Linear Regression Model
  fit_wf4 <- lm(Size.of.Backup..GB. ~ 0+Week..+Day.of.Week+Backup.Start.Time...Hour.of.Day+Work.Flow.ID+File.Name+Backup.Time..hour.,data = wf4)
  prediction_wf4 <- predict(fit_wf4,data.frame(testData))
  rmse_wf4 <- rmse(testData$Size.of.Backup..GB., prediction_wf4)
  print(rmse_wf4)
}