## New Workspace
rm(list = ls())
cat("\014")

## Libraries
library(dplyr)
library(glmnet)
library(car)
library(MASS)

mercedes.benz <- file.path("C:","Users","NeerajSubhedar","Google Drive","Kaggle","Mercedes Benz")

## Loading data
merc.train <- read.csv(paste0(mercedes.benz,"/train.csv"))
merc.test <- read.csv(paste0(mercedes.benz,"/test.csv"))

## Merging files
merc.test$y <- 0
merc.train$type <- "train"
merc.test$type <- "test"
merc.merge <- rbind(merc.train,merc.test)

## Write merge file
#write.csv(x = merc.merge,file = paste0(mercedes.benz,"/merged_file.csv"))

## Column types
target <- "y"
file.type <- "type"
ID <- "ID"
merc.is.factor <- colnames(merc.merge[,sapply(merc.merge,is.factor)])
merc.is.int <- colnames(merc.merge[,sapply(merc.merge,is.integer)])
merc.other.type <- colnames(
  merc.merge[!(colnames(merc.merge) %in% c(merc.is.factor,merc.is.int))])

## Feature Engineering
merc.merge[,(colnames(merc.merge) %in% merc.is.factor)] <- 
  is.numeric(merc.merge[,(colnames(merc.merge) %in% merc.is.factor)])

## Creating datasets
traindata <- merc.merge[merc.merge$type == "train",
                        !(colnames(merc.merge) %in% c(file.type,ID))]
testdata <- merc.merge[merc.merge$type == "test",
                       !(colnames(merc.merge) %in% c(file.type,ID,target))]

## Base Model
glm.model <- glm(formula = traindata$y~.,family = "gaussian",data = traindata)
opar <- par()
par(mfrow=c(2,2))
plot(glm.model)

## R-Square
merc.mean <- 100.6693
RSS <- sum((glm.model$fitted.values-merc.mean)^2)
TSS <- sum((glm.model$y-merc.mean)^2)
r.square <- 1 - (RSS/TSS)
r.square # 0.4109443

## Predictions
glm.model.predict <- predict(object = glm.model,newdata = testdata)

## Read sample file
sample.file <- read.csv(paste0(mercedes.benz,"/submissions/sample_submission.csv"))

## Submission # rsquare = 0.47333
#submission1 <- cbind.data.frame(ID = merc.merge$ID[merc.merge$type == "test"],y = glm.model.predict)
submission1 <- cbind.data.frame(ID = sample.file$ID,y = glm.model.predict)
write.csv(x = submission1,file = paste0(mercedes.benz,"/submissions/submission1.csv"),row.names = F)

## With boxCox transformation
boxCox(glm.model)
traindata$y.ts <- 1/(traindata$y)
traindata$y.tss <- 1/(traindata$y^2)

## model after transformation 1
glm.model.ts <- glm(formula = traindata$y.ts~.,family = "gaussian",data = traindata[,!(colnames(traindata) %in% c("y","y.tss"))])
opar <- par()
par(mfrow=c(2,2))
plot(glm.model.ts)

## R-Square
merc.mean <- 100.6693
fit.val <- (glm.model.ts$fitted.values)^-1
val <- traindata$y.ts
RSS <- sum((fit.val-merc.mean)^2)
TSS <- sum((val-merc.mean)^2)
r.square.ts <- 1 - (RSS/TSS)
r.square.ts # 0.9907139

## Predictions
glm.model.ts.predict <- (predict(object = glm.model.ts,newdata = testdata))^-1

## Read sample file
sample.file <- read.csv(paste0(mercedes.benz,"/submissions/sample_submission.csv"))

## Submission # rsquare = 0.45739 # seems to be overfitting
submission2 <- cbind.data.frame(ID = sample.file$ID,y = glm.model.ts.predict)
write.csv(x = submission2,file = paste0(mercedes.benz,"/submissions/submission2.csv"),row.names = F)

## model after transformation 2
glm.model.tss <- glm(formula = traindata$y.tss~.,family = "gaussian",data = traindata[,!(colnames(traindata) %in% c("y","y.ts"))])
opar <- par()
par(mfrow=c(2,2))
plot(glm.model.tss)

## R-Square
merc.mean <- 100.6693
fit.val <- (glm.model.ts$fitted.values)^(-1/2)
val <- traindata$y.tss
RSS <- sum((fit.val-merc.mean)^2)
TSS <- sum((val-merc.mean)^2)
r.square.tss <- 1 - (RSS/TSS)
r.square.tss # 0.1886916

## Predictions
glm.model.tss.predict <- (predict(object = glm.model.tss,newdata = testdata))^(-1/2)

## Read sample file
sample.file <- read.csv(paste0(mercedes.benz,"/submissions/sample_submission.csv"))

## Submission # rsquare = 0.41544 # not sure of the difference
submission3 <- cbind.data.frame(ID = sample.file$ID,y = glm.model.tss.predict)
write.csv(x = submission3,file = paste0(mercedes.benz,"/submissions/submission3.csv"),row.names = F)