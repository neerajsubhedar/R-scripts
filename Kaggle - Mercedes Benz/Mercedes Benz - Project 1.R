## New Workspace
rm(list = ls())
cat("\014")

## Libraries
library(dplyr)
library(glmnet)

mercedes.benz <- file.path("C:","Users","NeerajSubhedar","Google Drive","Kaggle","Mercedes Benz")

## Loading data
merc.train <- read.csv(paste0(mercedes.benz,"/train.csv"))
merc.test <- read.csv(paste0(mercedes.benz,"/test.csv"))

## Merging files
merc.test$y <- 0
merc.train$type <- "train"
merc.test$type <- "test"
merc.merge <- rbind(merc.train,merc.test)

## Column types
target <- "y"
file.type <- "type"
ID <- "ID"
merc.is.factor <- colnames(merc.merge[,sapply(merc.merge,is.factor)])
merc.is.int <- colnames(merc.merge[,sapply(merc.merge,is.integer)])
merc.other.type <- colnames(
  merc.merge[!(colnames(merc.merge) %in% c(merc.is.factor,merc.is.int))])

## Creating datasets
traindata <- merc.merge[!(colnames(merc.merge) %in% c(file.type,ID))]
testdata <- merc.merge[!(colnames(merc.merge) %in% c(file.type,ID,target))]

## Base Model
glm.model <- glm(formula = traindata$y~.,family = "gaussian",data = traindata)
opar <- par()
par(mfrow=c(2,2))
plot(glm.model)

## Predictions
glm.model.predict <- predict(object = glm.model,newdata = testdata)
submission1 <- cbind.data.frame(ID = merc.merge$ID[merc.merge$type == "test"],y = glm.model.predict)
write.csv(x = submission1,file = paste0(mercedes.benz,"/submissions/submission1.csv"),row.names = F)