## With macro file
## Imputing all
## merge all
## 5/1/2017

# Clear workspace and environment
cat("\014")
rm(list = ls())

#modify memory size
options(java.parameters = "-Xmx10g" )

# Functions
Missing <- function(input.data.frame){
  col.name <- colnames(input.data.frame)
  
  list.input.nas <- lapply(lapply(input.data.frame[col.name],is.na),sum)
  input.nas <- unlist(list.input.nas)
  
  num.rows <- nrow(input.data.frame)
  percent.missing <- (input.nas/num.rows)*100
  
  list.unique.values <- lapply(lapply(input.data.frame[col.name],unique)
                               ,length)
  unique.values <- unlist(list.unique.values)
  
  results.data.frame <- cbind.data.frame(col.name,input.nas,
                                         percent.missing,unique.values)
  
  colnames(results.data.frame) <- c("Column Name","# Missing Values",
                                    "% Missing Values","# Unique Values")
  
  rownames(results.data.frame) <- seq(1,length(col.name))
  
  return(results.data.frame)
}

# Libraries
library(dplyr)
library(randomForest) # fails here
library(glmnet)
library(xgboost)
library(MASS) # for box cox and box tidwell
library(Metrics) # rmsle
library(e1071)
library(car) # boxtidwell
library(data.table) #merge

# Directory
dir.kaggle.srhm <- file.path("C:","Users","NeerajSubhedar","Google Drive","Kaggle","Sberbank Russian Housing Market")
train.srhm <- read.csv(paste0(dir.kaggle.srhm,"/files/train.csv"))
test.srhm <- read.csv(paste0(dir.kaggle.srhm,"/files/test.csv"))
## adding 0 to predicted price
test.srhm$price_doc <- 0
## subset type for each
train.srhm$type <- "train"
test.srhm$type <- "test"
## merging the data
merge.srhm <- rbind(train.srhm,test.srhm)

## importing and merging the macroeconomic features
macro.srhm <- read.csv(paste0(dir.kaggle.srhm,"/files/macro.csv"))
macro.merge.srhm <-merge(x = merge.srhm,y = macro.srhm,by = "timestamp",all.x = T)

## Getting variables with missing data
results.missing <- Missing(macro.merge.srhm)

## number of columns with missing values
#sum(results.missing$`# Missing Values`!=0)

## Column Index
col.index.na.all <- as.integer(row.names(results.missing[results.missing$`# Missing Values`!=0,]))
col.index.na.more.than.ten.percent <- as.integer(row.names(results.missing[(results.missing$`# Missing Values`/nrow(macro.merge.srhm))>0.1,]))
col.index.na.less.than.ten.percent <- col.index.na.all[!(col.index.na.all %in% col.index.na.more.than.ten.percent)]

col.name.na.all <- as.character(results.missing[results.missing$`# Missing Values`!=0,"Column Name"])
col.name.na.more.than.ten.percent <- as.character(results.missing[(results.missing$`# Missing Values`/nrow(macro.merge.srhm))>0.1,"Column Name"])
col.name.na.less.than.ten.percent <- col.name.na.all[!(col.name.na.all %in% col.name.na.more.than.ten.percent)]

## Variable Selection
# removing variables large amount of missing values
macro.merge.srhm.new <- macro.merge.srhm[,!(colnames(macro.merge.srhm) %in% col.name.na.more.than.ten.percent)]

## 4/30/2017
## Imputation
missing.new <- Missing(macro.merge.srhm.new)

col.index.na.new <- as.integer(row.names(missing.new[missing.new$`# Missing Values`!=0,]))
col.name.na.new <- as.character(missing.new[missing.new$`# Missing Values`!=0, "Column Name"])

missing.means <- sapply(macro.merge.srhm.new[,(colnames(macro.merge.srhm.new) %in% col.name.na.new)], mean, na.rm = T)

for (i in c(1,3:length(missing.means))){
  macro.merge.srhm.new[is.na(macro.merge.srhm.new[,names(missing.means[i])]),names(missing.means[i])] <- missing.means[i]
}

## Modeling GLM Model 4
new.data <- macro.merge.srhm.new[,-c(1,2)]
traindata <- new.data[new.data$type == "train",]
traindata <- traindata[,!(colnames(traindata) %in% c("type","product_type"))]
testdata <- new.data[new.data$type == "test",]
testdata <- testdata[,!(colnames(testdata) %in% c("type","product_type"))]

# glm gaussian family
glm.gaussian.model4 <- glm(formula = I(log2(1+price_doc))~.,family = gaussian,data = traindata)

summary(glm.gaussian.model4)
par(mfrow = c(2,2))
plot(glm.gaussian.model4)

## RMSLE
antilog.target <- (2^(glm.gaussian.model4$fitted.values))-1

# Validating the fit with rmsle
val <- 0
n <- length(traindata$price_doc)
for (i in 1:n){
  val[i] <- (log(antilog.target[i] + 1) - 
               log(traindata$price_doc[i] + 1))^2
}

rmsle.glm.gaussian.model4 <- (sum(val,na.rm = T)/n)^(1/2)
rmsle.glm.gaussian.model4 # 0.5127848
## RMSLE ends

## Prediction
## rebuilding the model after removing product_type column
preds.glm.gaussian.model4 <- predict(object = glm.gaussian.model4, newdata = testdata)
sum(is.na(preds.glm.gaussian.model4))

## File
submission4 <- cbind.data.frame(id=test.srhm$id,price_doc = ((2^(preds.glm.gaussian.model4))-1))
write.csv(submission4,paste0(dir.kaggle.srhm,"/submissions/submission4.csv"),row.names = F)

##########################################
# Trying normalization and denormalzation
# did not work
##########################################

##########################################
# Trying to normalize numeric predictors
# Negative values produced
##########################################

## Modeling GLM Model 5
new.data <- macro.merge.srhm.new[,-c(1,2)]
numerics <- sapply(new.data,is.numeric)
intergers <- sapply(new.data,is.integer)
new.data.normalized <- new.data
new.data.normalized[,numerics|intergers] <- sapply(new.data.normalized[,numerics|intergers], function(x) {(x-mean(x))/sd(x)})
traindata <- new.data.normalized[new.data.normalized$type == "train",]
traindata <- traindata[,!(colnames(traindata) %in% c("type","product_type"))]
testdata <- new.data.normalized[new.data.normalized$type == "test",]
testdata <- testdata[,!(colnames(testdata) %in% c("type","product_type"))]

# glm gaussian family
glm.gaussian.model5 <- glm(formula = I(log2(1+price_doc))~.,family = gaussian,data = traindata)

summary(glm.gaussian.model5)
par(mfrow = c(2,2))
plot(glm.gaussian.model5)

## RMSLE
antilog.target <- (2^(glm.gaussian.model5$fitted.values))-1

# Validating the fit with rmsle
val <- 0
n <- length(traindata$price_doc)
for (i in 1:n){
  val[i] <- (log(antilog.target[i] + 1) - 
               log(traindata$price_doc[i] + 1))^2
}

rmsle.glm.gaussian.model5 <- (sum(val,na.rm = T)/n)^(1/2)
rmsle.glm.gaussian.model5 # 0.5127848
## RMSLE ends

## Prediction
## rebuilding the model after removing product_type column
preds.glm.gaussian.model5 <- predict(object = glm.gaussian.model5, newdata = testdata)
sum(is.na(preds.glm.gaussian.model5))

## File
submission5 <- cbind.data.frame(id=test.srhm$id,price_doc = ((2^(preds.glm.gaussian.model5))-1))
write.csv(submission5,paste0(dir.kaggle.srhm,"/submissions/submission5.csv"),row.names = F)