## With macro file
## Imputing all - change in missing value approach
## merge all
## 5/3/2017

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
library(MissMech) #for MCAR (Missing Completely at Random) # did not help
library(mice) # for missing value imputation

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

#macro.merge.srhm.new[,names(col.name.na.new)] <- mice(data = macro.merge.srhm.new[,names(col.name.na.new)],
#                                                      m = 5,seed = 500)

macro.merge.srhm.new[,col.index.na.new] <- mice(data = macro.merge.srhm.new[,col.index.na.new],
                                                m = 1,seed = 100)

## Modeling GLM Model 10
new.data <- macro.merge.srhm.new[,-c(1,2)]
traindata <- new.data[new.data$type == "train",]
traindata <- traindata[,!(colnames(traindata) %in% c("type","product_type"))]
testdata <- new.data[new.data$type == "test",]
testdata <- testdata[,!(colnames(testdata) %in% c("type","product_type"))]

## limited features
traindata.new <- traindata[,colnames(traindata) %in% c("full_sq","floor","sub_area","price_doc")]
testdata.new <- testdata[,colnames(testdata) %in% c("full_sq","floor","sub_area")]

## Model 10
glm.gaussian.model10 <- glm(formula = I(log(price_doc))~.,family = gaussian,data = traindata)

summary(glm.gaussian.model10)
par(mfrow = c(2,2))
plot(glm.gaussian.model10)

step.glm.model10 <- step(glm.gaussian.model10,direction = "forward")

## RMSLE
antilog.target <- exp(glm.gaussian.model10$fitted.values)

# Validating the fit with rmsle
val <- 0
n <- length(traindata$price_doc)
for (i in 1:n){
  val[i] <- (log(antilog.target[i] + 1) - 
               log(traindata$price_doc[i] + 1))^2
}

rmsle.glm.gaussian.model10 <- (sum(val,na.rm = T)/n)^(1/2)
rmsle.glm.gaussian.model10 #0.6433202
## RMSLE ends

## Prediction
## rebuilding the model after removing product_type column
preds.glm.gaussian.model10 <- predict(object = glm.gaussian.model10, newdata = testdata)
sum(is.na(preds.glm.gaussian.model10))

## File
submission10 <- cbind.data.frame(id=test.srhm$id,price_doc = exp(preds.glm.gaussian.model10))
write.csv(submission10,paste0(dir.kaggle.srhm,"/submissions/submission10.csv"),row.names = F)

## Model 11
glm.gaussian.model11 <- glm(formula = I(log(price_doc))~.*.,family = gaussian,data = traindata.new)

summary(glm.gaussian.model11)
par(mfrow = c(2,2))
plot(glm.gaussian.model11)

## RMSLE
antilog.target <- exp(glm.gaussian.model11$fitted.values)

# Validating the fit with rmsle
val <- 0
n <- length(traindata$price_doc)
for (i in 1:n){
  val[i] <- (log(antilog.target[i] + 1) - 
               log(traindata$price_doc[i] + 1))^2
}

rmsle.glm.gaussian.model11 <- (sum(val,na.rm = T)/n)^(1/2)
rmsle.glm.gaussian.model11 # 0.7039722
## RMSLE ends

## Prediction
## rebuilding the model after removing product_type column
preds.glm.gaussian.model11 <- predict(object = glm.gaussian.model11, newdata = testdata.new)
sum(is.na(preds.glm.gaussian.model11))

## File
submission11 <- cbind.data.frame(id=test.srhm$id,price_doc = exp(preds.glm.gaussian.model11))
write.csv(submission11,paste0(dir.kaggle.srhm,"/submissions/submission11.csv"),row.names = F)


#################
# Cooks distance
#################
glm.gaussian.model11 <- glm(formula = I(log(price_doc))~.*.,family = gaussian,data = traindata.new)
cooks.dist <- cooks.distance(glm.gaussian.model11)
cutoff <- 4/nrow(traindata.new)

wt <- ifelse(cooks.dist<cutoff,1,cutoff/cooks.dist)

## Model 12
glm.gaussian.model12 <- glm(formula = I(log(price_doc))~.*.,family = gaussian,data = traindata.new[as.integer(names(wt)),],weights = wt)

summary(glm.gaussian.model12)
par(mfrow = c(2,2))
plot(glm.gaussian.model12)

## RMSLE
antilog.target <- exp(glm.gaussian.model12$fitted.values)

# Validating the fit with rmsle
val <- 0
n <- length(traindata.new[as.integer(names(wt)),"price_doc"])
for (i in 1:n){
  val[i] <- (log(antilog.target[i] + 1) - 
               log(traindata$price_doc[i] + 1))^2
}

rmsle.glm.gaussian.model12 <- (sum(val,na.rm = T)/n)^(1/2)
rmsle.glm.gaussian.model12 # 0.8201017
## RMSLE ends

## Prediction
## rebuilding the model after removing product_type column
preds.glm.gaussian.model12 <- predict(object = glm.gaussian.model12, newdata = testdata.new)
sum(is.na(preds.glm.gaussian.model12))

## File
submission12 <- cbind.data.frame(id=test.srhm$id,price_doc = exp(preds.glm.gaussian.model12))
write.csv(submission12,paste0(dir.kaggle.srhm,"/submissions/submission12.csv"),row.names = F)