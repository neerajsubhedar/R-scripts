## 5/14/2017
## implementing xgboost
## including hypertuning of the xgboost parameters

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
library(tidyr)
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
library(Matrix) # for xgboost
library(Ckmeans.1d.dp) # required to plot xgb importance plot
library(caret) # for hypertuning in xgboost trees

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

## 5/9/2017
## Imputation
missing.new <- Missing(macro.merge.srhm.new)

col.index.na.new <- as.integer(row.names(missing.new[missing.new$`# Missing Values`!=0,]))
col.name.na.new <- as.character(missing.new[missing.new$`# Missing Values`!=0, "Column Name"])

missing.means <- sapply(macro.merge.srhm.new[,(colnames(macro.merge.srhm.new) %in% col.name.na.new)], mean, na.rm = T)
for (i in c(1,3:length(missing.means))){
  macro.merge.srhm.new[is.na(macro.merge.srhm.new[,names(missing.means[i])]),names(missing.means[i])] <- missing.means[i]
}

## Dataset and feature engineering
new.data <- macro.merge.srhm.new[,-c(1,2)]
traindata <- new.data[new.data$type == "train",]
traindata <- traindata[,!(colnames(traindata) %in% c("type","product_type"))]
testdata <- new.data[new.data$type == "test",]
testdata <- testdata[,!(colnames(testdata) %in% c("type","product_type"))]

## Modeling xgboost Model 6 - with hyperparametric tuning
## create sparse matrix
traindata_sparse <- Matrix(data.matrix(traindata[,!(colnames(traindata) %in% "price_doc")]))
testdata_sparse <- Matrix(data.matrix(testdata[,!(colnames(testdata) %in% "price_doc")]))

traindata_xgbMatrix <- xgb.DMatrix(data = data.matrix(traindata[,!(colnames(traindata) %in% "price_doc")]),label = data.matrix(traindata$price_doc))

## creating grid of control parameters to perform grid search
xgb_grid1 <- expand.grid()

## packing the control parameters
xgb_trcontrol1 = trainControl()

## XGB new approach
glmnet_model1 <- train(x = data.matrix(traindata[,!(colnames(traindata) %in% "price_doc")]),
                    y = traindata$price_doc,
                    trControl = xgb_trcontrol1,
                    tuneGrid = xgb_grid1,metric = "rmse",
                    method = "glmnet"     # label for the model
                    )

# RMSLE for XGB5 data
target <- predict(xgb_model5,data.matrix(traindata[,!(colnames(traindata) %in% "price_doc")]))
# Validating the fit with rmsle
val <- 0
n <- length(traindata$price_doc)
for (i in 1:n){
  val[i] <- (log(target[i] + 1) - 
               log(traindata$price_doc[i] + 1))^2
}

rmsle.xgb.model5 <- (sum(val,na.rm = T)/n)^(1/2)
rmsle.xgb.model5 # 0.4259445

## Predictions with cross-validation
preds.xgb_model5 <- predict(xgb_model5,data.matrix(testdata[,!(colnames(testdata) %in% "price_doc")]))
sum(is.na(preds.xgb_model5))
sum(is.finite(preds.xgb_model5))
preds.xgb_model5[preds.xgb_model5<0] <- 0

## File - submission 18 # 0.34350
submission18 <- cbind.data.frame(id=test.srhm$id,price_doc = preds.xgb_model5)
write.csv(submission18,paste0(dir.kaggle.srhm,"/submissions/submission18.csv"),row.names = F)