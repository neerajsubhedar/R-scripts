## With macro file
## 4/29/2017

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

# Directory
dir.kaggle.srhm <- file.path("C:","Users","NeerajSubhedar","Google Drive","Kaggle","Sberbank Russian Housing Market")
train.srhm <- read.csv(paste0(dir.kaggle.srhm,"/files/train.csv"))
traindata <- train.srhm[,c(-1)]

test.srhm <- read.csv(paste0(dir.kaggle.srhm,"/files/test.csv"))
testdata <- test.srhm[,c(-1)]

macro.srhm <- read.csv(paste0(dir.kaggle.srhm,"/files/macro.csv"))

train.macro.srhm <- merge.data.frame(x = train.srhm, y = macro.srhm, 
                                     all.x = train.srhm$timestamp)
test.macro.srhm <- merge.data.frame(x = test.srhm, y = macro.srhm, 
                                    all.x = train.srhm$timestamp)

## EDA
missing.train.macro <- Missing(train.macro.srhm)
missing.test.macro <- Missing(test.macro.srhm)
#write.csv(missing.train.macro,paste0(dir.kaggle.srhm,"/files/eda/train_macro.csv"))
#write.csv(missing.test.macro,paste0(dir.kaggle.srhm,"/files/eda/test_macro.csv"))

## Variable selection
index.na.tenormore <- as.integer(row.names(missing.train.macro[missing.train.macro$`# Missing Values` >= nrow(train.macro.srhm)/10,]))
column.name.na.tenormore <- missing.train.macro[index.na.tenormore,"Column Name"]

index.na <- as.integer(row.names(missing.train.macro[missing.train.macro$`# Missing Values` != 0,]))
column.name.na <- missing.train.macro[index.na,"Column Name"]

## New test data
testdata.new <- test.macro.srhm[,-c(1,2,(!(colnames(test.macro.srhm) %in% column.name.na.tenormore)))]

## New dataset with columns having missing values less than 10% of row count
train.macro.low.missing <- train.macro.srhm[,-index.na.tenormore]
missing.low.missing <- Missing(train.macro.low.missing)

index.na.lm <- as.integer(row.names(missing.low.missing[missing.low.missing$`# Missing Values` != 0,]))
column.name.na.lm <- missing.low.missing[index.na,"Column Name"]

## Imputation
str(train.macro.low.missing[index.na.lm])

missing.means <- sapply(train.macro.low.missing[index.na.lm],mean,na.rm = T)

train.macro.low.missing[is.na(train.macro.low.missing[index.na.lm[1]]),names(missing.means[1])] <- missing.means[1]

for (i in 1:length(missing.means)){
  train.macro.low.missing[is.na(train.macro.low.missing[index.na.lm[i]]),
                   names(missing.means[i])] <- missing.means[i]
}

traindata.glm.model3 <- train.macro.low.missing[,-c(1,2)]

###################
#
###################
glm.gaussian.model3 <- glm(I(log(price_doc))~.,family = gaussian,data = traindata.glm.model3)
summary(glm.gaussian.model3)
par(mfrow = c(2,2))
plot(glm.gaussian.model3) # shows huge deviation at 3528

antilog.target <- exp(glm.gaussian.model3$fitted.values)

# Validating the fit with rmsle
val <- 0
n <- length(traindata.glm.model3$price_doc)
for (i in 1:n){
  val[i] <- (log(antilog.target[i] + 1) - 
               log(traindata.glm.model3$price_doc[i] + 1))^2
}

rmsle.glm.gaussian.model3 <- (sum(val,na.rm = T)/n)^(1/2)
rmsle.glm.gaussian.model3 # 0.5102906

# Predicting testdata values
preds.glm.gaussian.model3 <- predict(object = glm.gaussian.model3, 
                                     newdata = testdata.new,na.action = na.pass)
preds.glm.gaussian.model3 <- exp(preds.glm.gaussian.model3)

lowrmsle.file <- read.csv(paste0(dir.kaggle.srhm,"/high/sub.csv"))

## Comparison
refdata <- lowrmsle.file$price_doc
antilog.target <- preds.glm.gaussian.model3
val <- 0
n <- length(traindata.glm.model3$price_doc)
for (i in 1:n){
  val[i] <- (log(antilog.target[i] + 1) - 
               log(refdata[i] + 1))^2
}

rmsle.compare <- (sum(val,na.rm = T)/n)^(1/2)
rmsle.compare # 0.5102906

## File for model3
submission3 <- cbind.data.frame(test.macro.srhm$id,preds.glm.gaussian.model3)
