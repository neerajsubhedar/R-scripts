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

# Directory
dir.kaggle.srhm <- file.path("C:","Users","NeerajSubhedar","Google Drive","Kaggle","Sberbank Russian Housing Market")
train.srhm <- read.csv(paste0(dir.kaggle.srhm,"/files/train.csv"))
traindata <- train.srhm[,c(-1,-2)]

test.srhm <- read.csv(paste0(dir.kaggle.srhm,"/files/test.csv"))
testdata <- test.srhm[,c(-1,-2)]

# EDA
plot(density(traindata$price_doc))
plot(density(log(traindata$price_doc)))
summary(traindata$price_doc)
missing.traindata <-Missing(traindata)
boxplot(traindata$price_doc)
boxplot(traindata[-3528,"price_doc"])

# Subsetting and other feature operations
index.na <- as.integer(row.names(missing.traindata[missing.traindata$`# Missing Values`!=0,]))
traindata.without.NAs <- traindata[,-index.na]
testdata.without.NAs <- testdata[,-index.na]

###############
# Baseline
###############

price_dc.mean <- mean(traindata$price_doc)
val <- 0
n <- length(traindata$price_doc)
for (i in 1:n){
  val[i] <- (log(price_dc.mean + 1) - 
               log(traindata$price_doc[i] + 1))^2
}

rmsle.base <- (sum(val,na.rm = T)/n)^(1/2)
rmsle.base # 0.6278388

#############
# Base Model
#############

glm.gaussian.model1 <- glm(price_doc~.,family = gaussian,data = traindata.without.NAs)
summary(glm.gaussian.model1)
par(mfrow = c(2,2))
plot(glm.gaussian.model1) # shows huge deviation at 3528

# Validating the fit with rmsle
val <- 0
n <- length(traindata$price_doc)
for (i in 1:n){
  val[i] <- (log(glm.gaussian.model1$fitted.values[i] + 1) - 
               log(traindata$price_doc[i] + 1))^2
}

rmsle.glm.gaussian.model1 <- (sum(val,na.rm = T)/n)^(1/2)
rmsle.glm.gaussian.model1 # 0.5270581

# Predicting testdata values
preds.glm.gaussian.model1 <- predict(object = glm.gaussian.model1, newdata = testdata.without.NAs)

#######################
# BoxCox on base model
#######################
boxcox(object = glm.gaussian.model1)

glm.gaussian.model2 <- glm(I(log(price_doc))~.,family = gaussian,data = traindata.without.NAs)
summary(glm.gaussian.model2)
par(mfrow = c(2,2))
plot(glm.gaussian.model2) # shows huge deviation at 3528, better Q-Q plot

antilog.target <- exp(glm.gaussian.model2$fitted.values)

# Validating the fit with rmsle
val <- 0
n <- length(traindata$price_doc)
for (i in 1:n){
  val[i] <- (log(antilog.target[i] + 1) - 
               log(traindata$price_doc[i] + 1))^2
}

rmsle.glm.gaussian.model2 <- (sum(val,na.rm = T)/n)^(1/2)
rmsle.glm.gaussian.model2 # 0.5215858

# Predicting testdata values
preds.glm.gaussian.model2 <- predict(object = glm.gaussian.model2, newdata = testdata.without.NAs)

####################################
# random forest with transformation
####################################

rf.model1 <- randomForest(I(log(price_doc))~.,data = traindata.without.NAs) # fails
# Can not handle categorical predictors with more than 53 categories.

#########################################
# linear regression with transformation
#########################################

lm.model1 <- lm(formula = I(log(price_doc))~.,data = traindata.without.NAs)

summary(lm.model1)
par(mfrow = c(2,2))
plot(lm.model1) # shows huge deviation at 3528, better Q-Q plot

antilog.target <- exp(lm.model1$fitted.values)

# Validating the fit with rmsle
val <- 0
n <- length(traindata$price_doc)
for (i in 1:n){
  val[i] <- (log(antilog.target[i] + 1) - 
               log(traindata$price_doc[i] + 1))^2
}

rmsle.lm.model1 <- (sum(val,na.rm = T)/n)^(1/2)
rmsle.lm.model1 # 0.5215962

# Predicting testdata values
preds.lm.model1 <- exp(predict(object = lm.model1, newdata = testdata.without.NAs))

#######################
# Submission files
#######################
sum(is.na(preds.glm.gaussian.model2))
plot(density(preds.glm.gaussian.model2,na.rm = T))
preds.glm.gaussian.model2[is.na(preds.glm.gaussian.model2)] <- mean(preds.glm.gaussian.model2,na.rm = T)
submission.glm.gaussion.model2 <- cbind.data.frame(id = test.srhm$id,
                                                   price_doc = exp(preds.glm.gaussian.model2))
write.csv(x = submission.glm.gaussion.model2,paste0(dir.kaggle.srhm,"/submissions/submission2.csv"),row.names = FALSE)