#######################
## installing prophet
#######################

## Installing all the packages that have dependencies
install.packages(c("git2r","digest","inline","StanHeaders","devtools"))

## Dependencies for updated devtools in hadley/devtools github repo
library(git2r)
library(digest)

## Dependencies for prophet package
library(inline)
library(StanHeaders)
library(devtools)
require(devtools)
install_github("hadley/devtools")

## Installing prophet from facebookincubator/prophet repository
install_github("facebookincubator/prophet",ref = "master",subdir = "R")
library(prophet)
library(plyr)
library(dplyr)

#modify memory size
options(java.parameters = "-Xmx10g" )

citi.bike <-read.csv("C:/Users/NeerajSubhedar/Desktop/dc/citi_bike_subset_final.csv")
citi.bike$revenue <- ifelse((citi.bike$tripduration/60)<30,4,
                            4+((round_any(citi.bike$tripduration/60,15,f = ceiling)-30)*(4/15)))

library(lubridate)
citi.bike$tripendtime <- mdy_hms(citi.bike$stoptime)
citi.bike.model <- citi.bike[c(-1:-17)]
citi.bike.model <- citi.bike.model[c(2,1)]
names(citi.bike.model) <- c("ds","y")

citi.bike.prophet <- prophet(citi.bike.model)
class(citi.bike.prophet)

predict.citi.bike <- make_future_dataframe(citi.bike.prophet,periods = 7)
tail(predict.citi.bike)

forecast.citi.bike <- predict(citi.bike.prophet,predict.citi.bike)

plot(citi.bike.prophet,forecast.citi.bike)

prophet_plot_components(citi.bike.prophet,forecast.citi.bike)
