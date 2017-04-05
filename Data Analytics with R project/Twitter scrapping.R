#install.packages(c("devtools", "rjson", "bit64", "httr"))

# clearing the workspace and console
cat("\014")
rm(list=ls())

# loading the libraries
library(rjson)
library(httr)
library(devtools)
#install_github("geoffjentry/twitteR")
library(twitteR)

oauth <- setup_twitter_oauth(consumer_key = "RTPZs421qBw2rnPtNGsaG6V7S",
                             consumer_secret = "kY4CgL7SdnYtkLqobAqMqDBNc7ASJ2Ks7rTLG4HhLbH7tUBfIv",
                             access_token = "75229041-KfJBkGaKgZCSLWSF1kuQbsAZHjBS4bGyntg7tTzbE",
                             access_secret = "M63vL0YGCQ8HGzYJEy5wccAtKq3aBOEloxl1hZXYXCSQj")

user1 <- getUser("neerajsub")
location <- closestTrendLocations(lat = 41, long = 72)
trends <- getTrends(location$woeid)

search1 <- searchTwitteR(searchString = "#Trump", n=100)
usertl <- userTimeline("AnkitRB",n = 50)
usertl <- NULL
searchTwitter("patriots", geocode="42.375,-71.1061111,100mi")
searchTwitter('patriots', geocode='42.375,-71.1061111,100mi',since = '2017-02-05',until = '2017-02-06')


############
# Functions
############

getType <- function(){
  one.two <- readline(prompt = "Enter, 1  - to search by string and location, 2 - to search by user: ")
  return(as.integer(one.two))
}

getString <- function(){
  search.string <- readline(prompt = "Enter string that you would like to search on Twitter: ")
  return(as.character(search.string))
}

getNumber <- function(){
  number.tweets <- readline(prompt = "Enter the number of tweets: ")
  return(as.integer(number.tweets))
}

searchThis <- function(search_string,geocode_string = "42.375,-71.1061111,1000mi",number.of.tweets = 100){
  searchTwitter(search_string, geocode=geocode_string,n = number.of.tweets)
}

getUser <- function(){
  search.user <- readline(prompt = "Enter the username to view their timeline: ")
  return(as.character(search.user))
}

userTL <- function(user.name,number.of.tweets = 100){
  userTimeline(user.name,n = number.of.tweets)
}

##################
# Executable Part
##################
run.the.code <- function(){
  choice <- getType()
  if (choice == 1) {
    find.on.twitter <- getString()
    num <- getNumber()
    out <- searchThis(search_string = find.on.twitter,
                         number.of.tweets = num)
  } else if (choice == 2) {
    user.tl <- getUser()
    num <- getNumber()
    out <- userTL(user.name = user.tl,number.of.tweets = num)
  } else {
    out <- "Enter a valid option."
  }
  return(out)
}

cat("\014")
return.object <- run.the.code()