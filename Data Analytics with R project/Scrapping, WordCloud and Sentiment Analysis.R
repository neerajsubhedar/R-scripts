#############################################
# Initialize the project
# Installs packages and loads libraries
# Sets up the twitter authentication
# NOTE: initialize and install the libraries
#############################################

project.initialize <- function(){
  ## clear console
  cat("\014")
  ## clear global variables
  rm(list=ls())
  
  ## list of packages required
  list.of.packages <- c("git2r","digest","devtools",
                        "RCurl","RJSONIO","stringr","syuzhet","httr",
                        "rjson","tm","NLP","RCurl","wordcloud",
                        "tidytext","dplyr","zipcode","bit","wordcloud2","png")
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  ## for devtools
  library(git2r);library(digest)
  #require(devtools)
  #install_github("hadley/devtools")
  library(devtools)
  install_github("geoffjentry/twitteR")
  
  ## data manipultion
  library(dplyr);library(stringr)
  
  # loading the libraries
  ## Linked to importing tweets
  library(rjson);library(httr);library(twitteR);library(zipcode)
  
  ## Linked to generating a wordcloud
  library(tm);library(NLP);library(RCurl);library(RJSONIO)
  library(stringr);library(wordcloud);library(wordcloud2);library(png)
  
  ## Linked to sentiment analysis
  library(syuzhet)
  
  # Twitter authentication key
  oauth <- setup_twitter_oauth(consumer_key = "RTPZs421qBw2rnPtNGsaG6V7S",
                               consumer_secret = "kY4CgL7SdnYtkLqobAqMqDBNc7ASJ2Ks7rTLG4HhLbH7tUBfIv",
                               access_token = "75229041-KfJBkGaKgZCSLWSF1kuQbsAZHjBS4bGyntg7tTzbE",
                               access_secret = "M63vL0YGCQ8HGzYJEy5wccAtKq3aBOEloxl1hZXYXCSQj")
}

#############################
# Functions to import tweets
#############################

# Prompts the user to choose between importing tweets by user or by using  search string
getType <- function(){
  one.two <- readline(prompt = "Enter, 1  - to search by string and location, 2 - to search by user: ")
  return(as.integer(one.two))
}

# Prompts the user to enter a search string, limited to a single character input as of now
getString <- function(){
  search.string <- readline(prompt = "Enter string that you would like to search on Twitter: ")
  return(as.character(search.string))
}

# Prompts the user to enter the number of tweets they want to import
getNumber <- function(){
  number.tweets <- readline(prompt = "Enter the number of tweets: ")
  return(as.integer(number.tweets))
}

# Runs searchTwitter function based on the search string input provided by the user
# To-Be-Implemented: get gecode information from user
# Implementation completed

# Function to prompt the user to input the zip code
getZipCode <- function(){
  zip.code <- readline(prompt = "Enter zipcode (should be between 00210 and 99950): ")
  return(as.character(zip.code))
}

# Function to prompt the user to input radius in miles
getMiles <- function(){
  miles <- readline(prompt = "Enter the radius, in miles, to search for tweets: ")
  return(as.character(miles))
}

# Returns a vector containing latitude and longitude with zipcode and radius as inputs
getLatLong.zip <- function(enter.zipcode,radius.mi){
  data("zipcode")
  attach(zipcode)
  enter.zipcode <- as.character(enter.zipcode)
  radius.mi <- as.character(radius.mi)
  lat.long <- zipcode[zip == enter.zipcode,c("latitude","longitude")]
  lat.long.mi <- paste0(lat.long$latitude,",",lat.long$longitude,",",radius.mi,"mi")
  detach(zipcode)
  # the rm() does not work here as the scope here is limited to whithin this function
  # zipcode is now placed in global variables list
  # rm(list = c("zipcode"))
  return(lat.long.mi)
}

# To-Be-Implemented: handle error for mismatch in number of tweets
searchThis <- function(search_string,geocode_string = "42.375,-71.1061111,1000mi",number.of.tweets = 100){
  searchTwitter(search_string, geocode=geocode_string,n = number.of.tweets,lang = "en")
}

# Prompts the user to enter the username of the twitter handle
getUser <- function(){
  search.user <- readline(prompt = "Enter the username to view their timeline: ")
  return(as.character(search.user))
}

# Imports tweets from user's timeline
# To-Be-Implemented: handle error for mismatch in number of tweets
userTL <- function(user.name,number.of.tweets = 100){
  userTimeline(user.name,n = number.of.tweets)
}

# Cleans the tweets
cleanTweets <- function(object.with.tweets){
  # list to dataframe
  df.tweets <- twListToDF(object.with.tweets)
  
  # Removes RT
  df.tweets$text_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df.tweets$text)
  # Removes @<twitter handle>
  df.tweets$text_clean = gsub("@\\w+", "", df.tweets$text_clean)
  # Removes punctuations
  df.tweets$text_clean = gsub("[[:punct:]]", "", df.tweets$text_clean)
  # Removes numbers
  df.tweets$text_clean = gsub("[[:digit:]]", "", df.tweets$text_clean)
  # Removes html links
  df.tweets$text_clean = gsub("http\\w+", "", df.tweets$text_clean)
  # Removes unnecessary spaces
  df.tweets$text_clean = gsub("[ \t]{2,}", "", df.tweets$text_clean)
  df.tweets$text_clean = gsub("^\\s+|\\s+$", "", df.tweets$text_clean)
  # Fix for error related to formatting 'utf8towcs'"
  df.tweets$text_clean <- str_replace_all(df.tweets$text_clean,"[^[:graph:]]", " ")
  return(df.tweets)
}

#################################################################################
# Function to run functions associated with user input and importing the tweets
#################################################################################
# Executes the part of code that imports tweets
# Returns tweets as a list
run.the.code <- function(){
  # Gets users choice
  choice <- getType()
  
  # Selects whether user wants to search by string or by user based the choice 
  # Implemented
  if (choice == 1) {
    find.on.twitter <- getString()
    num <- getNumber()
    zip.code <- getZipCode()
    rm(list = c("zipcode"))
    rad.mi <- getMiles()
    geocode.string <- getLatLong.zip(enter.zipcode = zip.code,radius.mi = rad.mi)
    # implementation fails, reason to be determined
    out <- searchThis(search_string = find.on.twitter,
                      number.of.tweets = num,geocode_string = geocode.string)
    # falling back to original code
    # returns tweets as a list
    # out <- searchThis(search_string = find.on.twitter,
    #                  number.of.tweets = num)
  } else if (choice == 2) {
    user.tl <- getUser()
    num <- getNumber()
    # returns tweets as a list
    out <- userTL(user.name = user.tl,number.of.tweets = num)
  } else {
    out <- "Enter a valid option."
  }
  return(out)
}

##############################
# Generate wordcloud function
##############################

generateWordCloud.positive.tmStopWords <- function(object.with.tweets, minimum.frequency = 10){
  
  ## Cleans the tweets ans stores them as a dataframe
  df.tweets <- cleanTweets(object.with.tweets)
  
  # Creates a text corpus from the plain text document for every tweet
  text_corpus <- Corpus(VectorSource(df.tweets$text_clean))
  # Text_corpus is a collection of tweets where every tweet is a document
  
  # creating a Term Document Matrix 
  tdm <- TermDocumentMatrix(
    # the text corpus created from the text_clean object
    text_corpus,
    # defining the stopwords to be removed before creating a term document matrix
    control = list(
      removePunctuation = TRUE,
      stopwords("en"),
      removeNumbers = TRUE,
      tolower = TRUE)
  )
  
  # converting term document matrix to matrix
  m <- as.matrix(tdm)
  
  # get word counts in decreasing order
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  
  # create a data frame with words and their frequencies
  dm <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  nrc.lexicons <- get_nrc_sentiment(as.character(dm$word))
  
  #tweets.negative <- dm[nrc.lexicons$negative>0,]
  tweets.positive <- dm[nrc.lexicons$positive>0,]
  #tweets.neutral <- dm[nrc.lexicons$negative==0 & nrc.lexicons$positive==0,]
  
  fig.path.pos <- "C:/Users/NeerajSubhedar/Documents/GitHub/R-scripts/Data Analytics with R project/positive.png"
  #fig.path.neg <- "C:/Users/NeerajSubhedar/Documents/GitHub/R-scripts/Data Analytics with R project/negative.png"
  
  wordcloud2(data = tweets.positive, figPath = fig.path.pos,size = 1.5,gridSize = 3)
  #wordcloud2(data = tweets.negative, figPath = fig.path.neg,size = 1.5,gridSize = 3)
}

generateWordCloud.negative.tmStopWords <- function(object.with.tweets, minimum.frequency = 10){
  
  ## Cleans the tweets ans stores them as a dataframe
  df.tweets <- cleanTweets(object.with.tweets)
  
  # Creates a text corpus from the plain text document for every tweet
  text_corpus <- Corpus(VectorSource(df.tweets$text_clean))
  # Text_corpus is a collection of tweets where every tweet is a document
  
  # creating a Term Document Matrix 
  tdm <- TermDocumentMatrix(
    # the text corpus created from the text_clean object
    text_corpus,
    # defining the stopwords to be removed before creating a term document matrix
    control = list(
      removePunctuation = TRUE,
      stopwords("en"),
      removeNumbers = TRUE,
      tolower = TRUE)
  )
  
  # converting term document matrix to matrix
  m <- as.matrix(tdm)
  
  # get word counts in decreasing order
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  
  # create a data frame with words and their frequencies
  dm <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  nrc.lexicons <- get_nrc_sentiment(as.character(dm$word))
  
  tweets.negative <- dm[nrc.lexicons$negative>0,]
  #tweets.positive <- dm[nrc.lexicons$positive>0,]
  #tweets.neutral <- dm[nrc.lexicons$negative==0 & nrc.lexicons$positive==0,]
  
  #fig.path.pos <- "C:/Users/NeerajSubhedar/Documents/GitHub/R-scripts/Data Analytics with R project/positive.png"
  fig.path.neg <- "C:/Users/NeerajSubhedar/Documents/GitHub/R-scripts/Data Analytics with R project/negative.png"
  
  #wordcloud2(data = tweets.positive, figPath = fig.path.pos,size = 1.5,gridSize = 3)
  wordcloud2(data = tweets.negative, figPath = fig.path.neg,size = 1.5,gridSize = 3)
}

generateWordCloud.positive.TF_IDF <- function(object.with.tweets, minimum.frequency = 10){
  ## TF-IDF word cloud
  object.with.tweets <- return.object
  df.tweets <- cleanTweets(object.with.tweets)
  
  text_corpus <- Corpus(VectorSource(df.tweets$text_clean))
  # Text_corpus is a collection of tweets where every tweet is a document
  
  tdm <- DocumentTermMatrix(text_corpus, control = list(weighting = weightTfIdf))
  Zipf_plot(tdm,type = "l",col="blue")
  
  # converting term document matrix to matrix
  m <- as.matrix(tdm)
  
  word_freqs <- sort(colSums(m), decreasing = TRUE)
  # create a data frame with words and their frequencies
  
  dm <- data.frame(word = names(word_freqs), freq = word_freqs)
  plot(dm$freq,type = "l")
  
  subset.dm <- dm[dm$freq<=mean(dm$freq) + 2*sd(dm$freq) & dm$freq>=mean(dm$freq) - 2*sd(dm$freq),]
  
  tdm.word.freq <- TermDocumentMatrix(text_corpus,control = list(removePunctuation = TRUE,
                                                                 removeNumbers = TRUE,
                                                                 tolower = TRUE))
  
  m.word.freq <- as.matrix(tdm.word.freq)
  word_freqs.word.freq <- sort(colSums(m), decreasing = TRUE)
  dm.word.freq <- data.frame(word = names(word_freqs.word.freq), freq = word_freqs.word.freq)
  
  dm.word.freq.new <- dm.word.freq[dm.word.freq$word %in% subset.dm$word,]
  
  nrc.lexicons <- get_nrc_sentiment(as.character(dm.word.freq.new$word))
  
  #tweets.negative <- dm.word.freq.new[nrc.lexicons$negative>0,]
  tweets.positive <- dm.word.freq.new[nrc.lexicons$positive>0,]
  #tweets.neutral <- dm.word.freq.new[nrc.lexicons$negative==0 & nrc.lexicons$positive==0,]
  
  fig.path.pos <- "C:/Users/NeerajSubhedar/Documents/GitHub/R-scripts/Data Analytics with R project/positive.png"
  #fig.path.neg <- "C:/Users/NeerajSubhedar/Documents/GitHub/R-scripts/Data Analytics with R project/negative.png"
  
  wordcloud2(data = tweets.positive, figPath = fig.path.pos,size = 1.5,gridSize = 3)
  #wordcloud2(data = tweets.negative, figPath = fig.path.neg,size = 1.5,gridSize = 3)
}

generateWordCloud.negative.TF_IDF <- function(object.with.tweets, minimum.frequency = 10){
  ## TF-IDF word cloud
  object.with.tweets <- return.object
  df.tweets <- cleanTweets(object.with.tweets)
  
  text_corpus <- Corpus(VectorSource(df.tweets$text_clean))
  # Text_corpus is a collection of tweets where every tweet is a document
  
  tdm <- DocumentTermMatrix(text_corpus, control = list(weighting = weightTfIdf))
  Zipf_plot(tdm,type = "l",col="blue")
  
  # converting term document matrix to matrix
  m <- as.matrix(tdm)
  
  word_freqs <- sort(colSums(m), decreasing = TRUE)
  # create a data frame with words and their frequencies
  
  dm <- data.frame(word = names(word_freqs), freq = word_freqs)
  plot(dm$freq,type = "l")
  
  subset.dm <- dm[dm$freq<=mean(dm$freq) + 2*sd(dm$freq) & dm$freq>=mean(dm$freq) - 2*sd(dm$freq),]
  
  tdm.word.freq <- TermDocumentMatrix(text_corpus,control = list(removePunctuation = TRUE,
                                                                 removeNumbers = TRUE,
                                                                 tolower = TRUE))
  
  m.word.freq <- as.matrix(tdm.word.freq)
  word_freqs.word.freq <- sort(colSums(m), decreasing = TRUE)
  dm.word.freq <- data.frame(word = names(word_freqs.word.freq), freq = word_freqs.word.freq)
  
  dm.word.freq.new <- dm.word.freq[dm.word.freq$word %in% subset.dm$word,]
  
  nrc.lexicons <- get_nrc_sentiment(as.character(dm.word.freq.new$word))
  
  tweets.negative <- dm.word.freq.new[nrc.lexicons$negative>0,]
  #tweets.positive <- dm.word.freq.new[nrc.lexicons$positive>0,]
  #tweets.neutral <- dm.word.freq.new[nrc.lexicons$negative==0 & nrc.lexicons$positive==0,]
  
  #fig.path.pos <- "C:/Users/NeerajSubhedar/Documents/GitHub/R-scripts/Data Analytics with R project/positive.png"
  fig.path.neg <- "C:/Users/NeerajSubhedar/Documents/GitHub/R-scripts/Data Analytics with R project/negative.png"
  
  #wordcloud2(data = tweets.positive, figPath = fig.path.pos,size = 1.5,gridSize = 3)
  wordcloud2(data = tweets.negative, figPath = fig.path.neg,size = 1.5,gridSize = 3)
}

##########################################################################
# Get sentiments using the syuzhet package
# currently using the sentiment libraries
# syuzhet, bing, nrc and afinn
# lexicons available for nrc
# To-Be-Implemented: include Stanford CoreNLP to the sentiment libraries
# To-Be-Implemented: implement lexicons for syuzhet, afinn and bing
#
# General information about NLP, lexicons and NLP libraries
#- word, an english word (unigram)
#- sentiment, one of either positive, negative, anger, anticipation, disgust, 
#  fear, joy, sadness, surprise, trust, or NA.
#   * the Bing lexicon has positive/negative,
#   * the NRC lexicon has all options except NA, and
#   * the AFINN lexicon has only NA.
#- lexicon, the source either “nrc”, “bing” or “AFINN”
#- score, A numerical score for the sentiment. This value is NA 
#  for the Bing and NRC lexicons, and runs between -5 and 5 for the AFINN 
#  lexicon.
##########################################################################

getSentiments.all <- function(object.with.tweets){
  
  # cleans the tweets and returns them as a dataframe
  df.tweets <- cleanTweets(object.with.tweets)
  
  ###########################################
  #Implementing Lexicons using NRC library
  ###########################################
  nrc.lexicons <- get_nrc_sentiment(df.tweets$text_clean)
  
  # Barplot for emotions
  barplot(
    sort(colSums(prop.table(nrc.lexicons[, 1:8]))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = "Emotions in tweets", xlab="Percentage"
  )
  
  # Barplot for positive vs negative
  barplot(
    sort(colSums(prop.table(nrc.lexicons[, 9:10]))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = "Polarity in tweets", xlab="Percentage"
  )
}

getSentiments.TF_IDF.nrc <- function(object.with.tweets){
  
  df.tweets <- cleanTweets(object.with.tweets)
  text_corpus <- Corpus(VectorSource(df.tweets$text_clean))
  tdm <- DocumentTermMatrix(text_corpus, control = list(weighting = weightTfIdf))
  m <- as.matrix(tdm)
  
  word_freqs <- sort(colSums(m), decreasing = TRUE)
  dm <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  nrc.lex <- get_nrc_sentiment(as.character(dm$word))
  
  barplot(
    sort(colSums(prop.table(nrc.lex[, 1:8]))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = "Emotions in tweets", xlab="Percentage"
  )
  
  barplot(
    sort(colSums(prop.table(nrc.lex[, 9:10]))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = "Polarity in tweets", xlab="Percentage"
  )
}

##################
# Executable Part
##################

## clears the console
cat("\014")
## Runs the entire code with following steps:
# 1. Initializes the twitter authentication
# 2. Prompts the user to select to choose the means of importing the tweets
# 3. Gets input for search string or twitter username  and number of tweets to import
# 4. returns the tweets as a list
# 5. stores the list of tweets to return.object
project.initialize()
## clears the console
cat("\014")
return.object <- run.the.code()

#######################################s###################################
# Building corpus from the tweets which we will use to build a wordcloud
##########################################################################

## creating a copy of the tweets object
searchtweet <- return.object
## generate WordCloud
generateWordCloud.positive.tmStopWords(searchtweet)
generateWordCloud.negative.tmStopWords(searchtweet)
generateWordCloud.positive.TF_IDF(searchtweet)
generateWordCloud.negative.TF_IDF(searchtweet)
## get sentiments
getSentiments.all(searchtweet)
getSentiments.TF_IDF.nrc(searchtweet)