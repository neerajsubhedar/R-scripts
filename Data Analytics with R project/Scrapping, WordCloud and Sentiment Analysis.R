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
                        "tidytext","dplyr")
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  ## for devtools
  library(git2r)
  library(digest)
  #require(devtools)
  install_github("hadley/devtools")
  library(devtools)
  install_github("geoffjentry/twitteR")
  
  ## data manipultion
  library(dplyr);library(stringr)
  
  # loading the libraries
  ## Linked to importing tweets
  library(rjson);library(httr);library(twitteR)
  
  ## Linked to generating a wordcloud
  library(tm);library(NLP);library(RCurl);library(RJSONIO)
  library(stringr);library(wordcloud)
  
  ## Linked to sentiment analysis
  library(syuzhet)
  
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
# To-Be-Implemented: handle error for mismatch in number of tweets
searchThis <- function(search_string,geocode_string = "42.375,-71.1061111,1000mi",number.of.tweets = 100){
  searchTwitter(search_string, geocode=geocode_string,n = number.of.tweets)
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

#################################################################################
# Function to run functions associated with user input and importing the tweets
#################################################################################
# Executes the part of code that imports tweets
# Returns tweets as a list
run.the.code <- function(){
  # Gets users choice
  choice <- getType()
  
  # Selects whether user wants to search by string or by user based the choice 
  if (choice == 1) {
    find.on.twitter <- getString()
    num <- getNumber()
    # returns tweets as a list
    out <- searchThis(search_string = find.on.twitter,
                      number.of.tweets = num)
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

generateWordCloud <- function(object.with.tweets, minimum.frequency = 10){
  ## importing the tweets object to a dataframe
  df.tweets <- twListToDF(object.with.tweets)
  ## defining a new object to store tweets after cleaning
  df.tweets$text_clean <- NULL
  # Removes 'RT'
  df.tweets$text_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df.tweets$text)
  # Removes '@<twitter handle>
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
  ############################################################
  ## fix for an error 
  ## "Error in tolower(char_v) : invalid input 
  ## <invalid input string with error> in 'utf8towcs'"
  ############################################################
  df.tweets$text_clean <- str_replace_all(df.tweets$text_clean,"[^[:graph:]]", " ")
  
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
  
  # creating word cloud
  # wordcloud(word, associated frequency, ordering, color palette)
  wordcloud(dm$word, dm$freq, min.freq=minimum.frequency, random.order = FALSE, 
            colors = brewer.pal(11, "Spectral"))
  #png(filename=plotfile1, width=740, height=740, units="px") # optional
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

getSentiments <- function(object.with.tweets){
  
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
  
  # Sentiments using the syuzhet NLP library
  senti_syuzhet <- get_sentiment(df.tweets$text_clean,method = "syuzhet")
  # Sentiments using the syuzhet BING library
  senti_bing <- get_sentiment(df.tweets$text_clean,method = "bing")
  # Sentiments using the syuzhet AFINN library
  senti_afinn <- get_sentiment(df.tweets$text_clean,method = "afinn")
  # Sentiments using the syuzhet NRC library
  senti_nrc <- get_sentiment(df.tweets$text_clean,method = "nrc")
  
  # storing all sentiments to a single dataframe
  sentiments_all <- cbind.data.frame(senti_syuzhet,senti_bing,
                                     senti_afinn, senti_nrc)
  
  ## evaluate percent values after breaking tweets into chunks of 10 by number of tweets
  percent_sentiments_all <- sapply(sentiments_all,get_percentage_values,bins=10)
  
  # 2x2 matrix to plot the graphs
  opar = par()
  par(bg = "white", mfrow = c(2,2), las = 2, col = "blue")
  
  # Plots for percent sentiment
  plot(percent_sentiments_all[,"senti_bing"],type = "l",main = "bing sentiments")
  plot(percent_sentiments_all[,"senti_afinn"],type = "l",main = "afinn sentiments")
  plot(percent_sentiments_all[,"senti_nrc"],type = "l",main = "nrc sentiments")
  plot(percent_sentiments_all[,"senti_syuzhet"],type = "l",main = "syuzhet sentiments")
  
  ## smoothing and normalization using fourier transformation and low pass filter
  ft_vals_sentiments_all <- sapply(sentiments_all,get_transformed_values,
                                   low_pass_size = 3, 
                                   x_reverse_len = 100,
                                   padding_factor = 2,
                                   scale_vals = TRUE,
                                   scale_range = FALSE)
  
  # 2x2 matrix to plot the graphs after smoothing and normalization using ft and low pass
  opar = par()
  par(bg = "white", mfrow = c(2,2), las = 2, col = "blue")
  
  # Plots for sentiments after smoothing using ft and low pass
  plot(ft_vals_sentiments_all[,"senti_bing"],type = "l",main = "bing sentiments")
  plot(ft_vals_sentiments_all[,"senti_afinn"],type = "l",main = "afinn sentiments")
  plot(ft_vals_sentiments_all[,"senti_nrc"],type = "l",main = "nrc sentiments")
  plot(ft_vals_sentiments_all[,"senti_syuzhet"],type = "l",main = "syuzhet sentiments")
  
  ## smoothing and normalization using Discrete cosine transform (DCT) and low pass filter
  dct_vals_sentiments_all <- sapply(sentiments_all,get_dct_transform,
                                    low_pass_size = 5, 
                                    x_reverse_len = 100,
                                    scale_vals = F,
                                    scale_range = T)
  
  # 2x2 matrix to plot the graphs after smoothing and normalization using DCT and low pass filtering
  opar = par()
  par(bg = "white", mfrow = c(2,2), las = 2, col = "blue")
  
  # Plots for sentiments after smoothing using DCT and low pass
  plot(dct_vals_sentiments_all[,"senti_bing"],type = "l",main = "bing sentiments")
  plot(dct_vals_sentiments_all[,"senti_afinn"],type = "l",main = "afinn sentiments")
  plot(dct_vals_sentiments_all[,"senti_nrc"],type = "l",main = "nrc sentiments")
  plot(dct_vals_sentiments_all[,"senti_syuzhet"],type = "l",main = "syuzhet sentiments")
  
  ###############
  # Simple plot
  # 
  ###############
  
  simple_plot(sentiments_all[,"senti_bing"],title = "bing sentiments")
  simple_plot(sentiments_all[,"senti_afinn"],title = "afinn sentiments")
  simple_plot(sentiments_all[,"senti_nrc"],title = "nrc sentiments")
  simple_plot(sentiments_all[,"senti_syuzhet"],title = "syuzhet sentiments")
  
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
    main = "Ratio of positive to negative tweets", xlab="Percentage"
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
return.object <- run.the.code()

##########################################################################
# Building corpus from the tweets which we will use to build a wordcloud
##########################################################################

## creating a copy of the tweets object
searchtweet <- return.object
## generate WordCloud
generateWordCloud(searchtweet)
## get sentiments
getSentiments(searchtweet)