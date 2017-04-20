cat("\014")
## clear global variables
rm(list=ls())

## list of packages required
list.of.packages <- c("git2r","digest","devtools",
                      "RCurl","RJSONIO","stringr","syuzhet","httr",
                      "rjson","tm","NLP","RCurl","wordcloud","wordcloud2",
                      "tidytext","dplyr","zipcode","bit", "shiny")

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
library(stringr);library(wordcloud);library(wordcloud2); library(shiny)

## Linked to sentiment analysis
library(syuzhet)

# Twitter authentication key
oauth <- setup_twitter_oauth(consumer_key = "RTPZs421qBw2rnPtNGsaG6V7S",
                             consumer_secret = "kY4CgL7SdnYtkLqobAqMqDBNc7ASJ2Ks7rTLG4HhLbH7tUBfIv",
                             access_token = "75229041-KfJBkGaKgZCSLWSF1kuQbsAZHjBS4bGyntg7tTzbE",
                             access_secret = "M63vL0YGCQ8HGzYJEy5wccAtKq3aBOEloxl1hZXYXCSQj")

# Returns a vector containing latitude and longitude with zipcode and radius as inputs





ui <- fluidPage(
  titlePanel("Sentiment Analysis by abs(R)"),
  sidebarLayout(
    sidebarPanel( width = 4,
      radioButtons("typeInput", "Choose an Input type for twitter search:",
                   list("Search by hashtag and location" = "hashtag", "Search by Twitter username"= "username")),
      sliderInput("numberInput", "Number of tweets", min = 0, max = 3000, value = 100),
      
      
      
      #Only show this panel if the Input Type is "search by hashtag and location"    
      
      conditionalPanel(
        condition = "input.typeInput == 'hashtag'",
        textInput("hashtagInput", "Hashtag","Uber"),
        textInput("zipInput", "Enter zipcode (should be between 00210 and 99950)", "06105"),
        textInput("radiusInput", "Enter the radius (miles)","100")),
      
      #Only show this panel if the Input Type is "search by Twitter username"
      conditionalPanel(
        condition = "input.typeInput == 'username'",
        textInput("usernameInput", "Username", "AnkitRB")),
      
      actionButton("goButton", "Go!", icon("twitter")),
      p("Click the button to update main panel content.")
      
    ),
    mainPanel(
      
      tabsetPanel(
        tabPanel("Plot1", plotOutput("senti")), 
        tabPanel("Plot2", plotOutput("senti1"))#, 
        #tabPanel("Table", tableOutput("table")
        )
      )
  )
  
)

server = function(input, output)
{
  
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
  
  searchThis <- function(search_string,geocode_string = "42.375,-71.1061111,1000mi",number.of.tweets = 100)
  {
    searchTwitter(search_string, geocode=geocode_string,n = number.of.tweets, lang = "en")
  }
  
  data("zipcode")
  attach(zipcode)
  
  getLatLong.zip <- function(enter.zipcode,radius.mi)
  {
    enter.zipcode <- as.character(enter.zipcode)
    radius.mi <- as.character(radius.mi)
    lat.long <- zipcode[zip == enter.zipcode,c("latitude","longitude")]
    lat.long.mi <- paste0(lat.long$latitude,",",lat.long$longitude,",",radius.mi,"mi")
    return(lat.long.mi)
  }
  
  
  #geocode.string <- getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
  
  data1 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      geocode.string <- getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
      
      run.the.code <- searchThis(search_string = input$hashtagInput,
                                 number.of.tweets = input$numberInput, geocode_string = geocode.string)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      userTL <- function(user.name,number.of.tweets = 100)
      {
        userTimeline(user.name,n = number.of.tweets)
      }
      
      run.the.code <- userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    
    df.tweets <- cleanTweets(run.the.code)
    
    nrc.lexicons <- get_nrc_sentiment(df.tweets$text_clean)
  })
  
  output$senti = renderPlot(
   { # { opar = par()
    #   par(bg = "white", mfrow = c(1,2), las = 1, col = "blue")
    #   
      # Barplot for emotions
      barplot(
        sort(colSums(prop.table(data1()[, 1:8]))), 
        horiz = TRUE, 
        cex.names = 0.76, 
        las = 1, 
        main = "Emotions in tweets", xlab="Percentage"
      )
    })  
  
  output$senti1 = renderPlot(
    { 
    #   opar = par()
    # par(bg = "white", mfrow = c(1,2), las = 1, col = "blue")
    # 
    # Barplot for positive vs negative
    barplot(
      sort(colSums(prop.table(data1()[, 9:10]))), 
      horiz = TRUE, 
      cex.names = 0.76, 
      las = 1, 
      main = "Ratio of positive to negative tweets", xlab="Percentage"
    )
    
    })
  
  data2 = eventReactive(input$goButton, {
    
    if (input$typeInput == "hashtag") 
    {
      
      geocode.string <- getLatLong.zip(enter.zipcode = input$zipInput,radius.mi = input$radiusInput)
      
      run.the.code <- searchThis(search_string = input$hashtagInput,
                                 number.of.tweets = input$numberInput, geocode_string = geocode.string)
      
    } 
    
    else if (input$typeInput == "username") 
    {
      userTL <- function(user.name,number.of.tweets = 100)
      {
        userTimeline(user.name,n = number.of.tweets)
      }
      
      run.the.code <- userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
    }
    
    else {}
    
    
    df.tweets <- cleanTweets(run.the.code)
    
    Encoding(df.tweets$text_clean) = "latin1"
    iconv(df.tweets$text_clean, "latin1", "ASCII", sub = "")
    
    # Creates a text corpus from the plain text document for every tweet
    text_corpus <- Corpus(VectorSource(df.tweets$text_clean))
    #text_corpus <- tm_map(text_corpus, removeWords, c(stopwords("en"), input$hashtagInput, "uber"))
    # Text_corpus is a collection of tweets where every tweet is a document
    print(input$hashtagInput)
    # creating a Term Document Matrix 
    tdm <- TermDocumentMatrix(
      # the text corpus created from the text_clean object
      text_corpus,
      # defining the stopwords to be removed before creating a term document matrix
      control = list(
        removePunctuation = TRUE,
        removeNumbers = TRUE,
        stopwords = TRUE,
        tolower = TRUE)
    )
    
    # converting term document matrix to matrix
    m <- as.matrix(tdm)
    
    # get word counts in decreasing order
    word_freqs <- sort(rowSums(m), decreasing = TRUE)
    
    # create a data frame with words and their frequencies
    dm <- data.frame(word = names(word_freqs), freq = word_freqs)
    print(head(dm))
    # creating word cloud
    # wordcloud(word, associated frequency, ordering, color palette)
    #wordcloud(dm$word, dm$freq, min.freq=minimum.frequency, random.order = FALSE, 
    #          colors = brewer.pal(11, "Spectral"))
    
  })
  
  output$wordCloud =  renderPlot({wordcloud2(data = data2(), size = 1)})
  
  
}



shinyApp(ui, server)


