## Tweet text mining

#####################
# Loading libraries
#####################

library(tm)
library(NLP)
#install.packages(c("RCurl","RJSONIO","stringr","wordcloud"))
library(RCurl)
library(RJSONIO)
library(stringr)
library(wordcloud)

########################
## Clean text function
########################

# Get text data from the result of Twitter search
text1 <- sapply(return.object, function(x) x$getText())
# Remove retweets
text1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text1)
# Remove at people
text1 = gsub("@\\w+", "", text1)
# Remove punctuation
text1 = gsub("[[:punct:]]", "", text1)
# Remove numbers
text1 = gsub("[[:digit:]]", "", text1)
# Remove html links
text1 = gsub("http\\w+", "", text1)
# remove unnecessary spaces
text1 = gsub("[ \t]{2,}", "", text1)
text1 = gsub("^\\s+|\\s+$", "", text1)

##############################

clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

################
# get sentiment
################

getSentiment <- function (text, key){
  
  text <- URLencode(text);
  
  #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
  text <- str_replace_all(text, "%20", " ");
  text <- str_replace_all(text, "%\\d\\d", "");
  text <- str_replace_all(text, " ", "%20");
  
  if (str_length(text) > 360){
    text <- substr(text, 0, 359);
  }
  ##########################################
  
  data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=",text, sep=""))
  
  js <- fromJSON(data, asText=TRUE);
  
  # get mood probability
  sentiment = js$output$result
  
  ###################################
  return(list(sentiment=sentiment))
}
senti <- getSentiment(text1,key = "e0735be37cc97737e32275e5c8c921f7")
#sents <- levels(factor(senti))
##################
# Building corpus
##################

# creates a text corpus from the plain text document for every tweet
text_corpus <- Corpus(VectorSource(text1))

# creating a Term Document Matrix 
tdm <- TermDocumentMatrix(
  text_corpus,
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
# word cloud
#plotfile1 <- "krankheit_wordcloud.png" # optional
wordcloud(dm$word, dm$freq, min.freq=10, random.order = FALSE, 
          colors = brewer.pal(11, "Spectral"))
#png(filename=plotfile1, width=740, height=740, units="px") # optional
#dev.off() # optional