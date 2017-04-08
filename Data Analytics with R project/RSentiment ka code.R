saaf.tweets <- cleanTweets(return.object)

rsenti.score <- calculate_sentiment(saaf.tweets$text_clean)
rsenti.score

# Creates a text corpus from the plain text document for every tweet
text_corpus <- Corpus(VectorSource(saaf.tweets$text_clean))
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

#text <- as.data.frame(data = c("This is cool.","This is awesome","This is horrible","Shut up","Fuck you"))
rsenti.score <- calculate_sentiment(m)
rsenti.score.1 <- calculate_score(m)

