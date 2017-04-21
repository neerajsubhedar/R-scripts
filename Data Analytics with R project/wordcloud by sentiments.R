object.with.tweets <- return.object

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
tweets.positive <- dm[nrc.lexicons$positive>0,]
tweets.neutral <- dm[nrc.lexicons$negative==0 & nrc.lexicons$positive==0,]

#fig.path1 <- base::system.file("examples/positive.png",package = "wordcloud2")
#fig.path2 <- base::system.file("examples/negative.png",package = "wordcloud2")
fig.path.pos <- "C:/Users/NeerajSubhedar/Documents/GitHub/R-scripts/Data Analytics with R project/positive.png"
fig.path.neg <- "C:/Users/NeerajSubhedar/Documents/GitHub/R-scripts/Data Analytics with R project/negative.png"

wordcloud2(data = tweets.positive, figPath = fig.path.pos,size = 1.5,gridSize = 3)
wordcloud2(data = tweets.negative, figPath = fig.path.neg,size = 1.5,gridSize = 3)