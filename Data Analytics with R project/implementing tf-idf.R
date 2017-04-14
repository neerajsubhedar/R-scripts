generateWordCloud.tmTfIdf <- function(object.with.tweets, minimum.frequency = 10){
  
  ## Cleans the tweets ans stores them as a dataframe
  #df.tweets <- cleanTweets(object.with.tweets)
  df.tweets <- cleanTweets(object.with.tweets)
  # Creates a text corpus from the plain text document for every tweet
  df.tweets$text_clean <- str_replace_all(df.tweets$text_clean,"[^[:graph:]]", " ")
  text_corpus <- Corpus(VectorSource(df.tweets$text_clean))
  # Text_corpus is a collection of tweets where every tweet is a document
  
  tdm <- DocumentTermMatrix(text_corpus, control = list(weighting = weightTfIdf))
  tdm <- removeSparseTerms(tdm,sparse = 0.95)
  
  inspect(tdm)
  
  # converting term document matrix to matrix
  m <- as.matrix(tdm)
  
  # get word counts in decreasing order
  #word_freqs <- sort(rowSums(m), decreasing = TRUE)
  word_freqs <- sort(colSums(m), decreasing = TRUE)
  # create a data frame with words and their frequencies
  dm <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  # creating word cloud
  # wordcloud(word, associated frequency, ordering, color palette)
  #wordcloud(dm$word, dm$freq, min.freq=minimum.frequency, random.order = FALSE, 
  #          colors = brewer.pal(11, "Spectral"))
  wordcloud2(data = dm,minSize = 5)
  #png(filename=plotfile1, width=740, height=740, units="px") # optional
}
