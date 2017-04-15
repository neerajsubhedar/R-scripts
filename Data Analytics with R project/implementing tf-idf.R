## Cleans the tweets ans stores them as a dataframe
  #df.tweets <- cleanTweets(object.with.tweets)
  df.tweets <- cleanTweets(object.with.tweets)
  # Creates a text corpus from the plain text document for every tweet
  #df.tweets$text_clean <- str_replace_all(df.tweets$text_clean,"[^[:graph:]]", " ")
  text_corpus <- Corpus(VectorSource(df.tweets$text_clean))
  # Text_corpus is a collection of tweets where every tweet is a document
  
  tdm <- DocumentTermMatrix(text_corpus, control = list(weighting = weightTfIdf))
  tdm <- removeSparseTerms(tdm,sparse = 0.95)
  
  Zipf_plot(tdm,type = "l",col="blue")
  # the line
  inspect(tdm)
  
  # converting term document matrix to matrix
  m <- as.matrix(tdm)
  head(m)
  str(m)
  
  # get word counts in decreasing order
  #word_freqs <- sort(rowSums(m), decreasing = TRUE)
  word_freqs <- sort(colSums(m), decreasing = TRUE)
  # create a data frame with words and their frequencies
  dm <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  ###########
  senti_nrc <- get_sentiment(as.character(dm$word),method = "nrc")
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
    main = "Emotions in tweets", xlab="Percentage"
  )
  ###########
  
  plot(dm$freq,type = "l")
  plot(density(dm$freq),type = "l")
  abline(v = mean(dm$freq), col = "red")
  #abline(v = median(dm$freq), col = "blue")
  #abline(v = mean(dm$freq) - sd(dm$freq), col = "green")
  #abline(v = mean(dm$freq) + sd(dm$freq), col = "green")
  abline(v = mean(dm$freq) - 2*sd(dm$freq), col = "blue")
  abline(v = mean(dm$freq) + 2*sd(dm$freq), col = "blue")
  subset.dm <- dm[dm$freq<=mean(dm$freq) + 2*sd(dm$freq) & dm$freq>=mean(dm$freq) - 2*sd(dm$freq),]
  plot(subset.dm$freq, type = "l")
  # creating word cloud
  # wordcloud(word, associated frequency, ordering, color palette)
  #wordcloud(dm$word, dm$freq, min.freq=minimum.frequency, random.order = FALSE, 
  #          colors = brewer.pal(11, "Spectral"))
  wordcloud2(data = subset.dm)
  #png(filename=plotfile1, width=740, height=740, units="px") # optional
  
