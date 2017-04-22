## analyzing term document matrix

## Generate Term Document Matrix
searchtweet.tdm.tm.stopword <- tdm.tmStopWord(searchtweet.clean[1:1000,])
searchtweet.tdm.tfidf <- tdm.TFIDF(searchtweet.clean[1:1000,])
searchtweet.tdm.tm <- tdm.tm(searchtweet.clean[1:1000,])

tweets.df <- twListToDF(searchtweet)
tweets.df$text <- str_replace_all(tweets.df$text,"[^[:graph:]]", " ")
text_corpus <- Corpus(VectorSource(tweets.df$text[1:1000]))
searchtweet.tdm.raw <- TermDocumentMatrix(text_corpus,control = list(removePunctuation = TRUE,
                                                     removeNumbers = TRUE,
                                                     tolower = TRUE))

inspect(searchtweet.tdm.raw)
inspect(searchtweet.tdm.tm)
inspect(searchtweet.tdm.tm.stopword)
inspect(searchtweet.tdm.tfidf)

tfidf.matrix <- as.matrix(searchtweet.tdm.tfidf)
word_freqs <- sort(colSums(tfidf.matrix), decreasing = TRUE)
# create a data frame with words and their frequencies
dm <- data.frame(word = names(word_freqs), freq = word_freqs)
plot(dm$freq,type = "l")
plot(density(dm$freq))
Zipf_plot(tfidf.matrix,col = "red")

summary(dm$freq)

ninetieth.percentile <- quantile(dm$freq, 0.9)
ninety.fifth.percentile <- quantile(dm$freq, 0.95)

dm.subset.ninetieth <- dm[dm$freq<=ninetieth.percentile,]
dm.subset.ninety.fifth <- dm[dm$freq<=ninety.fifth.percentile,]

dm.subset.twosd.mean <- dm[dm$freq<=mean(dm$freq) + 2*sd(dm$freq),]
dm.subset.twosd.median <- dm[dm$freq<=median(dm$freq) + 2*sd(dm$freq),]

dm.subset.onesd.mean <- dm[dm$freq<=mean(dm$freq) + sd(dm$freq),]
dm.subset.onesd.median <- dm[dm$freq<=median(dm$freq) + sd(dm$freq),]