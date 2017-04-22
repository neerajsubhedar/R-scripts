## analyzing term document matrix

## Generate Term Document Matrix
searchtweet.tdm.tm.stopword <- tdm.tmStopWord(searchtweet.clean[1:10000,])
searchtweet.tdm.tfidf <- tdm.TFIDF(searchtweet.clean[1:10000,])
searchtweet.tdm.tm <- tdm.tm(searchtweet.clean[1:10000,])

tweets.df <- twListToDF(searchtweet)
tweets.df$text <- str_replace_all(tweets.df$text,"[^[:graph:]]", " ")
text_corpus <- Corpus(VectorSource(tweets.df$text[1:10000]))
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
tenth.percentile <- quantile(dm$freq, 0.1)
fifth.percentile <- quantile(dm$freq, 0.05)
firstquartile <- quantile(dm$freq,0.25)

############################################################################

plot(1:nrow(dm),dm$freq, type = "l")
#abline(h = ninety.fifth.percentile,col = "green")
#abline(h = ninetieth.percentile,col="blue")
#abline(h = tenth.percentile,col="yellow")
#abline(h = fifth.percentile,col = "red")
abline(h=mean(dm$freq))

ninetieth.percentile.log <- quantile(log(dm$freq), 0.9)
ninety.fifth.percentile.log <- quantile(log(dm$freq), 0.95)
tenth.percentile.log <- quantile(log(dm$freq), 0.05)
fifth.percentile.log <- quantile(log(dm$freq), 0.1)

plot(log(1:nrow(dm)),log(dm$freq), type = "l")
#abline(h = ninety.fifth.percentile.log,col = "red")
#abline(h = ninetieth.percentile.log,col="blue")
#abline(h = tenth.percentile.log,col="blue")
#abline(h = fifth.percentile.log,col = "red")
abline(h=mean(dm$freq))

plot(density(dm$freq))
abline(v=mean(dm$freq))
abline(v=mean(dm$freq) + sd(dm$freq),col="blue")
abline(v=mean(dm$freq) + 2*sd(dm$freq),col="red")

dm.subset.ninetieth <- dm[dm$freq<=ninetieth.percentile,]
dm.subset.ninety.fifth <- dm[dm$freq<=ninety.fifth.percentile,]

dm.subset.twosd.mean <- dm[dm$freq<=mean(dm$freq) + 2*sd(dm$freq),]
dm.subset.twosd.median <- dm[dm$freq<=median(dm$freq) + 2*sd(dm$freq),]

dm.subset.onesd.mean <- dm[dm$freq<=mean(dm$freq) + sd(dm$freq),]
dm.subset.onesd.median <- dm[dm$freq<=median(dm$freq) + sd(dm$freq),]

############################################################################

dm.subset.median <- dm[dm$freq>=median(dm$freq),]
dm.subset.1stquantile <- dm[dm$freq>=firstquartile,]