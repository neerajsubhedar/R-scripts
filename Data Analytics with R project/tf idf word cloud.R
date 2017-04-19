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

plot(density(dm$freq),type = "l")
abline(v = mean(dm$freq), col = "red")
abline(v = mean(dm$freq) - 2*sd(dm$freq), col = "blue")
abline(v = mean(dm$freq) + 2*sd(dm$freq), col = "blue")

subset.dm <- dm[dm$freq<=mean(dm$freq) + 2*sd(dm$freq) & dm$freq>=mean(dm$freq) - 2*sd(dm$freq),]

tdm.word.freq <- TermDocumentMatrix(text_corpus,control = list(removePunctuation = TRUE,
                                                               removeNumbers = TRUE,
                                                               tolower = TRUE))
m.word.freq <- as.matrix(tdm.word.freq)
word_freqs.word.freq <- sort(colSums(m), decreasing = TRUE)
dm.word.freq <- data.frame(word = names(word_freqs.word.freq), freq = word_freqs.word.freq)

dm.word.freq.new <- dm.word.freq[dm.word.freq$word %in% subset.dm$word,]

wordcloud2(data = dm.word.freq.new)