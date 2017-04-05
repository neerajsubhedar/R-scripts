library(syuzhet)
sentences <- get_sentences(text1)
class(sentences)
str(sentences)
head(sentences)


searchtweet <- return.object
class(searchtweet)
library(dplyr)
str(searchtweet)

length(searchtweet)
# list to dataframe
df.tweets <- twListToDF(searchtweet)
summary(df.tweets)
head(df.tweets$text)

############################ cleaning stuff #####################
df.tweets$text1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df.tweets$text)
# Remove at people
df.tweets$text1 = gsub("@\\w+", "", df.tweets$text1)
# Remove punctuation
df.tweets$text1 = gsub("[[:punct:]]", "", df.tweets$text1)
# Remove numbers
df.tweets$text1 = gsub("[[:digit:]]", "", df.tweets$text1)
# Remove html links
df.tweets$text1 = gsub("http\\w+", "", df.tweets$text1)
# remove unnecessary spaces
df.tweets$text1 = gsub("[ \t]{2,}", "", df.tweets$text1)
df.tweets$text1 = gsub("^\\s+|\\s+$", "", df.tweets$text1)
#################################################################
## fixes the error 
## "Error in tolower(char_v) : invalid input 
## 'Back to UCONN I go í ½í¸©í ½í¹Œí ¼í¿¾' in 'utf8towcs'"
df.tweets$text1 <- str_replace_all(df.tweets$text1,"[^[:graph:]]", " ")

## Sentiments
senti_syuzhet <- get_sentiment(df.tweets$text1,method = "syuzhet")
senti_bing <- get_sentiment(df.tweets$text1,method = "bing")
senti_afinn <- get_sentiment(df.tweets$text1,method = "afinn")
senti_nrc <- get_sentiment(df.tweets$text1,method = "nrc")

# net positive or negative sentiments
#sentiments_all <- rbind(
#  sign(head(senti_syuzhet)),
#  sign(head(senti_bing)),
#  sign(head(senti_afinn)),
#  sign(head(senti_nrc))
#)

sentiments_all <- cbind.data.frame(senti_syuzhet,senti_bing,
                                   senti_afinn, senti_nrc)
write.csv(sentiments_all,"C:/Users/NeerajSubhedar/Desktop/senti/sentiment.csv")
### summary
sum(senti_syuzhet)
mean(senti_syuzhet)
plot(senti_syuzhet,type = "l")
abline(h=mean(senti_syuzhet))
plot(senti_syuzhet,type = "h")
###
## evaluate percent values after breaking tweets into chunks of 25
percent_sentiments_all <- sapply(sentiments_all,get_percentage_values,bins=25)

# 2x2 matrix for graphs
opar = par()
par(bg = "white", mfrow = c(2,2), las = 2, col = "blue")
## plots
plot(percent_sentiments_all[,"senti_bing"],type = "l",main = "bing sentiments")
plot(percent_sentiments_all[,"senti_afinn"],type = "l",main = "afinn sentiments")
plot(percent_sentiments_all[,"senti_nrc"],type = "l",main = "nrc sentiments")
plot(percent_sentiments_all[,"senti_syuzhet"],type = "l",main = "syuzhet sentiments")


## smoothing and normalization using fourier transformation and low pass filtering
ft_vals_sentiments_all <- sapply(sentiments_all,get_transformed_values,
                                 low_pass_size = 3, 
                                 x_reverse_len = 100,
                                 padding_factor = 2,
                                 scale_vals = TRUE,
                                 scale_range = FALSE)

# 2x2 matrix for graphs
opar = par()
par(bg = "white", mfrow = c(2,2), las = 2, col = "blue")
## plots
plot(ft_vals_sentiments_all[,"senti_bing"],type = "l",main = "bing sentiments")
plot(ft_vals_sentiments_all[,"senti_afinn"],type = "l",main = "afinn sentiments")
plot(ft_vals_sentiments_all[,"senti_nrc"],type = "l",main = "nrc sentiments")
plot(ft_vals_sentiments_all[,"senti_syuzhet"],type = "l",main = "syuzhet sentiments")

####################
## smoothing and normalization using Discrete cosine transform (DCT) and low pass filter
dct_vals_sentiments_all <- sapply(sentiments_all,get_dct_transform,
                                 low_pass_size = 5, 
                                 x_reverse_len = 100,
                                 scale_vals = F,
                                 scale_range = T)

# 2x2 matrix for graphs
opar = par()
par(bg = "white", mfrow = c(2,2), las = 2, col = "blue")
## plots
plot(dct_vals_sentiments_all[,"senti_bing"],type = "l",main = "bing sentiments")
plot(dct_vals_sentiments_all[,"senti_afinn"],type = "l",main = "afinn sentiments")
plot(dct_vals_sentiments_all[,"senti_nrc"],type = "l",main = "nrc sentiments")
plot(dct_vals_sentiments_all[,"senti_syuzhet"],type = "l",main = "syuzhet sentiments")


############ Simple plot
# 2x2 matrix for graphs
#opar = par()
#par(bg = "white", mfrow = c(2,2), las = 2, col = "blue")
## plots
simple_plot(sentiments_all[,"senti_bing"],title = "bing sentiments")
simple_plot(sentiments_all[,"senti_afinn"],title = "afinn sentiments")
simple_plot(sentiments_all[,"senti_nrc"],title = "nrc sentiments")
simple_plot(sentiments_all[,"senti_syuzhet"],title = "syuzhet sentiments")

##### NRC Lexicons
nrc_data <- get_nrc_sentiment(df.tweets$text1)
tweets.of.joy_nrc <- df.tweets$text1[nrc_data$joy != 0]
# sample
head(tweets.of.joy_nrc)
## barplot
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)

# Stanford CoreNLP
#stanford.NLP.path <- file.path("C:","Users","NeerajSubhedar","Documents","R",
#                               "win-library","3.3","NLP","texts")
#stanford.NLP.jar <- file.path("C:","Users","NeerajSubhedar","Documents","StanfordCoreNLP")
#stanford_data <- get_stanford_sentiment(df.tweets$text1,paste0(stanford.NLP.path,"/stanford.rds"))
#stanford_data <- get_stanford_sentiment(df.tweets$text1,
#                                        paste0(stanford.NLP.jar,
#"/stanford-english-corenlp-2016-10-31-models.jar"))

#stanford_data <- get_sentiment(df.tweets$text1,method = "stanford",stanford.NLP.jar)