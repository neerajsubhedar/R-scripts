library(syuzhet)
sentences <- get_sentences(text1)
class(sentences)
str(sentences)
head(sentences)


searchtweet <- searchTwitteR(searchString = "UCONN",n = 2500)
class(searchtweet)
library(dplyr)
str(searchtweet)

length(searchtweet)
# list to dataframe
df.tweets <- twListToDF(searchtweet)
summary(df.tweets)
df.tweets$text

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
## evaluate percent values after breaking tweets into chunks of 10
percent_sentiments_all <- sapply(sentiments_all,get_percentage_values,bins=10)

# 2x2 matrix for graphs
opar = par()
par(bg = "white", mfrow = c(2,2), las = 2, col = "blue")
## plots
plot(percent_sentiments_all[,"senti_bing"],type = "l",main = "bing sentiments")
plot(percent_sentiments_all[,"senti_afinn"],type = "l",main = "afinn sentiments")
plot(percent_sentiments_all[,"senti_nrc"],type = "l",main = "nrc sentiments")
plot(percent_sentiments_all[,"senti_syuzhet"],type = "l",main = "syuzhet sentiments")


