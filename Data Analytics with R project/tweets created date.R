class(return.object)
new.tweets.df <- twListToDF(return.object)
str(new.tweets.df)
nrow(new.tweets.df)
sum(is.na(new.tweets.df[c("latitude","longitude")]))
sum(is.na(new.tweets.df[c("created")]))

#table(new.tweets.df$created)

library(lubridate)
head(trunc.POSIXt(new.tweets.df$created,units = "hours"))

new.tweets.df$created.hours <- trunc.POSIXt(new.tweets.df$created,units = "hours")
new.tweets.df$created.days <- trunc.POSIXt(new.tweets.df$created,units = "days")

head(new.tweets.df[c("created.days","text")])
head(new.tweets.df[c("created.hours","text")])

min(ymd(new.tweets.df$created.days))
max(ymd(new.tweets.df$created.days))
min(ymd_hms(new.tweets.df$created.hours))
max(ymd_hms(new.tweets.df$created.hours))
## Can use to aggregate sentiments by hours and by days

getCreatedHours <- function(object.with.tweets){
  new.tweets.df <- twListToDF(object.with.tweets)
  new.tweets.df$created.hours <- trunc.POSIXt(new.tweets.df$created,units = "hours")
  new.tweets.df$created.hours <- ymd_hms(new.tweets.df$created.hours)
  return(new.tweets.df)
}

newDF <- getCreatedHours(return.object)

summary(newDF$created.hours)

# Removes RT
newDF$text_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", newDF$text)
# Removes @<twitter handle>
newDF$text_clean = gsub("@\\w+", "", newDF$text_clean)
# Removes punctuations
newDF$text_clean = gsub("[[:punct:]]", "", newDF$text_clean)
# Removes numbers
newDF$text_clean = gsub("[[:digit:]]", "", newDF$text_clean)
# Removes html links
newDF$text_clean = gsub("http\\w+", "", newDF$text_clean)
# Removes unnecessary spaces
newDF$text_clean = gsub("[ \t]{2,}", "", newDF$text_clean)
newDF$text_clean = gsub("^\\s+|\\s+$", "", newDF$text_clean)
# Fix for error related to formatting 'utf8towcs'"
newDF$text_clean <- str_replace_all(newDF$text_clean,"[^[:graph:]]", " ")

# Sentiments using the syuzhet NLP library
newDF$senti_syuzhet <- get_sentiment(newDF$text_clean,method = "syuzhet")
# Sentiments using the syuzhet BING library
newDF$senti_bing <- get_sentiment(newDF$text_clean,method = "bing")
# Sentiments using the syuzhet AFINN library
newDF$senti_afinn <- get_sentiment(newDF$text_clean,method = "afinn")
# Sentiments using the syuzhet NRC library
newDF$senti_nrc <- get_sentiment(newDF$text_clean,method = "nrc")

# storing all sentiments to a single dataframe
sentiments_all <- cbind.data.frame(senti_syuzhet,senti_bing,
                                   senti_afinn, senti_nrc)

newDF_hours <- newDF[c("created.hours","senti_syuzhet","senti_bing","senti_afinn","senti_nrc")]
newDF_agg1 <- aggregate.data.frame(newDF_hours,by = list(newDF$created.hours),FUN = mean)
head(newDF_agg1)
newDF_agg2 <- aggregate.data.frame(newDF_hours,by = list(newDF$created.hours),FUN = max)
head(newDF_agg2)

## evaluate percent values after breaking tweets into chunks of 10 by number of tweets
percent_sentiments_all <- sapply(newDF_agg1[c("senti_syuzhet","senti_bing","senti_afinn","senti_nrc")],get_percentage_values,bins=10)

# 2x2 matrix to plot the graphs
opar = par()
par(bg = "white", mfrow = c(2,2), las = 2, col = "blue")

# Plots for percent sentiment
plot(percent_sentiments_all[,"senti_bing"],type = "l",main = "bing sentiments")
plot(percent_sentiments_all[,"senti_afinn"],type = "l",main = "afinn sentiments")
plot(percent_sentiments_all[,"senti_nrc"],type = "l",main = "nrc sentiments")
plot(percent_sentiments_all[,"senti_syuzhet"],type = "l",main = "syuzhet sentiments")


## smoothing and normalization using fourier transformation and low pass filter
ft_vals_sentiments_all <- sapply(newDF_agg1[c("senti_syuzhet","senti_bing","senti_afinn","senti_nrc")],get_transformed_values,
                                 low_pass_size = 3, 
                                 x_reverse_len = 100,
                                 padding_factor = 2,
                                 scale_vals = TRUE,
                                 scale_range = FALSE)

# 2x2 matrix to plot the graphs after smoothing and normalization using ft and low pass
opar = par()
par(bg = "white", mfrow = c(2,2), las = 2, col = "blue")

# Plots for sentiments after smoothing using ft and low pass
plot(ft_vals_sentiments_all[,"senti_bing"],type = "l",main = "bing sentiments")
plot(ft_vals_sentiments_all[,"senti_afinn"],type = "l",main = "afinn sentiments")
plot(ft_vals_sentiments_all[,"senti_nrc"],type = "l",main = "nrc sentiments")
plot(ft_vals_sentiments_all[,"senti_syuzhet"],type = "l",main = "syuzhet sentiments")


###############
# Simple plot
# 
###############

simple_plot(newDF_agg1$senti_bing,title = "bing sentiments")
simple_plot(newDF_agg1$senti_afinn,title = "afinn sentiments")
simple_plot(newDF_agg1$senti_nrc,title = "nrc sentiments")
simple_plot(newDF_agg1$senti_syuzhet,title = "syuzhet sentiments")
