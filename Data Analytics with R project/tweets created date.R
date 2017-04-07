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