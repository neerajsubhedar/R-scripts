library(tm.plugin.mail)
library(tm)

#eml.analytics <- 
#convert_mbox_eml("C:/Users/NeerajSubhedar/Documents/Personal-Analytics Internships.mbox",
#                                  "C:/Users/NeerajSubhedar/Documents/emls")

mail.boxsource <- MBoxSource(mbox = "C:/Users/NeerajSubhedar/Documents/Personal-Analytics Internships.mbox")

class(mail.boxsource)

require("tm")
#mails <- system.file("C:/Users/NeerajSubhedar/Documents/emls", package = "tm.plugin.mail")
analytics.mail <- VCorpus(DirSource("C:/Users/NeerajSubhedar/Documents/emls"), readerControl = list(reader = mail.boxsource$reader()))
inspect(analytics.mail)


ids <- sapply(analytics.mail, function(x) meta(x, "id"))
headings <- sapply(analytics.mail, function(x) meta(x, "heading"))
header <- lapply(analytics.mail, function(x) grep("References", attr(x, "Header"), value = TRUE))

headings = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", headings)
# Removes '@<twitter handle>
headings = gsub("@\\w+", "", headings)
# Removes punctuations
headings = gsub("[[:punct:]]", "", headings)
# Removes numbers
headings = gsub("[[:digit:]]", "", headings)
# Removes html links
headings = gsub("http\\w+", "", headings)
# Removes unnecessary spaces
headings = gsub("[ \t]{2,}", "", headings)
headings = gsub("^\\s+|\\s+$", "", headings)
############################################################
## fix for an error 
## "Error in tolower(char_v) : invalid input 
## <invalid input string with error> in 'utf8towcs'"
############################################################
library(stringr)
headings <- str_replace_all(headings,"[^[:graph:]]", " ")
head(headings)

text_corpus <- Corpus(VectorSource(headings))
# Text_corpus is a collection of tweets where every tweet is a document

# creating a Term Document Matrix 
tdm <- TermDocumentMatrix(
  # the text corpus created from the text_clean object
  text_corpus,
  # defining the stopwords to be removed before creating a term document matrix
  control = list(
      removePunctuation = TRUE,
    stopwords("en")
      #stopwords = c("thank","you","for","your","application","applying",
      #              "data","intern","analytics","analyst"),
    ,removeNumbers = TRUE,
    tolower = TRUE)
)

# converting term document matrix to matrix
m <- as.matrix(tdm)

# get word counts in decreasing order
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# create a data frame with words and their frequencies
dm <- data.frame(word = names(word_freqs), freq = word_freqs)

library(wordcloud)
# creating word cloud
# wordcloud(word, associated frequency, ordering, color palette)
wordcloud(dm$word, dm$freq, min.freq=2, random.order = FALSE, 
          colors = brewer.pal(11, "Spectral"))