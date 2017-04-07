library(tm.plugin.mail)
library(tm)

#eml.analytics <- 
convert_mbox_eml("C:/Users/NeerajSubhedar/Documents/Personal-Analytics Internships.mbox",
                                  "C:/Users/NeerajSubhedar/Documents/emls")

mail.boxsource <- MBoxSource(mbox = "C:/Users/NeerajSubhedar/Documents/Personal-Analytics Internships.mbox")

class(mail.boxsource)

require("tm")
#mails <- system.file("C:/Users/NeerajSubhedar/Documents/emls", package = "tm.plugin.mail")
analytics.mail <- VCorpus(DirSource("C:/Users/NeerajSubhedar/Documents/emls"), readerControl = list(reader = mail.boxsource$reader()))
inspect(analytics.mail)


ids <- sapply(analytics.mail, function(x) meta(x, "id"))
headings <- sapply(analytics.mail, function(x) meta(x, "heading"))
header <- lapply(analytics.mail, function(x) grep("References", attr(x, "Header"), value = TRUE))
