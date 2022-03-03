##############################################################################
#                         Text Mining Script
##############################################################################
##############################################################################
#                     Load Libraries
##############################################################################
if (!("tm" %in% rownames(installed.packages())))
 {install.packages("tm",dependencies = TRUE)}

if (!("qdap" %in% rownames(installed.packages())))
 {install.packages("qdap",dependencies = TRUE)}

library(tm,quietly =T)
library(sentiment,quietly =T)
library(qdap)
#suppressPackageStartupMessages(library(ggplot2))
##############################################################################
#                   Self-defined Functions
##############################################################################
###function to detect the command string
str_detect <- function(cmdString, string)
{
 if (cmdString == string)
   { return (TRUE)}

return (FALSE)
}
###function to pre-process text
preprocessText<-function(inText, cmdString)
{
  # Create a corpus from the input text column
  inTextSrc <- VectorSource(inText)
  outCorp <- Corpus(inTextSrc)

  # if n, remove numbers
  if(str_detect(cmdString, "n"))
  {
   outCorp <- tm_map(outCorp, removeNumbers)
  }
  # if p, remove punctuation
  if(str_detect(cmdString, "p"))
  {
   outCorp <- tm_map(outCorp, removePunctuation)
  }
  # if w, remove extra white space
  if(str_detect(cmdString, "w"))
  {
   outCorp <- tm_map(outCorp, stripWhitespace)
  }
  # if s, remove english stopwords
  if(str_detect(cmdString, "s"))
  {
   outCorp <- tm_map(outCorp, removeWords, stopwords("en"))
  }
  # if w, remove extra white space
  if(str_detect(cmdString, "l"))
  {
   outCorp <- tm_map(outCorp, tolower)
  }
  # Convert the corpus back to a list of strings
  unlist(outCorp)
}
##############################################################################
#                        Input Arguments
##############################################################################
x = commandArgs();
#print (x)
#inText 
x[3] <- "C:\\work\\Textmining"
x[4] <- "inText_Ali.csv"
inText <- read.table(file.path(x[3],x[4]), header = F)
#cmdString
x[5] <- "n"
##############################################################################
#                        Output
##############################################################################
#outText 
x[6] <- "outText.txt"
outText <- preprocessText(inText, x[5])
write.table(outText, file.path(x[3],x[6]),row.names = F,col.names= F)
##############################################################################
#                 Construct a Term Document Matrix
##############################################################################
tdm <- inspect(TermDocumentMatrix(Corpus(VectorSource(inText))))
x[7] <- "termDocumentMatrix.csv"
write.csv(tdm, file.path(x[3],x[7]))
##############################################################################
#                 Sentiment Analysis I
##############################################################################
x[8] <- "SentimentAnalysis.csv"
sa <- sentiment(inText)
write.csv(sa, file.path(x[3],x[8]))
#x[8] <- "Emotion.csv"
#x[9] <- "Polarity.csv"
## classify emotion
#class_emo = classify_emotion(inText, algorithm="bayes", prior=1.0)
## get emotion best fit
#emotion = class_emo[,7]
## substitute NA's by "unknown"
#emotion[is.na(emotion)] = "unknown"
## classify polarity
#class_pol = classify_polarity(inText, algorithm="bayes")
## get polarity best fit
#polarity = class_pol[,4]
## data frame with results
#sent_df = data.frame(text=inText, emotion=emotion,
#polarity=polarity, stringsAsFactors=FALSE)
#
#write.csv(class_emo, file.path(x[3],x[8]))
#write.csv(class_pol, file.path(x[3],x[9]))
###functions are deprecated.
## sort data frame
#sent_df = within(sent_df,
#  emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
## plot distribution of emotions
#ggplot(sent_df, aes(x=emotion)) +
#geom_bar(aes(y=..count.., fill=emotion)) +
#scale_fill_brewer(palette="Dark2") +
#labs(x="emotion categories", y="number of tweets") +
#opts(title = "Sentiment Analysis of Tweets about Starbucks\n(classification by emotion)",
#     plot.title = theme_text(size=12))
## plot distribution of polarity
#ggplot(sent_df, aes(x=polarity)) +
#geom_bar(aes(y=..count.., fill=polarity)) +
#scale_fill_brewer(palette="RdGy") +
#labs(x="polarity categories", y="number of tweets") +
#opts(title = "Sentiment Analysis of Tweets about Starbucks\n(classification by polarity)",
#     plot.title = theme_text(size=12))
## separating text by emotion
#emos = levels(factor(sent_df$emotion))
#nemo = length(emos)
#emo.docs = rep("", nemo)
#for (i in 1:nemo)
#{
#   tmp = inText[emotion == emos[i]]
#   emo.docs[i] = paste(tmp, collapse=" ")
#}
#
## remove stopwords
#emo.docs = removeWords(emo.docs, stopwords("english"))
## create corpus
#corpus = Corpus(VectorSource(emo.docs))
#tdm = TermDocumentMatrix(corpus)
#tdm = as.matrix(tdm)
#colnames(tdm) = emos
#
## comparison word cloud
#comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
#   scale = c(3,.5), random.order = FALSE, title.size = 1.5)

##############################################################################
#                 Sentiment Analysis II
##############################################################################

