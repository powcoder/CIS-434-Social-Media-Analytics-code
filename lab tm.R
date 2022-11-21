rm(list=ls())
library(tm)

###########################################
##########   1. Building Corpus   #########
###########################################

csvdata <- read.csv("~/data/movietweets.csv", header=TRUE, sep=',', quote='"')

# Method 1: VectorSource
sentence <- c("social media is a new media", "social media is fun")
docs <- Corpus( VectorSource(sentence) )
writeCorpus(docs, filenames=c("doc1.txt", "doc2.txt"))

docs <- Corpus( VectorSource(csvdata$tweet) )


# Method 2: DataframeSource
mtweets <- csvdata[,1:2]
names(mtweets) = c("doc_id", "text") # The 1st column must be named "doc_id" and contain a unique string identifier for each document. 
# The 2nd column must be named "text".
docs <- Corpus( DataframeSource(mtweets) )

# Method 3: DirSource
docs<-Corpus(DirSource( "~/data/pediatrician" ))


###########################################
##########   2. Managing Corpus   #########
###########################################

# understanding the structure of a document object
mode(docs[[1]])
length(docs[[1]])
names(docs[[1]])

docs[[1]]$content
docs[[1]]$meta

mode(docs[[1]][[2]])
length(docs[[1]][[2]])
names(docs[[1]][[2]])
mode( docs[[1]][[2]]$id )


# transformations
docs <- tm_map(docs, content_transformer(tolower))
docs[[1]]$content

docs <- tm_map(docs, removePunctuation)
docs[[1]]$content

docs <- tm_map(docs,removeWords,c("dr", "doctor"))
docs[[1]]$content

docs <- tm_map(docs,removeNumbers)
docs[[1]]$content

docs <- tm_map(docs,removeWords,stopwords("english"))
docs[[1]]$content

docs <- tm_map(docs,stripWhitespace)
docs[[1]]$content

docs<-tm_map(docs,stemDocument)
docs[[1]]$content


###########################################
########   3. Document-Term Matrix  #######
###########################################
docs <- Corpus(VectorSource( c("social media is a new media", "social media is fun") ))
dtm <- DocumentTermMatrix( docs )
# The following is equivalent. By default, weighting=weightTF 
dtm <- DocumentTermMatrix( docs, control=list(weighting=weightTf) )
as.matrix(dtm)

# Note the tm package use log2() instead of log() to calculate IDF
dtm <- DocumentTermMatrix( docs, control=list(weighting=function(x)weightTfIdf(x, normalize = FALSE)) )
as.matrix(dtm)

# The followings are equivalent. By default, normalize = TRUE.
dtm <- DocumentTermMatrix( docs, control=list(weighting=function(x)weightTfIdf(x, normalize = TRUE)) )
dtm <- DocumentTermMatrix( docs, control=list(weighting=weightTfIdf) )
as.matrix(dtm)

###########################################
#######  4. An Example with ratemds  ######
###########################################

dir("~/data/ratemds")
docs<-Corpus(DirSource( "~/data/ratemds" ))
docs<-tm_map(docs,removeWords,c("dr", "doctor", "patient"))

dtm <- DocumentTermMatrix(docs, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, stopwords=stopwords("english")))
inspect(dtm[1:10,1:8])
dim(dtm)

dtm <- removeSparseTerms(dtm,0.95)
inspect(dtm[1:10,1:8])
dim(dtm)

findFreqTerms(dtm, lowfreq=1500)
findAssocs(dtm, "staff", corlimit=0.98)

freq = colSums( as.matrix(dtm) )
freq.sorted = sort( freq, decreasing=TRUE )
freq.sorted[1:10]

Zipf_plot(dtm)

mydic <- c("recommend", "time", "wait", "wonder")
mydtm <- DocumentTermMatrix(docs, list(dictionary=mydic))
inspect(mydtm[1:10,])
dim(mydtm)


library(wordcloud)
#set.seed(123)
wordcloud(names(freq), freq, min.freq=5000)

# the following 2 are equivalent
wordcloud(names(freq), freq, max.words=5)
wordcloud(names(freq.sorted), freq.sorted, max.words=5)

# add color
wordcloud(names(freq), freq, max.words=10, colors=brewer.pal(6,"Dark2"))


# Term Frequency - Inverse Document Frequency (TF-IDF)

dtm.tfidf <- DocumentTermMatrix(docs, control=list(weighting=weightTfIdf)) # TF is normalized by default, see manual

specialty <- 3
freq <- as.matrix(dtm)[specialty,]
freq.tfidf <- as.matrix(dtm.tfidf)[specialty,]
wordcloud(names(freq), freq, max.words=10)
wordcloud(names(freq.tfidf), freq.tfidf, max.words=10)
