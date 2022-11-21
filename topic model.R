rm(list=ls())
library(tm)
library(wordcloud)
library(topicmodels)

#############   Term Frequency   ############

docs<-Corpus(DirSource( "~/data/ebola/20days" ))
mystop=c('ebola', 'replacemovietitlewithebola', 'http', 'https', '“', '”', 'amp', 'via', 'don', 'dont')
dtm <- DocumentTermMatrix(docs, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, stopwords=c(mystop, stopwords("english"), stopwords("spanish"))))

currentdate = '14'
filename = paste("ebola", currentdate, ".dat", sep='')
freq <- as.matrix(dtm)[filename,]
wordcloud(names(freq), freq, max.words=15, colors=brewer.pal(6,"Dark2"))

##########   Topic Modelling   ############

docs<-Corpus(DirSource( "~/data/ebola/ebola14") )
dtm <- DocumentTermMatrix(docs, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, stopwords=c(mystop, stopwords("english"), stopwords("spanish"))))
dtm = removeSparseTerms(dtm,0.996)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- docs[idx]
dtm = dtm[idx,]

# Estimate the LDA model
lda.model = LDA(dtm, 12)

###########   Post Analysis   #############

myposterior <- posterior(lda.model) # get the posterior of the model

# topic distribution of each document, one row per document, one column per topic
coins = myposterior$topics 
barplot(coins[1:2,], beside=TRUE, col=c("red","blue")) # plot topic distribution of specific documents

# term distribution of each topic, one row per topic, one column per term
dices = myposterior$terms
tid <- 3
dice <- dices[tid, ] # the probability of each term in a given topic
barplot( dice, names.arg=rep("",length(dice)) )
freqterms = sort( dice, decreasing=TRUE )
freqterms[1:10]
barplot(freqterms[1:10]) # plot top 10 terms of a given topic
wordcloud(names(dice), dice, max.words=20, colors=brewer.pal(6,"Dark2"))

ix = sort( topics[,tid], decreasing = TRUE, index.return=TRUE )$ix # sort documents by topic proportion
newdocs[ ix[1:5] ]$"content" # check textual contents of documents with highe proportion of the topic