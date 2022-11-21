rm(list=ls())
library(tm)
library(NMF)

docs<-Corpus( DirSource("~/data/yelp/") )
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"),stopwords("spanish"), stopwords("portuguese")),stemming=F, weighting=weightTfIdf)
dtm.full = DocumentTermMatrix(docs, control=dtm.control)
dtm = removeSparseTerms(dtm.full,0.97)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- docs[idx]
dtm = dtm[idx,]
X = as.matrix( dtm )
dim(X)
#############################
r = 20
res = nmf(X, r)
W = res@fit@W
H = res@fit@H
#############################
approxi = W %*% H
diff = X - approxi
norm(diff, 'F')

library(wordcloud)
k = 4
wordcloud(names(H[k,]), H[k,], max.words=20)