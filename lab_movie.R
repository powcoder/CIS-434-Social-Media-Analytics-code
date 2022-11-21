rm(list=ls())
library(tm)
library(e1071)
library(maxent)

csvdata <- read.csv("~/data/movietweets.csv", header=TRUE, sep=',', quote='"')
y <- csvdata$sentiment

docs <- Corpus(VectorSource(csvdata$tweet))

mystop <- c("movie")
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"), mystop), 
                   stripWhitespace=T, stemming=T)
dtm.full <- DocumentTermMatrix(docs, control=dtm.control)


set.seed(1) # fixing the seed value for the random selection guarantees the same results in repeated runs
n=length(y)
n1=round(n*0.8)
n2=n-n1
train=sample(1:n,n1)

###########################################
#############   Evaluation   ##############
###########################################

Evaluation <- function(pred, true, class)
{
  
  tp <- sum( pred==class & true==class)
  fp <- sum( pred==class & true!=class)
  tn <- sum( pred!=class & true!=class)
  fn <- sum( pred!=class & true==class)
  precision <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  F1 <- 2/(1/precision + 1/recall)
  F1
}

###########################################
##########   Naive Bayesion   #############
###########################################
dtm <- removeSparseTerms(dtm.full,0.99)
X <- as.matrix(dtm)
Y <- as.factor(y)

nb.model <- naiveBayes(X[train,], Y[train])
pred <- predict(nb.model, X[-train,])
table(pred, Y[-train])
Evaluation(pred, Y[-train], 1)
Evaluation(pred, Y[-train], -1)


###########################################
##########   Maximum Entropy   ############
###########################################
dtm <- removeSparseTerms(dtm.full,0.996)
X <- as.matrix(dtm)
Y <- as.factor(y)

# if you can install the maxent package, then library(maxent) and  maxent.model = maxent( X[train,], Y[train] )
source('~/code/maxiumentropy.R')
maxent.model = maximumentropy( X[train,], Y[train] )
pred <- predict(maxent.model,X[-train,])
table(pred[,1], Y[-train])
Evaluation(pred[,1], Y[-train], 1)
Evaluation(pred[,1], Y[-train], -1)


###########################################
#######   Support Vector Machine   ########
###########################################

svm.model <- svm(Y[train] ~ ., data = X[train,], kernel='linear')
pred <- predict(svm.model, X[-train,])
table(pred, Y[-train])
Evaluation(pred, Y[-train], 1)
Evaluation(pred, Y[-train], -1)
