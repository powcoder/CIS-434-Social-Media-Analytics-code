rm(list=ls())
library(tm)
library(DBI)
library(RMySQL)
library(e1071)
library(pROC)

driver <- dbDriver("MySQL")
myhost <- "localhost"
mydb <- "studb"
myacct <- "cis434"
mypwd <- "LLhtFPbdwiJans8F@S207" 

mytable <- "obgyn"
conn	<- dbConnect(driver, host=myhost, dbname=mydb, myacct, mypwd)
mydata	<- dbGetQuery(conn, paste("SELECT id, staff, punctual, helpful, knowledge, text FROM ", mytable))
dbDisconnect(conn)

rating <- rowMeans(cbind(mydata$staff, mydata$punctual, mydata$helpful, mydata$knowledge))

###########################################
#######  Rule-based Classification  #######
###########################################

pos <- rep(0, nrow(mydata))
neg <- rep(0, nrow(mydata))
pos <- grepl("great|good", mydata$text, ignore.case=TRUE,  perl=TRUE)
neg <- grepl("bad|poor", mydata$text, ignore.case=TRUE, perl=TRUE)
senti <- pos - neg

tp <- sum( senti>=0 & (rating>=4) )
fp <- sum( senti>=0 & (rating<4) )
tn <- sum( senti<0 & (rating<4) )
fn <- sum( senti<0 & (rating>=4) )
precision <- tp/(tp+fp)
recall <- tp/(tp+fn)
2/(1/precision + 1/recall) # F1 score


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
#######  Supervised Classification  #######
###########################################

Y = as.numeric( rating >= 4)

docs <- Corpus(VectorSource(mydata$text))
mystopwords <- c("dr", "doctor", "patient")
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"), mystopwords), stripWhitespace=T)
dtm.full <- DocumentTermMatrix(docs, control=dtm.control)
dtm <- removeSparseTerms(dtm.full,0.99)
X <- as.matrix(dtm)

set.seed(1) # fixing the seed value for the random selection guarantees the same results in repeated runs
n=length(Y)
n1=round(n*0.75)
n2=n-n1
train=sample(1:n,n1)

###########################################
##########   Naive Bayesion   #############
###########################################

nb.model <- naiveBayes( X[train,], factor( Y[train]) ) # encode the response as a factor variable
pred.class <- predict( nb.model, X[-train,] )
table( pred.class, Y[-train] )
Evaluation( pred.class, Y[-train], 1 )

pred <- predict( nb.model, X[-train,], type = "raw" )
nb.roc <- roc( Y[-train], pred[,2] )
plot.roc( nb.roc )
auc( Y[-train], pred[,2] )

###########################################
##########   Maximum Entropy   ############
###########################################

# if you can install the maxent package, then library(maxent) and  maxent.model = maxent( X[train,], Y[train] )
source('~/code/maxiumentropy.R')
maxent.model = maximumentropy( X[train,], Y[train] )

pred <- predict( maxent.model, X[-train,] )
as.numeric(pred[,2]) + as.numeric(pred[,3]) # the 2nd and 3rd columns of pred are probabilities
table( pred[,1], Y[-train] )
Evaluation( pred[,1], Y[-train], 1 )

maxent.roc <- roc( Y[-train], as.numeric(pred[,2]) )
plot.roc( maxent.roc )
auc( Y[-train], as.numeric(pred[,2]) )


###########################################
#######   Support Vector Machine   ########
###########################################

svm.model <- svm(Y[train] ~ ., data = X[train,], kernel='linear')
pred <- predict( svm.model, X[-train,] )
pred.class <- as.numeric( pred>0.7 ) # try varying the threshold distance
table(pred.class, Y[-train])
Evaluation( pred.class, Y[-train], 1 )

svm.roc <- roc( Y[-train], pred )
plot.roc( svm.roc )
auc( Y[-train], pred )