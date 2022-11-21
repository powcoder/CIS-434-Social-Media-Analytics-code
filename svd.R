###########################################
###########   Doing SVD in R   ############
###########################################

X = matrix(c(3,-1,1,3,1,1), 2,3)
X.svd <- svd(X)
D <- diag(X.svd$d)
U <- X.svd$u
V <- X.svd$v
U %*% D %*% t(V) #  X = U D V'


###########################################
#########   Illustrating Corpus   #########
###########################################

rm(list=ls())
library(tm)

sentence <- c("Human machine interface for ABC computer applications",
              "A survey of user opinion of computer system response time",
              "The EPS user interface management system",
              "System and human system engineering testing of EPS",
              "Relation of user perceived response time to error measurement",
              "The generation of random, binary, ordered trees",
              "The intersection graph of paths in trees",
              "Graph minors IV: Width of trees and well-quasi-ordering",
              "Graph minors: A survey")
docs <- Corpus(VectorSource(sentence))

###########################################
#####   SVD on TermDocumentMatrix   #######
###########################################

tdm.full <- TermDocumentMatrix(docs, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, stopwords=stopwords("english")))
tdm = removeSparseTerms(tdm.full,0.8)
X = as.matrix(tdm)
X.svd = svd(X)
D = diag(X.svd$d)
U <- X.svd$u
V <- X.svd$v
X
round( U %*% D %*% t(V) )
round(D, digits=2)


###########################################
#####   2-dimensional Reconstruction  #####
###########################################
dim=2
Uk = U[,seq(1,dim)]
Dk = D[seq(1,dim),seq(1,dim)]
Vk = V[,seq(1,dim)]
rownames(Uk) = rownames(X)
rownames(Vk) = colnames(X)

R = Uk %*% Dk %*% t(Vk) 

### relations between terms and documents
round(R, digits=2)

### relations between terms
cor(X['human',], X['user',])
cor(R['human',], R['user',])
cor(X['human',], X['minors',])
cor(R['human',], R['minors',])

### relations between documents
cor(X)
cor(R)

### projection on semantic space
term.proj = Uk %*% Dk
doc.proj = Dk %*% t(Vk)

require(ggplot2)
term.plot <- data.frame(x=term.proj[,1], y=term.proj[,2], names=rownames(term.proj))
ggplot(term.plot, aes(x,y)) + geom_point() + geom_text(aes(label=names), hjust=0.5, vjust=-1.5)

colnames(doc.proj) = c('c1', 'c2', 'c3', 'c4', 'c5', 'm1', 'm2', 'm3', 'm4')
doc.plot <- data.frame(x=doc.proj[1,], y=doc.proj[2,], names=colnames(doc.proj))
ggplot(doc.plot, aes(x,y)) + geom_point() + geom_text(aes(label=names), hjust=0.5, vjust=-1)