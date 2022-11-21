rm( list=ls() )
library( nnet )
library( neuralnet )

###########################################
############   XOR Function   #############
###########################################

AND    <- c(0,0,0,1)
OR     <-  c(0,1,1,1)
XOR    <- c(0,1,1,0)
mydata <- data.frame( matrix(c(0,0,1,1,0,1,0,1), 4,2), AND, OR, XOR )
X      <- mydata[,1:2]

### nnet ###

Y        = mydata$AND
nn.model = nnet(X, Y, skip=TRUE, size=0)  # size: number of hidden units
nn.model$wts
nn.model$fitted.values
nn.model$value

predict( nn.model, X ) # predict new examples with trained neural net


Y = mydata$XOR
nn.model = nnet( X, Y, skip=TRUE, size=0 )
nn.model$fitted.values

nn.model = nnet(X, Y, size=2) # use two units in the hidden layer
nn.model$wts
nn.model$fitted.values
nn.model$value

### neuralnet ###

net <- neuralnet( AND~X1+X2, mydata, hidden=c(3,2), rep=1, act.fct='logistic')
plot(net, rep="best")
compute(net, mydata[,1:2])


###########################################
##############   Iris Data   ##############
###########################################

train = sample(1:nrow(iris), 50)
head(iris[train,])

### nnet ### 

X = iris[,1:4]
Y = iris[,5]

nn.model = nnet( X[train,], class.ind(Y[train]), size=10 ) # decay=0.02

pred = predict( nn.model, X[-train,] )
pred = apply( pred, 1, which.is.max )
table( c('setosa', 'versicolor', 'virginica')[pred],  Y[-train] )

### neuralnet ###

iris[,6:8] <- cbind( iris$Species == 'setosa', iris$Species == 'versicolor', iris$Species == 'virginica')

names(iris)[6:8] <- c('setosa','versicolor','virginica')
nn <- neuralnet( setosa+versicolor+virginica ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris[train,], hidden=c(10), rep=10 )

pred <- compute( nn, iris[-train,1:4] )$net.result
pred <- apply( pred, 1, which.is.max )

table( c('setosa', 'versicolor', 'virginica')[pred], iris[-train,]$Species )

plot( nn, rep="best" )