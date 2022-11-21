rm(list=ls())
library(rword2vec)

######################################################################################
########################           Train Word Vectors      ###########################
######################################################################################


### This is computationally very intensive and we need large stack size.
### Hence, we run it in the terminal, instead of in RStudio.
### In Linux, increase the stack size using this: ulimit -s 1024000

### model = word2vec( train_file = "text8", output_file = "vec.bin", binary=1 )
### bin_to_txt( "vec.bin", "vector.txt" )


######################################################################################
########################           Use Word Vectors         ##########################
######################################################################################

data = as.data.frame(read.table("~/vector.txt", skip=1))
vectors = as.matrix( data[,-1] )
rownames(vectors) = data[,1]

v_paris = vectors["paris", ]
v_france = vectors["france", ]
v_german = vectors["german", ]
v_berlin = vectors["berlin", ]
v_munich = vectors["munich", ]
v_china = vectors["china", ]
v_beijing = vectors["beijing", ]
v_tokyo = vectors["tokyo", ]

cosim = function(x,y) sum(x*y)/(norm(matrix(x,1),'f')*norm(matrix(y,1),'f'))

cosim( v_paris-v_france+v_german, v_berlin )
cosim( v_paris-v_france+v_german, v_munich )

cosim( v_paris-v_france+v_china, v_berlin )
cosim( v_paris-v_france+v_china, v_beijing )

cosim( v_paris-v_france+v_china, v_berlin )
cosim( v_paris-v_france+v_china, v_tokyo )

v_king = vectors["king", ]
v_prince = vectors["prince", ]
cosim( v_king, v_prince )

distance( file_name = "vec.bin", search_word = "king", num = 5 )
distance( file_name = "vec.bin", search_word = "princess", num = 5 )
distance( file_name = "vec.bin", search_word = "terrible", num = 5 )

word_analogy( file_name = "vec.bin", search_words = "king queen man",num = 8 )
word_analogy( file_name = "vec.bin", search_words = "paris france berlin",num = 8 )

######################################################################################
X = vectors[ rowSums( 0+is.na(vectors) )==0, ]
X.svd = svd( X )
D = diag(X.svd$d)
U <- X.svd$u
V <- X.svd$v

dim=2
Uk = U[,seq(1,dim)]
Dk = D[seq(1,dim),seq(1,dim)]
Vk = V[,seq(1,dim)]

term.proj = Uk %*% Dk
rownames(term.proj) = rownames(X)

require(ggplot2)
mywrds = c("germany","berlin","france","paris","china","beijing","japan","tokyo")
term.plot <- data.frame(x=term.proj[mywrds,1], y=term.proj[mywrds,2], names=mywrds)
ggplot(term.plot, aes(x,y)) + geom_point() + geom_text(aes(label=names))