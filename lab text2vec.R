rm(list=ls())

library(text2vec)

mytxt1 = "I like this movie. Do you like this movie?"
mytxt2 = "Watching the movie I like!"
mytxt = c(mytxt1, mytxt2)

# create an iterator over tokens with the itoken() function. Functions prefixed with create_ work with these iterators.
it = itoken(mytxt, preprocessor=tolower, tokenizer=word_tokenizer) # note the default tokenizer is space_tokenizer

vocab <- create_vocabulary(it)
vocab
vocab2 <- prune_vocabulary(vocab, term_count_min = 2L)# 1. the term least than 2 words. 2. help to reduce the memory requirement 
vocab2
# vocab_vectorizer() creates an object defining how to transform list of tokens into vector space - i.e. how to map words to indices
vectorizer <- vocab_vectorizer(vocab) 

# Set context window size to 5(左右两边 +/- 5 个 size). The suffix L indicates integer.
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
tcm
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L,
                  weights = 1/seq(2,6) )
tcm
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L,
                  weights = c(1,0.2,0.2,0.1,0.1) )
tcm

dtm <- create_dtm(it, vectorizer)
dtm

bigram_vocab = create_vocabulary(it, ngram = c(1L, 2L))
bigram_vectorizer = vocab_vectorizer(bigram_vocab)

dtm_bigram = create_dtm(it, bigram_vectorizer)
dtm_bigram[,1:9]
