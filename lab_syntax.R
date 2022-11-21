rm(list=ls())

### stemming
#install.packages("SnowballC")
require("SnowballC")
wordStem(c("win", "winning", "wins", "winner", "winners"))


require("NLP")
#install.packages("openNLP")
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
require("openNLP")

s <- "I ate the spaghetti with chopsticks. I ate the spaghetti with meatballs. John saw the saw and decided to take it. The duchess was entertaining last night."
s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
             "nonexecutive director Nov 29. ",
             "Mr. Vinken is chairman of Elsevier N.V., ",
             "the Dutch publishing group."),
           collapse = "")

## Sentence
sent_token_annotator <- Maxent_Sent_Token_Annotator()
a1 <- annotate(s, sent_token_annotator)
a1
annotate(s, Maxent_Sent_Token_Annotator(probs = TRUE))

class(s) # return the class attribute
s <- as.String(s)
class(s)
s[a1]


### Word
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, word_token_annotator, a1)
annotate(s, Maxent_Word_Token_Annotator(probs = TRUE), a1)

### Entity
entity_annotator <- Maxent_Entity_Annotator()
annotate(s, entity_annotator, a2)
entity_annotator(s, a2) ## Directly:
s[entity_annotator(s, a2)] ## And slice ...
annotate(s, Maxent_Entity_Annotator(probs = TRUE), a2) ## Variant with sentence probabilities as features.

### POS Tag
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
a3 <- annotate(s, pos_tag_annotator, a2)
a3
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, "[[", "POS")
table(tags)

### find all adjectives
mypos = a3w[tags=="JJ"]
s[mypos]

### Chunking needs word token annotations with POS tags.
aaa <- annotate(s, list(sent_token_annotator, word_token_annotator, pos_tag_annotator))
annotate(s, Maxent_Chunk_Annotator(), aaa)
annotate(s, Maxent_Chunk_Annotator(probs = FALSE), aaa)

parse_annotator <- Parse_Annotator()
p <- parse_annotator(s, a2)
ptexts <- sapply(p$features, `[[`, "parse")
ptexts
## Read into NLP Tree objects.
ptrees <- lapply(ptexts, Tree_parse)
ptrees