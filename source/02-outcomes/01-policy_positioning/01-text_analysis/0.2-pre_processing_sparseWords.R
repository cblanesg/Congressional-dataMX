setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/00-prep_data/aggregated_legislators/')

library('tidyverse')
library("XML")
library("tm")
library("plyr")
library("gdata")
library("stringr")
library("RWeka")
library("slam")
library('readxl')

data_all <- read_excel('legislatura60_speech.xlsx') %>%
  select(legislatura, id_incumbent, speech)

data_corpus = VCorpus(VectorSource(data_all$speech)) 

#######################
# Bigrams 
#######################

NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x),n = 2:2), paste, collapse = " "), use.names = FALSE)
}

dtm_  <- tm::DocumentTermMatrix(data_corpus, control = list(tokenize = NLP_tokenizer))
dim(dtm_)   ## (41,582:3,621,744)

dtm_bigrams <- removeSparseTerms(dtm_,.99) 
dim(dtm_bigrams)## (471; 23430)

dtm_bigrams$dimnames["Docs"] <- list(data_all$id_incumbent)

## Create  Tfidf to make threshold
term.tfidf <- tapply(dtm_$v/row_sums(dtm_)[dtm_$i], dtm_$j, mean) *
  log(nDocs(dtm_) / col_sums(dtm_ > 0))

df_tfidf <- data.frame(terms = colnames(dtm_), term.tfidf = term.tfidf)

order.scores<-order(df_tfidf$term.tfidf,df_tfidf$terms, decreasing = TRUE)
df_tfidf$rank[order.scores] <- 1:nrow(df_tfidf)

ggplot(data = subset(df_tfidf, rank <= 25000), mapping = aes(x = rank,  y = term.tfidf)) + 
  geom_line()


#######################
# Bigrams and unigrams
#######################

NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x),n = 1:2), paste, collapse = " "), use.names = FALSE)
}

dtm  <- tm::DocumentTermMatrix(data_corpus, control = list(tokenize = NLP_tokenizer))
dim(dtm) ## (64; 1,941,930)
dtm_bu_Sparse <- removeSparseTerms(dtm,.99) ## (64; 106,505)
dim(dtm_bu_Sparse)

#save(dtm_bu_Sparse, file = './policy-estimations/02-clean_input/bigrams_unigrams/dtm_sparse.Rda')

term.tfidf2 <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
  log(nDocs(dtm) / col_sums(dtm > 0))

df_tfidf2 <- data.frame(terms = colnames(dtm), term.tfidf = term.tfidf2)

order.scores<-order(df_tfidf2$term.tfidf,df_tfidf2$terms, decreasing = TRUE)
df_tfidf2$rank[order.scores] <- 1:nrow(df_tfidf2)
df_tfidf2

ggplot(data = subset(df_tfidf2, rank <= 50000), mapping = aes(x = rank,  y = term.tfidf)) + 
  geom_line()


## Create  Tfidf to make threshold
term.tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
  log(nDocs(dtm) / col_sums(dtm > 0))

thresh.tfidf <- quantile(term.tfidf, .3)

term.tf <- data.frame(terms = colnames(dtm), term.tfidf = term.tfidf)

term.tf <- term.tf[order(term.tf$term.tfidf, decreasing = TRUE),]

thresh.term <- term.tf$terms[term.tf$term.tfidf > thresh.tfidf]

dtm2 <- as.matrix(dtm)
dtm.obj <- dtm2[, (colnames(dtm2) %in% thresh.term)]  ## threshold depends on data
colfreq <- apply(dtm.obj, 2, max, na.rm = TRUE)
colmax <- names(colfreq[colfreq >= 2])
dtm_unigrams2 <- as.data.frame(dtm.obj[,colnames(dtm.obj) %in% colmax])


