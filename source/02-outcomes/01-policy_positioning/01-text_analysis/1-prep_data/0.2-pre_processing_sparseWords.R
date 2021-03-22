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
library('gofastr')

generate_dtms <- function(legislatura){
  file = paste0('legislatura', legislatura, '_speech.xlsx')
  data_all <- read_excel(file) %>%
    select(legislatura, id_incumbent, speech)
  
  data_corpus = VCorpus(VectorSource(data_all$speech)) 
  
  ######### Bigrams 
  NLP_tokenizer <- function(x) {
    unlist(lapply(ngrams(words(x),n = 2:2), paste, collapse = " "), use.names = FALSE)
  }
  dtm_  <- tm::DocumentTermMatrix(data_corpus, control = list(tokenize = NLP_tokenizer))
  dtm_bigrams <- removeSparseTerms(dtm_,.99) 
  dtm_bigrams$dimnames["Docs"] <- list(data_all$id_incumbent)
  save(dtm_bigrams, file = paste0('../../02-dtms/bigrams/dtm_sparse', legislatura, '.Rda'))
  
  term.tfidf <- tapply(dtm_$v/row_sums(dtm_)[dtm_$i], dtm_$j, mean) *
    log(nDocs(dtm_) / col_sums(dtm_ > 0))
  
  thresh.tfidf <- quantile(term.tfidf, .3)
  dtm_sparse_tfidf<- filter_tf_idf(dtm_, min =thresh.tfidf, verbose = FALSE)
  dtm_sparse_tfidf$dimnames["Docs"] <- list(data_all$id_incumbent)
  save(dtm_sparse_tfidf, file = paste0('../../02-dtms/bigrams/dtm_sparse', legislatura, '_tfidf.Rda'))
  
  ######### Unigrams
  NLP_tokenizer <- function(x) {
    unlist(lapply(ngrams(words(x),n = 1:2), paste, collapse = " "), use.names = FALSE)
  }
  dtm  <- tm::DocumentTermMatrix(data_corpus, control = list(tokenize = NLP_tokenizer))
  dtm_bu_Sparse <- removeSparseTerms(dtm,.99) ## (64; 106,505)
  dtm_bu_Sparse$dimnames["Docs"] <- list(data_all$id_incumbent)
  save(dtm_bu_Sparse, file = paste0('../../02-dtms/unigrams_bigrams/dtm_sparse', legislatura, '.Rda'))
  
  term.tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
    log(nDocs(dtm) / col_sums(dtm > 0))
  thresh.tfidf <- quantile(term.tfidf, .3)
  dtm_bu_sparse_tfidf<- filter_tf_idf(dtm, min =thresh.tfidf, verbose = FALSE)
  dtm_bu_sparse_tfidf$dimnames["Docs"] <- list(data_all$id_incumbent)
  save(dtm_bu_sparse_tfidf, file = paste0('../../02-dtms/unigrams_bigrams/dtm_sparse', legislatura, '_tfidf.Rda'))
}

legislaturas = c(60, 61, 62, 63, 64)

for (i in 1:length(legislaturas)){
  generate_dtms(legislaturas[i])
}

##########################
# Code for one Legislatura 
##########################

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
dim(dtm_bigrams)## (471; 23,430)
dtm_bigrams$dimnames["Docs"] <- list(data_all$id_incumbent)
save(dtm_bigrams, file = '../../02-dtms/bigrams/dtm_sparse60.Rda')

#### Remove Sparse terms with tf-idf 
term.tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
  log(nDocs(dtm) / col_sums(dtm > 0))

thresh.tfidf <- quantile(term.tfidf, .3)
dtm_sparse_tfidf<- filter_tf_idf(dtm_, min =thresh.tfidf, verbose = TRUE)
dim(dtm_sparse_tfidf) #  ### 471:475,324
dtm_sparse_tfidf$dimnames["Docs"] <- list(data_all$id_incumbent)
save(dtm_sparse_tfidf, file = '../../02-dtms/bigrams/dtm_sparse60_tfidf.Rda')

## Plot TF-IDF ranking 
term.tfidf <- tapply(dtm_$v/row_sums(dtm_)[dtm_$i], dtm_$j, mean) *
  log(nDocs(dtm_) / col_sums(dtm_ > 0))

df_tfidf <- data.frame(terms = colnames(dtm_), term.tfidf = term.tfidf)

order.scores<-order(df_tfidf$term.tfidf,df_tfidf$terms, decreasing = TRUE)
df_tfidf$rank[order.scores] <- 1:nrow(df_tfidf)

ggplot(data = df_tfidf, mapping = aes(x = rank,  y = term.tfidf)) + 
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
dtm_bu_Sparse$dimnames["Docs"] <- list(data_all$id_incumbent)
save(dtm_bu_Sparse, file = '../../02-dtms/unigrams_bigrams/dtm_sparse60.Rda')

#### Remove Sparse terms with tf-idf 
term.tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
  log(nDocs(dtm) / col_sums(dtm > 0))

thresh.tfidf <- quantile(term.tfidf, .3)
dtm_bu_sparse_tfidf<- filter_tf_idf(dtm, min =thresh.tfidf, verbose = TRUE)
dim(dtm_bu_sparse_tfidf) #  ### 471:475,324
dtm_bu_sparse_tfidf$dimnames["Docs"] <- list(data_all$id_incumbent)
save(dtm_bu_sparse_tfidf, file = '../../02-dtms/unigrams_bigrams//dtm_sparse60_tfidf.Rda')

## Plot TF-IDF ranking 
term.tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
  log(nDocs(dtm) / col_sums(dtm > 0))

df_tfidf <- data.frame(terms = colnames(dtm), term.tfidf = term.tfidf)

order.scores<-order(df_tfidf$term.tfidf,df_tfidf$terms, decreasing = TRUE)
df_tfidf$rank[order.scores] <- 1:nrow(df_tfidf)

ggplot(data = df_tfidf, mapping = aes(x = rank,  y = term.tfidf)) + 
  geom_line()
