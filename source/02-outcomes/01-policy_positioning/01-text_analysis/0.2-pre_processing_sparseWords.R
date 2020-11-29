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
dim(dtm_bigrams)## (41582; 2,339)

dtm_bigrams$dimnames["Docs"] <- list(data_all$id_incumbent)

## Create  Tfidf to make threshold
term.tfidf <- tapply(dtm_$v/row_sums(dtm_)[dtm_$i], dtm_$j, mean) *
  log(nDocs(dtm_) / col_sums(dtm_ > 0))

df_tfidf <- data.frame(terms = colnames(dtm_), term.tfidf = term.tfidf)

order.scores<-order(df_tfidf$term.tfidf,df_tfidf$terms, decreasing = TRUE)
df_tfidf$rank[order.scores] <- 1:nrow(df_tfidf)

ggplot(data = subset(df_tfidf, rank <= 5000), mapping = aes(x = rank,  y = term.tfidf)) + 
  geom_line()
