rm(list=ls())

## Output directory
setwd(dir = 'cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/')
#output <- "~/Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/1.1-topics_lda/"

list.packages <- c("readr",
                   "tidybayes",
                   "readxl",
                   "preText",
                   "plyr",
                   "dplyr",
                   "tidyr",
                   "stringr",
                   "xtable",
                   "ggplot2",
                   "RcppRoll",
                   "ggridges",
                   "rstan",
                   "shinystan",
                   "stm",
                   "quanteda",
                   "quanteda.textmodels",
                   "wordcloud",
                   #"austin", 
                   'readxl', 
                   'topicmodels', 
                   'tidytext', 
                   'tm', 
                   'gofastr', 
                   'slam')

lapply(list.packages,
       require,
       character.only = TRUE)

###################
# dtms for wordfish 
###################

## load data of topics in speeches
load('01-policy_positioning/01-text_analysis/1.1-topics_lda/speeches_topics.Rda')

input <- '01-policy_positioning/01-text_analysis/01-clean_data/disaaggregated_data/legislatura'
legislaturas <- c(60:64)
all_topics <- c('topic1', 'topic2', 'topic3', 'topic4', 'topic5', 'topic6', 'topic7', 'topic8', 'topic9', 'topic10')

dtm_topic <- function(dataframe, topic, legislatura){
  out <- '/Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/wordfish/02_dataprep_wordfish/dtms/'
  NLP_tokenizer <- function(x) {
    unlist(lapply(ngrams(words(x),n = 2:2), paste, collapse = " "), use.names = FALSE)
  }
  
  data_corpus = VCorpus(VectorSource(dataframe$speech)) 
  dtm  <- tm::DocumentTermMatrix(data_corpus, control = list(tokenize = NLP_tokenizer))
  term.tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
    log(nDocs(dtm) / col_sums(dtm > 0))
  
  thresh.tfidf <- quantile(term.tfidf, .3)
  dtm_sparse_tfidf<- filter_tf_idf(dtm, min = thresh.tfidf, verbose = FALSE)
  dtm_sparse_tfidf$dimnames["Docs"] <- list(dataframe$id)
  save(dtm_sparse_tfidf, file = paste0(out, 'dtm',legislatura, '_', topic, '.Rda'))
  
  } 

master_function <- function(legislatura){
  data_speech <- read_excel(paste0(input, legislatura, '_disaggregated.xlsx')) %>% 
    left_join(speeches, by = 'id_speech')  %>% 
    select(topic_name, id_speech, id, clean_speech) %>% 
    group_by(id, topic_name) %>%
    summarise(speech = paste0(clean_speech, collapse = ''))
  
  for (i in 1:10){
    data_topic <- data_speech %>%
      filter(topic_name == all_topics[i])
    dtm_topic(data_topic, all_topics[i], legislatura)
  }
}

for (i in 1:length(legislaturas)){
  master_function(legislaturas[i])
}
