


setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/')

list.packages <- c("readr",
                   "tidybayes",
                   "readxl",
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
                   "quanteda",
                   "quanteda.textmodels",
                   "austin", 
                   'miceadds', 
                   'corpustools')



lapply(list.packages,
       require,
       character.only = TRUE)

library(ggplot2)
library(tidyverse)
library(readxl)

load('wordfish/01_lda/speeches_20k.Rda', verbose = TRUE)

temp <- subset(docs_20k, topics20k == unique(docs_20k$topics20k)[[20]])
docs <- Corpus(VectorSource(temp$clean_speech))
dtm <- TermDocumentMatrix(docs)

m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

unique(docs_20k$topics20k)[[20]]
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2")) 


### Topics distributed across congress

docs_20k$topic_label

ggplot(docs_20k, aes(x = topic_label)) + 
  geom_histogram(stat="count") + 
  facet_grid(.~ legislatura) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(title = 'Topic Distribution across Congress', 
       subtitle = 'Each topic appears at least once in a Congress')
  







