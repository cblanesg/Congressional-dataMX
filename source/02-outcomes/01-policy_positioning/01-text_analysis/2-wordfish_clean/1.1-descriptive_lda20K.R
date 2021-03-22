rm(list=ls())
## 
setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/')

library(ggplot2)
library(tidyverse)
library(readxl)

docs_lda <- read_excel('wordfish/01_lda/clean_speaches_topics.xlsx')

## add title speech

dataFiles <- lapply(Sys.glob("01-clean_data/disaaggregated_data/*xlsx"), read_excel)
topics_assigned <- rbind(select(dataFiles[[1]], c(id_speech,topic_speech, legislatura, id)), 
                        select(dataFiles[[2]], c(id_speech,topic_speech, legislatura, id)), 
                        select(dataFiles[[3]], c(id_speech,topic_speech, legislatura, id)), 
                        select(dataFiles[[4]], c(id_speech,topic_speech, legislatura, id)), 
                        select(dataFiles[[5]], c(id_speech,topic_speech, legislatura, id))) %>%
  rename('topic_original' = 'topic_speech', 
         'id_legislador' = 'id')

docs_lda <- docs_lda %>%
  left_join(topics_assigned, by = 'id_speech') %>%
  select('legislatura',
         'id_legislador',
         'id_speech', 
         'sparse_text' = 'data_clean', 
         'clean_speech', 
         'topic_original',
         'topics20k', 
         'percentage_topics20k', 
         'topics15k', 
         'percentage_topics15k')

## check percentage of salient topics

ggplot(data = docs_lda, aes(x = percentage_topics20k)) +
  geom_density()


## check distribution of 10 top topics 

topics_top10 <- docs_lda %>%
  group_by(topics20k) %>%
  count() %>%
  ungroup() %>%
  rename('counts' = 'n') %>%
  top_n(10)

docs_top10 <-  docs_lda %>%
  filter(topics20k %in% topics_top10$topics20k)

## Check documents for main topics -> Assign Labels!

### 20K
topic_labels20k <- c('1' = 'transportes, ecologia',
                     '2' = 'genero', 
                     '3' = 'derechos politicos', 
                     '4' = 'salud', 
                     '5' = 'vivienda', 
                     '6' = 'topic_6',
                     '7' = 'comercio', 
                     '8' = 'fuerzas armadas', 
                     '9' = 'salud', 
                     '10' = 'derechos nacionales', 
                     '11' = 'topic_11',
                     '12' = 'laboral', 
                     '13' = 'ciencia y tecnologia', 
                     '14' = 'human rights', 
                     '15' = 'energia', 
                     '16' = 'fiscal, energia', 
                     '17' = 'inseguridad', 
                     '18' = 'educacion', 
                     '19' = 'comunicacion, responsabilidad administrativa', 
                     '20' = 'consitutcional')

topic6 <- docs_lda %>%
  filter(topics20k ==9)

docs_20k <- docs_lda %>%
  select(-percentage_topics15k, 
         -topics15k) %>%
  mutate(topic_label = topic_labels20k[topics20k])

topics_top10 <- docs_20k %>%
  group_by(topic_label) %>%
  count() %>%
  ungroup() %>%
  rename('counts' = 'n') %>%
  top_n(10)


docs_20k <- docs_20k %>%
  mutate(top10_topics = ifelse(topic_label %in% topics_top10$topic_label, 1, 0))

## remaining legislators
length(unique(docs_top10$id_legislador))/length(unique(docs_20k$id_legislador))*100

save(docs_20k, file = 'wordfish/01_lda/speeches_20k.Rda')

#### More descriptives

clean_speeches <- read_excel('wordfish/01_lda/clean_speaches_topics.xlsx')

library(stm)
library(tidytext)

tidy_docs <- clean_speeches %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, data_clean) 

speeches_dfm <- tidy_docs %>%
  count(id_speech, word, sort = TRUE) %>%
  cast_dfm(id_speech, word, n)

topic_model <- stm(speeches_dfm, K = 20, 
                   verbose = FALSE, init.type = "Spectral")



td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")




fit0 <- stm(docs_20k$clean_speech, # the documents
            out$vocab, # the words
            K = 10, # 10 topics
            max.em.its = 75, # set to run for a maximum of 75 EM iterations
            data = out$meta, # all the variables (we're not actually including any predictors in this model, though)
            init.type = "Spectral")


plot.STM() 





