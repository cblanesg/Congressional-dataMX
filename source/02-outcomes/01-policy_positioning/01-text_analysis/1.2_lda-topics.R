rm(list=ls())

## Output directory
setwd(dir = 'cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/')
output <- "~/Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/1.1-topics_lda/"

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
                   "austin", 
                   'readxl', 
                   'topicmodels', 
                   'tidytext')

lapply(list.packages,
       require,
       character.only = TRUE)

## load topics-speeches
data_topics_docs <- read_csv("/Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/01-clean_data/topics/topics_speeches.csv") %>%
  select(id_speech, clean_topic)

## tokenize data
toks <- tokens(x = data_topics_docs$clean_topic)
ngrams <- tokens_ngrams(toks, n = 1:2)
dfm_ob <- dfm(ngrams)

dfm_ob@docvars$docname_ <- data_topics_docs$id_speech
dfm_ob@docvars$docid_ <- data_topics_docs$id_speech
## Apply lda

ap_lda <- LDA(dfm_ob, k = 10, control = list(seed = 1234))

save(ap_lda, file = '01-policy_positioning/01-text_analysis/1.1-topics_lda/LDA_object.Rda')

ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() + 
  theme_minimal()

### Missing some wordcloud of terms in topics!

## document topic probabilities
ap_documents <- tidy(ap_lda, matrix = "gamma")

topics <- c('topic1', 'topic2','topic3', 'topic4', 'topic5', 
            'topic6', 'topic7','topic8', 'topic9', 'topic10')

df_docs <- as.data.frame(ap_documents)
df_docs$id_speech <- data_topics_docs$id_speech

speeches <- df_docs %>%
  group_by(document) %>%
  mutate(max_gamma = max(gamma)) %>%
  filter(gamma == max_gamma) %>%
  mutate(topic_name = topics[topic])

save(speeches, file = '01-policy_positioning/01-text_analysis/1.1-topics_lda/speeches_topics.Rda')

## recover labels speeches
data_topics_docs <- read_csv("/Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/01-clean_data/topics/topics_speeches.csv") %>%
  select(legislatura, id_speech ,topic_speech)
  
all_speeches <- left_join(speeches, data_topics_docs, 
          by = 'id_speech')

all_speeches %>% ## distribution of speeches in topics
  group_by(topic_name) %>%
  ggplot(aes(topic_name)) +
  geom_bar()


all_speeches %>% ## distribution of speeches by congress 
  group_by(topic_name) %>%
  ggplot(aes(topic_name)) +
  geom_bar() + facet_grid(.~legislatura) +
  theme_minimal() +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


## mapping speech to topic
all_speeches %>%
  select(id_speech, topic_name) %>%
  ungroup()


