rm(list=ls())

## Output directory
setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/')
output <- "~//Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/wordfish/03_ideal_wordfish/"

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

## Load estimates
load('wordfish/2.1-model_estimates/all_estimates.Rda', verbose = TRUE)
load('wordfish/01_lda/speeches_20k.Rda', verbose = TRUE)

data_party <- all_estimates %>%
  select(id_legislador, inc_party) %>%
  distinct()

## Obtain Median member of each party
discipline_data = all_estimates %>%
  group_by(legislatura, inc_party, topic)  %>%
  mutate(median_party = median(estimated_theta), 
            mean_party = mean(estimated_theta))%>%
  ungroup()%>%
  mutate(party_discipline = estimated_theta - median_party)


save(discipline_data, file = '/Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/wordfish/5-estimates_party-discipline/wordfish_party_discipline.Rda')

## plot median of each party 

party_estimates <- discipline_data %>%
  select(inc_party, topic, median_party, mean_party) %>%
  distinct()

### Load data

load('/Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/wordfish/5-estimates_party-discipline/wordfish_party_discipline.Rda', verbose = TRUE)

detach(package:plyr)

agrgegated_discipline <- discipline_data %>%
  ungroup() %>%
  dplyr::group_by(id_legislador, inc_party, legislatura) %>%
  dplyr::summarize(mean_party_discipline = mean(party_discipline, na.rm = TRUE))


save(agrgegated_discipline, file = '/Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/wordfish/5-estimates_party-discipline/wordfish_party_discipline_aggregated.Rda')


### Load cosine similarity

cosine_data <- read_excel('cosine_similarity/party_agenda_leadership.xlsx') %>%
  select(id_legislador, cosine_similarity, party_leader)

left_join(agrgegated_discipline, cosine_data, by = 'id_legislador')



