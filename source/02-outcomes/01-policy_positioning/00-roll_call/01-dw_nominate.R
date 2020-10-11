

library(tidyverse)
library(stargazer)
library(tidyr)
library(wnominate)

setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/01-collection_data/')

out <- '~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/00-roll_call/'

#######################
# 0.0 Load Data- Id data
#######################

id_data <- read_csv('06-aggregated-data/id_data.csv') %>%
  select(-c(X1))
colnames(id_data)

id_data_prop <- id_data %>%
  filter(suplente_propietario == 'P')

nrow(id_data)

###############################
# 0.1 Load Data- Roll Call Votes
###############################

votes <- read_csv('06-aggregated-data/roll_call_votes.csv') %>%
  select(-c(X1)) %>%
  mutate(voto_num = ifelse(voto == 'A favor', 'yea', 
                           ifelse(voto == 'En contra', 'nay', 'other')))


votes <- left_join(id_data_prop, votes, by = c('id_legislador' = 'id', 'legislatura')) %>%
  select(legislatura, id_legislador, titulo_votacion,voto_num, party) %>%
  filter(!is.na(titulo_votacion)) 


#############################
# 1.0 Rollcall() object
#############################



votes60 <- votes %>%
  filter(legislatura == 60) %>%
  mutate(code_vote = ifelse(voto_num == 'yea', 1, 
                            ifelse(voto_num == 'nay', 6, 9))) %>%
  group_by(id_legislador) %>%
  count() %>%
  filter(n == 590 )
  select(id_legislador, party, code_vote) %>%
  spread(id_legislador, code_vote)

length(unique(votes60$titulo_votacion))

