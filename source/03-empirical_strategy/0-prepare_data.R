setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/')


library(tidyverse)
library(reshape2)
library(readxl)

data_attendance <- read_csv('02-outcomes/02-legislative_effort/attendance_data.csv') %>%
  select(legislatura, id, nombre_completo, tipo_legislador, partido, reelection_dummy, ssd_legis, pct_attendance) 

bills <- read_csv('02-outcomes/02-legislative_effort/bills_data.csv') %>%
  select(id, number_bills) 

committees <- read_csv('02-outcomes/02-legislative_effort/') %>%
  select(id_legislador, 'number_committees' = 'counter')


effort <- read_csv('02-outcomes/02-legislative_effort/effort_data.csv') %>%
  select(legislatura, id_legislador, nombre_completo, tipo_legislador, partido, reelection_dummy, ssd_legis, 
         pct_attendance, number_committees, count_speech, number_bills) 

matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  for(j in 1:ncol(x)){
    x[,j] <- (x[,j] - mean(x[sgroup,j]))/sd(x[sgroup,j])
  }
  return(x)
}

# Function that takes in data in matrix format and returns
# (i) IC weights and (ii) ICW index.
# Weights can be incorporated using the "wgts" argument.
# The "revcols" argument takes a vector indicating which columns,
# if any, should have values reversed (that is, standardized 
# values are multiplied by -1) prior to construction of the index. 

icwIndex <- function(	xmat,
                      wgts=rep(1, nrow(xmat)),
                      revcols = NULL,
                      sgroup = rep(TRUE, nrow(xmat))){
  X <- matStand(xmat, sgroup)
  if(length(revcols)>0){
    X[,revcols] <-  -1*X[,revcols]
  }
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  Sx <- cov.wt(X, wt=wgts)[[1]]
  weights <- solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)
  index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(X))
  return(list(weights = weights, index = index))
}

effort_matrix <- effort %>%
  select(pct_attendance, number_committees, count_speech, number_bills) %>%
  as.matrix()

icwX_matrix <- icwIndex(effort_matrix)
z_effort <- icwX_matrix$index

effort$index_effort <- z_effort

demographics <- read_excel('01-collection_data/09-controls_reg/controls_reg.xlsx') %>%
  select(id_legislador, gender, grado_escolaridad, exp_pol, exp_sec_privado, admon_publica,asoc_civil, docencia, carrera_deportiva)

effort <- left_join(effort, demographics, by = 'id_legislador')

effort <- effort %>%
  mutate(female = ifelse(gender == 'female', 1, 0))



save(effort, file = '03-empirical_strategy/1-input_reg/effort_reg.Rda')
load('03-empirical_strategy/1-input_reg/effort_reg.Rda')

effort64 <- effort %>%
  filter(legislatura == 64)


### include discipline

discipline <- load('02-outcomes/01-policy_positioning/00-roll_call/03-party_discipline/party_discipline_rollCall.Rda', verbose = TRUE)
load('02-outcomes/01-policy_positioning/01-text_analysis/wordfish/5-estimates_party-discipline/wordfish_party_discipline_aggregated.Rda', verbose = TRUE)


voting_behavior = df_ideal_points %>%
  select('discipline_vote' = 'abs_dif_with_party', 'discipline_vote_no_absolute' = 'dif_with_party', 'id_legislador' = 'id_inc', party_dwnom, 'legislador_dwnom' = 'coord1D')

text <- agrgegated_discipline %>%
  ungroup() %>%
  select(id_legislador, 'discipline_speech' = 'mean_party_discipline')%>% 
  distinct() %>% 
  group_by(id_legislador) %>% 
  summarise(discipline_speech_no_absolute = mean(discipline_speech)) %>% 
  ungroup() %>% 
  mutate(discipline_speech = abs(discipline_speech_no_absolute))

data_reg <- left_join(effort, 
                      voting_behavior, 
                      by = 'id_legislador') %>%
  left_join(text, by = 'id_legislador') 
  

data_reg %>%
  group_by(id_legislador)%>% 
  count() %>%
  filter(n > 1)


save(data_reg, file = '03-empirical_strategy/1-input_reg/data_reg.Rda')


#### Data type of PR 
rm(list=ls()) ## clean enviornment

load('03-empirical_strategy/1-input_reg/data_reg.Rda', verbose = TRUE)

ranking_data <- read_excel('01-collection_data/08-ranking_party-list/3-data_ids/ranking_type_rp.xlsx') %>%
  select(id_legislador, type_pr)

data_temp <- left_join(data_reg, ranking_data, by = 'id_legislador')

`%notin%` <- negate(`%in%`)
data_reg_type <- data_temp %>%
  mutate(type_legislador = ifelse(ssd_legis == 1, 'mayoria_relativa', 
                                  ifelse(type_pr == 1, 'top_pr', 'low_pr'))) %>%
  mutate(partido = tolower(partido), 
         main_parties_raw = ifelse(partido == 'morenal', 'morena', 
                               ifelse(partido == 'partido verde ecologista', 'pvem', 
                                      ifelse(partido == 'movimiento ciudadano', 'mc', 
                                             ifelse(partido == "encuentro social", 'es', 
                                                    ifelse(is.na(partido), 'sp',
                                                           ifelse(partido == 'sin partido', 'sp', 
                                                                  ifelse(partido == 'pve', 'pvem', 
                                                                         ifelse(partido == 'alianza', 'pna', partido)))))))), 
         main_parties_raw = ifelse(id_legislador == '29eb5122-fd10-11ea-83d8-acde48001122', 'morena', main_parties_raw), 
         main_parties_raw = ifelse(is.na(main_parties_raw), 'pan', main_parties_raw), 
         main_parties = ifelse(main_parties_raw %notin% c('pan', 'prd', 'pri', 'morena', 'pvem', 'mc'), 'small parties', main_parties_raw))
  


save(data_reg_type, file = '03-empirical_strategy/1-input_reg/data_reg_type.Rda')


#### Inlcude type of MR
rm(list=ls()) ## clean enviornment

load('03-empirical_strategy/1-input_reg/data_reg.Rda', verbose = TRUE)

elec_results <- read_excel('01-collection_data/10-elec_results/3-id_data/elec_results_id.xlsx')%>%
  select(id_legislador, type_election)

data_reg <- data_reg %>%
  left_join(elec_results, by = 'id_legislador')

data_reg <- data_reg %>%
  mutate(type_election = ifelse(is.na(type_election), "Representacion proporcional", type_election)) %>%
  rename('type_ssd' = 'type_election')


data_reg %>%
  group_by(type_ssd)%>%
  count()

save(data_reg, file = '03-empirical_strategy/1-input_reg/data_reg.Rda')


### 


