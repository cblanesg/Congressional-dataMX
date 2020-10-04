
library(tidyverse)
library(stargazer)
library(tidyr)
library(broom)

setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/01-collection_data/')

## 1. Difference in means Pre-Post Reform

###############
# 0.1 Id Data
###############

id_data <- read_csv('05-aggregated-data/id_data.csv') %>%
  select(-c(X1))
colnames(id_data)

id_data_prop <- id_data %>%
  filter(suplente_propietario == 'P')

nrow(id_data)

###############
# 0.2 Committes
###############

committe <- read_csv('05-aggregated-data/committee_data.csv') %>%
  select(-c(X1))

summary_committe <- left_join(id_data_prop, committe, by = c('id_legislador', 'legislatura')) %>%
  select(legislatura, id_legislador, comision) %>%
  mutate(counter = ifelse(is.na(comision), 0, 1)) %>%
  select(legislatura, id_legislador, counter) %>%
  group_by(legislatura, id_legislador) %>%
  summarise(
    n = sum(counter)
  ) %>%
  mutate(reform = ifelse(legislatura < 64, 'pre', 'post'))

x <- summary_committe %>%
  filter(reform == 'post')

y <- summary_committe %>%
  filter(reform == 'pre')

result = t.test(x$n, y$n, alternative="two.sided")
tidy(result)
result$stderr

###############
# 3 Floor Speeches
###############
speech <- read_csv('05-aggregated-data/counts_speech.csv') %>%
  select(-c(X1))

summary_speech <- speech %>%
  mutate(reform = ifelse(legislatura < 64, 'pre', 'post'))

x <- summary_speech %>%
  filter(reform == 'post')

y <- summary_speech %>%
  filter(reform == 'pre')

result = t.test(x$counter, y$counter, alternative="two.sided")
tidy(result)
result$stderr

###############
# 2 Floor Attendance
###############


attendance <- read_csv('05-aggregated-data/attendance_data.csv') %>%
  select(-c(X1))

summary_attendance <- left_join(id_data_prop, attendance, by = c('id_legislador' = 'id', 'legislatura')) %>%
  select(legislatura, id_legislador, type_attendance) %>%
  mutate(attendance_num = ifelse(type_attendance == 'asistencia', 1, 0)) %>%
  group_by(legislatura, id_legislador) %>%
  mutate(n = n())%>%
  group_by(legislatura, id_legislador) %>%
  mutate(n_attendance = sum(attendance_num)) %>%
  select(legislatura, id_legislador, n, n_attendance) %>%
  distinct() %>%
  mutate(pct_attendance = n_attendance/n) %>%
  mutate(reform = ifelse(legislatura < 64, 'pre', 'post')) 



sd(summary_attendance$n)
summary(summary_attendance)

x <- summary_attendance %>%
  filter(reform == 'post')

y <- summary_attendance %>%
  filter(reform == 'pre')

result = t.test(x$pct_attendance, y$pct_attendance, alternative="two.sided")
tidy(result)
result$stderr

###############
# 3 Roll Call Votes
###############


votes <- read_csv('05-aggregated-data/roll_call_votes.csv') %>%
  select(-c(X1)) %>%
  mutate(voto_num = ifelse(voto == 'A favor', 'yes', 
                           ifelse(voto == 'En Contra', 'no', 'other')))

party_votes <- votes %>% 
  select(legislatura, titulo_votacion, party, voto_num) %>%
  group_by(legislatura, titulo_votacion, party, voto_num) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(legislatura, titulo_votacion, party) %>%
  mutate(max = max(n), 
         dummy_max = ifelse(n == max, 1, 0)) %>%
  filter(dummy_max == 1) %>%
  select(legislatura, titulo_votacion, party,'voto_party'  = voto_num)


votes <- left_join(id_data_prop, votes, by = c('id_legislador' = 'id', 'legislatura')) %>%
  select(legislatura, id_legislador, titulo_votacion,voto_num, party) %>%
  filter(!is.na(titulo_votacion)) %>%
  left_join(party_votes, by = c('legislatura', 'titulo_votacion', 'party'))%>%
  mutate(vote_with_party = ifelse(voto_num  == voto_party, 1, 0)) %>%
  group_by(legislatura, id_legislador) %>%
  mutate(n = n(), 
         num_vote_with_party = sum(vote_with_party)) %>%
  select(legislatura, id_legislador, num_vote_with_party, n) %>%
  distinct() %>%
  mutate(pct_vote_with_party = num_vote_with_party/n) %>%
  mutate(reform = ifelse(legislatura < 64, 'pre', 'post')) 

x <- votes %>%
  filter(reform == 'pre')

y <- votes %>%
  filter(reform == 'post')

result = t.test(x$pct_vote_with_party, y$pct_vote_with_party, alternative="two.sided")
result$stderr



summary_votes <- left_join(id_data_prop, votes, by = c('id_legislador' = 'id', 'legislatura')) %>%
  select(legislatura, id_legislador, voto) %>%
  group_by(legislatura, id_legislador) %>%
  summarise(
    n = n()
  )


sd(summary_votes$n)
summary(summary_votes)



###############
# 3 Legislation
###############

bills <- read_csv('05-aggregated-data/bills_proposed.csv') %>%
  select(-c(X1))

summary_bills <- left_join(id_data_prop, bills, by = c('id_legislador' = 'id', 'legislatura')) %>%
  select(legislatura, id_legislador) %>%
  group_by(legislatura, id_legislador) %>%
  summarise(
    n = n()
  ) %>%
  mutate(reform = ifelse(legislatura < 64, 'pre', 'post')) 

x <- summary_bills %>%
  filter(reform == 'post')

y <- summary_bills %>%
  filter(reform == 'pre')

result = t.test(x$n, y$n, alternative="two.sided")
result$stderr
tidy(result)

sd(summary_bills$n)
summary(summary_bills)



## 2. Difference in means Type of Legislator

###############
# 0.2 Committes
###############

committe <- read_csv('05-aggregated-data/committee_data.csv') %>%
  select(-c(X1))

summary_committe <- left_join(id_data_prop, committe, by = c('id_legislador', 'legislatura')) %>%
  select(legislatura, tipo_legislador, id_legislador, comision) %>%
  mutate(counter = ifelse(is.na(comision), 0, 1), 
         reform = ifelse(legislatura < 64, 'pre', 'post')) %>%
  select(reform, tipo_legislador, id_legislador, counter) %>%
  group_by(reform, tipo_legislador, id_legislador) %>%
  summarise(
    n = sum(counter)
  ) 
unique(summary_committe$tipo_legislador)

x_pre <- summary_committe %>%
  filter(tipo_legislador == 'Representacion proporcional' & reform == 'pre')

y_pre <- summary_committe %>%
  filter(tipo_legislador == 'Mayoria Relativa' & reform == 'pre')

result = t.test(x_pre$n, y_pre$n, alternative="two.sided")
tidy(result)
result$stderr


x_post <- summary_committe %>%
  filter(tipo_legislador == 'Representacion proporcional' & reform == 'post')

y_post <- summary_committe %>%
  filter(tipo_legislador == 'Mayoria Relativa' & reform == 'post')

result = t.test(x_post$n, y_post$n, alternative="two.sided")
tidy(result)
result$stderr

###############
# 3 Floor Speeches
###############
speech <- read_csv('05-aggregated-data/counts_speech.csv') %>%
  select(-c(X1))

summary_speech <- speech %>%
  left_join()
  mutate(reform = ifelse(legislatura < 64, 'pre', 'post'))

x <- summary_speech %>%
  filter(reform == 'post')

y <- summary_speech %>%
  filter(reform == 'pre')

result = t.test(x$counter, y$counter, alternative="two.sided")
tidy(result)
result$stderr

###############
# 2 Floor Attendance
###############


attendance <- read_csv('05-aggregated-data/attendance_data.csv') %>%
  select(-c(X1))

summary_attendance <- left_join(id_data_prop, attendance, by = c('id_legislador' = 'id', 'legislatura')) %>%
  select(tipo_legislador, id_legislador, type_attendance) %>%
  mutate(attendance_num = ifelse(type_attendance == 'asistencia', 1, 0)) %>%
  group_by(tipo_legislador, id_legislador) %>%
  mutate(n = n())%>%
  group_by(tipo_legislador, id_legislador) %>%
  mutate(n_attendance = sum(attendance_num)) %>%
  select(tipo_legislador, id_legislador, n, n_attendance) %>%
  distinct() %>%
  mutate(pct_attendance = n_attendance/n)


sd(summary_attendance$n)
summary(summary_attendance)

x <- summary_attendance %>%
  filter(tipo_legislador == 'Representacion proporcional')

y <- summary_attendance %>%
  filter(tipo_legislador == 'Mayoria Relativa')

result = t.test(x$pct_attendance, y$pct_attendance, alternative="two.sided")
tidy(result)
result$stderr

###############
# 3 Roll Call Votes
###############


votes <- read_csv('05-aggregated-data/roll_call_votes.csv') %>%
  select(-c(X1)) %>%
  mutate(voto_num = ifelse(voto == 'A favor', 'yes', 
                           ifelse(voto == 'En Contra', 'no', 'other')))

party_votes <- votes %>% 
  select(legislatura, titulo_votacion, party, voto_num) %>%
  group_by(legislatura, titulo_votacion, party, voto_num) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(legislatura, titulo_votacion, party) %>%
  mutate(max = max(n), 
         dummy_max = ifelse(n == max, 1, 0)) %>%
  filter(dummy_max == 1) %>%
  select(legislatura, titulo_votacion, party,'voto_party'  = voto_num)


votes <- left_join(id_data_prop, votes, by = c('id_legislador' = 'id', 'legislatura')) %>%
  select(tipo_legislador, legislatura, id_legislador, titulo_votacion,voto_num, party) %>%
  filter(!is.na(titulo_votacion)) %>%
  left_join(party_votes, by = c('legislatura', 'titulo_votacion', 'party'))%>%
  mutate(vote_with_party = ifelse(voto_num  == voto_party, 1, 0)) %>%
  group_by(tipo_legislador, id_legislador) %>%
  mutate(n = n(), 
         num_vote_with_party = sum(vote_with_party)) %>%
  select(tipo_legislador, id_legislador, num_vote_with_party, n) %>%
  distinct() %>%
  mutate(pct_vote_with_party = num_vote_with_party/n) 

x <- votes %>%
  filter(tipo_legislador == 'Representacion proporcional')

y <- votes %>%
  filter(tipo_legislador == 'Mayoria Relativa')

result = t.test(x$pct_vote_with_party, y$pct_vote_with_party, alternative="two.sided")
result$stderr



summary_votes <- left_join(id_data_prop, votes, by = c('id_legislador' = 'id', 'legislatura')) %>%
  select(legislatura, id_legislador, voto) %>%
  group_by(legislatura, id_legislador) %>%
  summarise(
    n = n()
  )


sd(summary_votes$n)
summary(summary_votes)



###############
# 3 Legislation
###############

bills <- read_csv('05-aggregated-data/bills_proposed.csv') %>%
  select(-c(X1))

summary_bills <- left_join(id_data_prop, bills, by = c('id_legislador' = 'id', 'legislatura')) %>%
  select(tipo_legislador, id_legislador) %>%
  group_by(tipo_legislador, id_legislador) %>%
  summarise(
    n = n()
  )

x <- summary_bills %>%
  filter(tipo_legislador == 'Representacion proporcional')

y <- summary_bills %>%
  filter(tipo_legislador == 'Mayoria Relativa')

result = t.test(x$n, y$n, alternative="two.sided")
result$stderr


bills <- read_csv('05-aggregated-data/bills_proposed.csv') %>%
  select(-c(X1))

summary_bills <- left_join(id_data_prop, bills, by = c('id_legislador' = 'id', 'legislatura')) %>%
  filter(legislatura < 64) %>%
  select(tipo_legislador, id_legislador) %>%
  group_by(tipo_legislador, id_legislador) %>%
  summarise(
    n = n()
  )

x <- summary_bills %>%
  filter(tipo_legislador == 'Representacion proporcional')

y <- summary_bills %>%
  filter(tipo_legislador == 'Mayoria Relativa')

result = t.test(x$n, y$n, alternative="two.sided")
result$stderr



