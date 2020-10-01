
library(tidyverse)


setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/01-collection_data/')

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
  )

summary(summary_committe)
sd(summary_committe$n)


###############
# 2 Floor Attendance
###############


attendance <- read_csv('05-aggregated-data/attendance_data.csv') %>%
  select(-c(X1))

summary_attendance <- left_join(id_data_prop, attendance, by = c('id_legislador' = 'id', 'legislatura')) %>%
  select(legislatura, id_legislador, type_attendance) %>%
  group_by(legislatura, id_legislador) %>%
  summarise(
    n = n()
  )

sd(summary_attendance$n)
summary(summary_attendance)



###############
# 3 Roll Call Votes
###############


votes <- read_csv('05-aggregated-data/roll_call_votes.csv') %>%
  select(-c(X1))

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
  )

sd(summary_bills$n)
summary(summary_bills)











