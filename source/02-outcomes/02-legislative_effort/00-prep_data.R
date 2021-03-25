
setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/')

rm(list=ls())

## Output directory
output <- "data/02-outcomes/02-legislative_effort//"

list.packages <- c("ggplot2", 
                   'stargazer', 
                   'tidyverse', 
                   'readxl')

lapply(list.packages,
       require,
       character.only = TRUE)

## attendance 
prep_attendance <- read_excel('/Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/01-collection_data/04-floor_attendance/03-clean_attendance/floor_attendance_aggregated.xlsx') %>%
  select(-c(`...1`, `Unnamed: 0`)) 

sessions_legis <- prep_attendance %>%
  filter(!is.na(id)) %>%
  group_by(legislatura, id) %>%
  count() %>%
  ungroup() %>%
  select(legislatura, n)%>%
  distinct() %>%
  ungroup() %>% group_by(legislatura) %>%
  mutate(max_sessions = max(n), 
         dummy_max = ifelse(n == max_sessions, 1, 0)) %>%
  filter(dummy_max == 1)%>%
  select(legislatura, max_sessions)

prep_attendance <-left_join(prep_attendance, sessions_legis, by = 'legislatura') %>%
  mutate(dummy_attendance = ifelse(type_attendance %in% c("ASISTENCIA", "A", "A/A", "OFICIAL COMISIÓN", "CÉDULA", "AO", "AC/A", "AC"), 1, 0))%>%
  group_by(legislatura, id)  %>%
  summarise(pct_attendance = sum(dummy_attendance)/max_sessions) %>%
  distinct()%>%
    select(legislatura, id, pct_attendance)

id_data <- read_excel('data/01-collection_data/00-id_data/00-id/id_data_aggregate.xlsx') %>%
  select(legislatura, id_legislador,nombre_completo,  tipo_legislador, suplente_propietario, partido)


attendance_data <- left_join(prep_attendance, id_data, by = c('id' = 'id_legislador', 'legislatura')) %>%
  filter(!is.na(id)) %>%
  filter(suplente_propietario == 'P') %>%
  mutate(reelection_dummy = ifelse(legislatura == 64, 1, 0), 
         ssd_legis = ifelse(tipo_legislador == 'Mayoria Relativa', 1, 0))

write.csv(attendance_data, file = 'data/02-outcomes/02-legislative_effort/attendance_data.csv')


### bills
prop_data <- id_data %>%
  select(legislatura, id_legislador, nombre_completo, tipo_legislador, suplente_propietario, partido)%>%
  filter(suplente_propietario == 'P') 

prep_bills <- read_csv('data/01-collection_data/06-aggregated-data/bills_proposed.csv')

bills_data <- prep_bills %>%
  distinct() %>%
  filter(!is.na(id))%>% 
  group_by(legislatura, id) %>%
  count() %>%
  select(legislatura, id, 'number_bills' = 'n')%>% 
  left_join(prop_data, by = c('id' = 'id_legislador', 'legislatura')) %>%
  ungroup() %>%
  group_by(legislatura) %>%
  mutate(reelection_dummy = ifelse(legislatura == 64, 1, 0), 
         ssd_legis = ifelse(tipo_legislador == 'Mayoria Relativa', 1, 0), 
         share_bills = number_bills/sum(number_bills, na.rm = TRUE))

write.csv(bills_data, file = 'data/02-outcomes/02-legislative_effort/bills_data.csv')


prep_bills2 <- prep_bills %>%
  filter(!is.na(id))%>% 
  group_by(id, legislatura) %>% 
  count()

ggplot(bills_data, mapping = aes(x = number_bills)) + 
  geom_density() +  facet_wrap(.~legislatura)

## floor speeches

prep_speech <- read_excel('data/01-collection_data/01-floor_speech/02-counts_speeches/counts_speeches.xlsx') %>%
  filter(!is.na(id)) %>% 
  select(-c(`...1`)) %>% 
  right_join(prop_data, by = c('id' = 'id_legislador', 'legislatura')) %>%
  mutate(reelection_dummy = ifelse(legislatura == 64, 1, 0), 
         ssd_legis = ifelse(tipo_legislador == 'Mayoria Relativa', 1, 0), 
         counter = replace_na(counter, 0)) %>%
  rename('count_speech' = 'counter')

data_speech <- left_join(prep_speech, sessions_legis, by  = 'legislatura') %>%
  mutate(share_speech = count_speech/max_sessions)

write.csv(data_speech, file = 'data/02-outcomes/02-legislative_effort/speech_data.csv')


## committee activity

prep_committee <- read_csv('data/01-collection_data/06-aggregated-data/committee_data.csv')

data_committee <- prep_committee %>%
  group_by(legislatura, id_legislador) %>%
  count() %>%
  right_join(prop_data, by = c( 'id_legislador', 'legislatura')) %>%
  mutate(reelection_dummy = ifelse(legislatura == 64, 1, 0), 
         ssd_legis = ifelse(tipo_legislador == 'Mayoria Relativa', 1, 0), 
         counter = replace_na(n, 0)) %>%
  rename('number_committees' = 'n')

write.csv(data_committee, file = 'data/02-outcomes/02-legislative_effort/committee_data.csv')

## aggregate all data

committee2 <- select(data_committee, id_legislador, number_committees)
speeches2 <- select(data_speech, 'id_legislador' = 'id', share_speech)
bills2 <- select(bills_data, 'id_legislador' = 'id',number_bills)
attendance2 <- select(attendance_data, 'id_legislador' = 'id',pct_attendance)

prep_effort <- left_join(prop_data, committee2) %>%
  left_join(speeches2) %>%
  left_join(bills2) %>%
  left_join(attendance2) 

data_effort <- prep_effort %>%
  mutate(number_committees = replace_na(number_committees, 0), 
         count_speech = replace_na(share_speech, 0), 
         number_bills = replace_na(number_bills, 0), 
         pct_attendance = replace_na(pct_attendance, 0))
select(data_effort, number_committees, count_speech, number_bills, pct_attendance)

pca_effort <- prcomp(x = select(data_effort, number_committees, count_speech, number_bills, pct_attendance))

data_effort$PC1 <- as.data.frame(pca_effort$x)$PC1

data_effort$tipo_legislador
data_effort <- data_effort%>%
  mutate(reelection_dummy = ifelse(legislatura == 64, 1, 0), 
         ssd_legis = ifelse(tipo_legislador == 'Mayoria Relativa', 1, 0))
write.csv(data_effort, file = '../02-outcomes/02-legislative_effort/effort_data.csv')

## correlation between effort variables

data_effort <- read_csv('data/02-outcomes/02-legislative_effort/effort_data.csv')

res <- cor(select(data_effort, number_committees, count_speech, number_bills, pct_attendance))
round(res, 2)


