library(tidyverse)

setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/01-collection_data/')

########################
# 0.0 Load Data- Id data
########################

id_data <- read_csv('06-aggregated-data/id_data.csv') %>%
  select(-c(X1))
colnames(id_data)

id_data <- id_data %>%
  filter(suplente_propietario == 'P') %>%
  mutate(reform = ifelse(legislatura < 64, 'pre', 'post')) %>%
  select(reform, legislatura, id_legislador, tipo_legislador)

nrow(id_data)

######################################
# 0.1 Load Data- Ideal Point Estimates
######################################

folder <- "../02-outcomes/01-policy_positioning/00-roll_call/01-ideal_points/"
files <- list.files(folder, pattern = ".Rda$")
legislaturas <- c('60', '61','62', '63', '64')

estimates_data <- list()
for (i in 1:length(files)){
  load(paste0("../02-outcomes/01-policy_positioning/00-roll_call/01-ideal_points/", files[[i]]))
  estimates_data[[i]] <- result
}

#########################
# 1.0 Measure of Outcomes
#########################

all_data <- estimates_data[[1]][["legislators"]] %>%
  mutate(legis = 60, 
         id_inc = rownames(estimates_data[[1]][["legislators"]])) %>%
  rbind(estimates_data[[2]][["legislators"]] %>%
          mutate(legis = 61, 
                 id_inc = rownames(estimates_data[[2]][["legislators"]]))) %>%
  rbind(estimates_data[[3]][["legislators"]] %>%
          mutate(legis = 62, 
                 id_inc = rownames(estimates_data[[3]][["legislators"]])))%>%
  rbind(estimates_data[[4]][["legislators"]] %>%
          mutate(legis = 63, 
                 id_inc = rownames(estimates_data[[4]][["legislators"]])))%>%
  rbind(estimates_data[[5]][["legislators"]] %>%
          mutate(legis = 64, 
                 id_inc = rownames(estimates_data[[5]][["legislators"]])))

df_ideal_points <- all_data %>%
  group_by(party, legis) %>%
  mutate(party_dwnom = median(coord1D, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(dif_with_party = coord1D - party_dwnom, 
         abs_dif_with_party = abs(dif_with_party)) %>% 
  left_join(id_data, by = c('id_inc' = 'id_legislador', 
                            'legis' = 'legislatura'))

x <- df_ideal_points %>%
  filter(reform == 'pre')

y <- df_ideal_points %>%
  filter(reform == 'post')

result = t.test(x$abs_dif_with_party, y$abs_dif_with_party, alternative="two.sided")
tidy(result)
result$stderr

ggplot(df_ideal_points, aes(x = abs_dif_with_party, colour = reform)) + 
  geom_density()





