

setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/')

library(tidyverse)
library(ggplot2)
library(readxl)

load('02-outcomes/01-policy_positioning/01-text_analysis/wordfish/03_ideal_wordfish/wordfish_model.Rda', verbose = TRUE)
load('02-outcomes/01-policy_positioning/01-text_analysis/wordfish/03_ideal_wordfish/all_estimates.Rda', verbose = TRUE)

data <- all_estimates %>%
  mutate(reforma = ifelse(legislatura == 64, 'Después de Reforma Político Electoral', 
                          'Antes de Reforma Político Electoral')) %>%
  filter(inc_party %in% c('Morena', 
                          "Partido de la Revolución Democrática", 
                          "Partido Acción Nacional",
                          "Partido Revolucionario Institucional",
                          "Partido Verde Ecologista de México", 
                          "Movimiento Ciudadano"))
  
ggplot(data, aes(x = estimated_theta, colour = inc_party)) + 
  geom_density()  + 
  facet_wrap(.~reforma, scales = 'free')


### Recover type of Legislator

load('03-empirical_strategy/1-input_reg/data_reg.Rda', verbose = TRUE)

data <- data %>%
  left_join(select(data_reg, id_legislador, tipo_legislador), 
            'id_legislador') 

  
###### Plot Parties

load('02-outcomes/01-policy_positioning/01-text_analysis/wordfish/01_lda/speeches_20k.Rda', verbose = TRUE)
  
main_topics <- docs_20k %>%
  select(topic_label, topics20k) %>%
  ungroup() %>%
  group_by(topic_label) %>%
  count() %>%
  top_n(n = 5, wt = freq)

main_topics$topic_label


main_topics <- c("comunicacion, responsabilidad administrativa", 
                 "energia",
                 "fiscal, energia",
                 "fuerzas armadas",
                 "genero")


unique(data$topic)
data_topics <- data %>%
  filter(topic %in% main_topics)

ggplot(data_topics, aes(x = estimated_theta, fill = reforma)) + 
  geom_density(alpha = 0.3)  + 
  facet_wrap(.~inc_party, scales = 'free')

ggplot(data_topics, aes(x = estimated_theta, fill = tipo_legislador)) + 
  geom_density(alpha = 0.3)  + 
  facet_wrap(.~reforma, scales = 'free')

ggplot(data_topics, aes(x = estimated_theta, colour = inc_party)) + 
  geom_density()  + 
  facet_wrap(.~reforma, scales = 'free')

ggplot(data_topics, aes(x = estimated_theta, fill = tipo_legislador)) + 
  geom_density(alpha = 0.3)  + 
  facet_wrap(.~reforma, scales = 'free')

ggplot(subset(data_topics, inc_party == 'Partido Acción Nacional'), 
       aes(x = estimated_theta, fill = topic)) + 
  geom_density(alpha = 0.3)  + 
  facet_wrap(.~tipo_legislador, scales = 'free')

estimates_party <- data %>%
  dplyr::group_by(inc_party, reforma)%>%
  dplyr::summarise(ideal_point = median(estimated_theta, na.rm = TRUE))
  





