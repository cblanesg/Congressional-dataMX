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
all_estimates %>%
  group_by(legislatura, inc_party, topic)  %>%
  summarise(median_party = median(estimated_theta), 
            mean_party = mean(estimated_theta))

docs_with_party <- left_join(docs_20k, data_party, 
           by = 'id_legislador')

#write.csv(docs_with_party, file = 'wordfish/01_lda/speeches_20k.csv')

legislaturas <- c('LX', 'LXI', 'LXII', 'LXIII', 'LIX')
pallette <- c('Partido de la Revolucion Democratica' = '#F4D03F',
              'Movimiento Ciudadano' ='#ff6700',
              'Partido del Trabajo' ='#DC2D07',
              'Partido Social Democrata' ='#496eaa',
              'Sin Partido' ='#c5a7d8',
              'Partido Revolucionario Institucional' ='#d63600',
              'Nueva Alianza' = '#00c1bc',
              'Partido Verde Ecologista de Mexico' ='#27AE60',
              'Partido Accion Nacional' ='#3498DB', 
              'Morena' = '#960000', 
              'Partido Encuentro Social' = '#bdc530')
top10_topics  <- unique(subset(docs_20k, top10_topics == 1)$topic_label)
unique(all_estimates$inc_party)
all_estimates_main_parties <- all_estimates %>%
  filter(inc_party %in% c('Partido de la Revolución Democrática',
                          'Partido Revolucionario Institucional', 
                          'Partido Acción Nacional',
                          'Morena', 
                          'Partido Verde Ecologista de México', 
                          'Movimiento Ciudadano'))  %>%
  filter(topic %in% top10_topics)

ggplot(subset(all_estimates, legislatura == 63), aes(x = estimated_theta, colour = inc_party)) +
  geom_density() +
  facet_wrap(.~topic)

ggplot(subset(all_estimates, legislatura == 64 & topic == 'vivienda'), aes(x = estimated_theta, colour = inc_party)) +
  geom_density() 


########## Points with variance

data_summary <- all_estimates_main_parties %>%
  group_by(inc_party, legislatura) %>%
  dplyr::summarise(ideal_point = mean(estimated_theta, na.rm = TRUE), 
                   sd_ideal = sd(estimated_theta, na.rm = TRUE))

#### 3.1 Graphs
ggplot(data_summary, aes(x= reorder(inc_party, ideal_point), y = ideal_point,  group = inc_party, color = inc_party)) + 
  geom_point()+
  #geom_pointrange(aes(ymin = ideal_point-sd_ideal, ymax = ideal_point +sd_ideal)) + 
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + coord_flip() + 
  facet_wrap(.~legislatura)


unique(subset(all_estimates_main_parties, legislatura == 60)$topic)
unique(all_estimates_main_parties$inc_party)
unique(all_estimates$inc_party)

ggplot(subset(all_estimates_main_parties, legislatura == 63), aes(x=reorder(id_legislador, estimated_theta), y = estimated_theta, colour = inc_party)) +
  #scale_color_manual(values = pallette) +
  geom_point() + 
  coord_flip() +
  theme(axis.text.y = element_blank())  +facet_wrap(.~topic)
  
#labs(x = 'Candidate', title  = paste0(all_topics[i], ': Legislatura LX')) + ggsave(filename = paste0(out_plots, 'wordfish60_',all_topics[i] , '.png'))


ggplot(subset(all_estimates_main_parties, legislatura == 61 & topic == 'salud'), aes(x=id_legislador, y = estimated_theta, colour = inc_party)) +
  geom_point() + 
  coord_flip() +
  theme(axis.text.y = element_blank()) 

ggplot(subset(all_estimates_main_parties, legislatura == 63 & topic == 'energia'), aes(x=reorder(id_legislador, estimated_theta), y = estimated_theta, colour = inc_party)) +
  geom_point() + 
  coord_flip() +
  theme(axis.text.y = element_blank())  +facet_grid(.~topic)
#labs(x = 'Candidate', title  = paste0(all_topics[i], ': Legislatura LX')) + ggsave(filename = paste0(out_plots, 'wordfish60_',all_topics[i] , '.png'))


