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

dataFiles <- lapply(Sys.glob("01-clean_data/disaaggregated_data/*xlsx"), read_excel)
party_data <- rbind(select(dataFiles[[1]], c(id_speech,inc_party)), 
                    select(dataFiles[[2]], c(id_speech,inc_party)), 
                    select(dataFiles[[3]], c(id_speech,inc_party)), 
                    select(dataFiles[[4]], c(id_speech,inc_party)), 
                    select(dataFiles[[5]], c(id_speech,inc_party)))

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

all_estimates_main_parties <- all_estimates %>%
  filter(inc_party %in% c('Partido de la Revolución Democrática',
                          'Partido Revolucionario Institucional', 
                          'Partido Acción Nacional',
                          'Morena', 
                          'Partido Verde Ecologista de México', 
                          'Movimiento Ciudadano'))  %>%
  filter(topic %in% top10_topics)

ggplot(all_estimates_main_parties, aes(x = estimated_theta, colour = inc_party)) +
  geom_density() +
  facet_wrap(.~topic, scales = 'free') +
  labs(title = 'Distribution of Ideal Points by Party', 
       subtitle = 'Top 10 topics, Legislatura LXIV')

ggplot(subset(all_estimates, legislatura == 64 & topic == 'vivienda'), aes(x = estimated_theta, colour = inc_party)) +
  geom_density() 


######### Wordfish

out_wordfish <- 'data/02-outcomes/01-policy_positioning/01-text_analysis/wordfish/03_ideal_wordfish/'

load('wordfish/2-model/wordfish_model.Rda')

names(models)[76]

str_split(names(models)[76], pattern = '_')[[1]]
length(models)

for (i in 81:length(models)){
  topic = str_split(names(models)[i], pattern = '_')[[1]][1]
  legis = str_split(names(models)[i], pattern = '_')[[1]][2]
  
  if (topic == 'topic'){
    topic = str_split(names(models)[i], pattern = '_')[[1]][1] + '_' + str_split(names(models)[i], pattern = '_')[[1]][2]
    legis = str_split(names(models)[i], pattern = '_')[[1]][3]
    
    subset_data <- docs_20k %>%
      left_join(party_data, by = 'id_speech')%>%
      filter(topic_label == topic)  %>%
      filter(legislatura == legis) %>%
      group_by(id_legislador, inc_party) %>%
      summarise(clean_speech = paste0(clean_speech, collapse = ' ')) %>%
      mutate(id_legislador = gsub(pattern = '-11ea-95ca-acde48001122', replacement = '', x = id_legislador)) %>%
      mutate(party = ifelse(inc_party == 'Partido Acción Nacional', 'PAN', 
                            ifelse(inc_party == "Partido de la Revolución Democrática", 'PRD', 
                                   ifelse(inc_party == "Partido Revolucionario Institucional", 'PRI', 
                                          ifelse(inc_party =="Convergencia", 'MC', 
                                                 ifelse(inc_party == "Partido del Trabajo", 'PT', 
                                                        ifelse(inc_party == "Partido Verde Ecologista de México", 'PVEM', 
                                                               ifelse(inc_party == "Partido Nueva Alianza", 'PNA', 
                                                                      ifelse(inc_party == 'Morenal', 'Morena', 
                                                                             ifelse(inc_party == 'Partido Encuentro Social', 'PES', inc_party))))))))))
    
    
    models[[i]]$docs <- subset_data$id_legislador
    p <- textplot_scale1d(models[[i]], margin = "documents", 
                          groups = subset_data$party) 
    
    p + 
      labs(title =paste0(topic), 
           subtitle = paste0('Ideal Point Estimations, ', 'Legislatura ', legis)) +
      theme(axis.ticks.y = element_blank()) + 
      ggsave(filename = paste0('wordfish/04_ideal_figures/2-documents/', names(models)[i], '.png'))
  }else{
    
    subset_data <- docs_20k %>%
      left_join(party_data, by = 'id_speech')%>%
      filter(topic_label == topic)  %>%
      filter(legislatura == legis) %>%
      group_by(id_legislador, inc_party) %>%
      summarise(clean_speech = paste0(clean_speech, collapse = ' ')) %>%
      mutate(id_legislador = gsub(pattern = '-11ea-95ca-acde48001122', replacement = '', x = id_legislador)) %>%
      mutate(party = ifelse(inc_party == 'Partido Acción Nacional', 'PAN', 
                            ifelse(inc_party == "Partido de la Revolución Democrática", 'PRD', 
                                   ifelse(inc_party == "Partido Revolucionario Institucional", 'PRI', 
                                          ifelse(inc_party =="Convergencia", 'MC', 
                                                 ifelse(inc_party == "Partido del Trabajo", 'PT', 
                                                        ifelse(inc_party == "Partido Verde Ecologista de México", 'PVEM', 
                                                               ifelse(inc_party == "Partido Nueva Alianza", 'PNA', 
                                                                      ifelse(inc_party == 'Morenal', 'Morena', 
                                                                             ifelse(inc_party == 'Partido Encuentro Social', 'PES', inc_party))))))))))
    
    
    models[[i]]$docs <- subset_data$id_legislador
    p <- textplot_scale1d(models[[i]], margin = "documents", 
                          groups = subset_data$party) 
    
    p + 
      labs(title =paste0(topic), 
           subtitle = paste0('Ideal Point Estimations, ', 'Legislatura ', legis)) +
      theme(axis.ticks.y = element_blank()) + 
      ggsave(filename = paste0('wordfish/04_ideal_figures/2-documents/', names(models)[i], '.png'))
  }

  
  
}

names(models)[1]

for (i in 1:length(models)){
  
  clean_name = gsub(pattern = '_', replacement = ' ',  x = names(models)[i])
  elements = str_split(string = clean_name, pattern = ' ')[[1]]
  
  legislatura = elements[length(elements) -1]
  topic_clean = paste0(elements[1:length(elements) -1], collapse = ' ')

  p <- textplot_scale1d(models[[i]], 
                        margin = "features")
  p + 
    labs(title = topic_clean, 
         subtitle = paste0('Word Weights. Legislatura: ', legislatura)) +
    theme(axis.ticks.y = element_blank()) + 
    ggsave(filename = paste0('wordfish/04_ideal_figures/1-features/', names(models)[i], '.png'))
  
}



textplot_scale1d(models[[72]], 
                 margin = "features",
                 highlighted = c("mujer", 
                                 'violencia', 
                                 'derecho', 'igualdad', 
                                 'hostigamiento',
                                 'justicia', 'hombre', 'discriminacion'))

subset_data <- docs_20k %>%
  left_join(party_data, by = 'id_speech')%>%
  filter(topic_label == 'genero')  %>%
  filter(legislatura == 60) %>%
  group_by(id_legislador, inc_party) %>%
  summarise(clean_speech = paste0(clean_speech, collapse = ' ')) %>%
  mutate(id_legislador = gsub(pattern = '-11ea-95ca-acde48001122', replacement = '', x = id_legislador)) %>%
  mutate(party = ifelse(inc_party == 'Partido Acción Nacional', 'PAN', 
                            ifelse(inc_party == "Partido de la Revolución Democrática", 'PRD', 
                                   ifelse(inc_party == "Partido Revolucionario Institucional", 'PRI', 
                                          ifelse(inc_party =="Convergencia", 'MC', 
                                                 ifelse(inc_party == "Partido del Trabajo", 'PT', 
                                                        ifelse(inc_party == "Partido Verde Ecologista de México", 'PVEM', 
                                                               ifelse(inc_party == "Partido Nueva Alianza", 'PNA', 
                                                                      ifelse(inc_party == 'Morenal', 'Morena', inc_party)))))))))

models[[72]]$docs <- subset_data$id_legislador
unique(subset_data$inc_party)
p <- textplot_scale1d(models[[72]], margin = "documents", 
                 groups = subset_data$party) 

p + 
  labs(title = 'Genero, Legislatura LX', 
       subtitle = 'Ideal Point Estimations') +
  theme(axis.ticks.y = element_blank()) + 
  ggsave(filename = 'wordfish/04_ideal_figures/2-documents/genero_60.png')
  

textplot_scale1d(models[[67]], 
                 margin = "features",
                 # highlighted = c("mujer", 
                 #                 'violencia', 
                 #                 'derecho', 'igualdad', 
                 #                 'hostigamiento',
                 #                 'justicia', 'hombre', 'discriminacion')
                 
                 
                 )

textplot_scale1d(models[[72]], margin = "documents")

textplot_scale1d(models[[62]], 
                 margin = "features",
                 # highlighted = c("mujer", 
                 #                 'violencia', 
                 #                 'derecho', 'igualdad', 
                 #                 'justicia', 'hombre', 'discriminacion')
                 )

textplot_scale1d(models[[2]], 
                 margin = "features",
                 # highlighted = c("mujer", 
                 #                 'violencia', 
                 #                 'derecho', 'igualdad', 
                 #                 'justicia', 'hombre', 'discriminacion')
)

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


