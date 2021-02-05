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

#devtools::install_github("conjugateprior/austin") ## install austin package
#devtools::install_github("kasperwelbers/corpus-tools", force = TRUE) ## install corpus-tools

lapply(list.packages,
       require,
       character.only = TRUE)

## One Topic, One Congress: Health LX

## load re-labaled speeches

load('wordfish/01_lda/speeches_20k.Rda', verbose = TRUE)
dataFiles <- lapply(Sys.glob("01-clean_data/disaaggregated_data/*xlsx"), read_excel)
party_data <- rbind(select(dataFiles[[1]], c(id_speech,inc_party)), 
                    select(dataFiles[[2]], c(id_speech,inc_party)), 
                    select(dataFiles[[3]], c(id_speech,inc_party)), 
                    select(dataFiles[[4]], c(id_speech,inc_party)), 
                    select(dataFiles[[5]], c(id_speech,inc_party))) 

top10_topics <- unique(subset(docs_20k, top10_topics == 1)$topic_label)
all_topics <- unique(docs_20k$topic_label)
legistalturas <- unique(docs_20k$legislatura)


obtain_directions_pan <- function(subset){
  subset_data$counter <- c(1:nrow(subset_data))
  unique(subset_data$inc_party)
  
  pan_values <- subset_data %>%
    filter(inc_party == 'Partido Acción Nacional')
  
  pan_values <- pan_values$counter[1]
  
  return(pan_values)
}

obtain_directions_prd <- function(subset){
  subset_data$counter <- c(1:nrow(subset_data))
  unique(subset_data$inc_party)
  
  prd_values <- subset_data %>%
    filter(inc_party == "Partido de la Revolución Democrática")
  
  prd_values <- prd_values$counter[1]
  
  return(prd_values)
}

models <- list()
estimates <- list()
for (i in all_topics){
  for (j in legistalturas){
    
    subset_data <- docs_20k %>%
      left_join(party_data, by = 'id_speech')%>%
      filter(topic_label == i)  %>%
      filter(legislatura == j) %>%
      group_by(id_legislador, inc_party) %>%
      summarise(clean_speech = paste0(clean_speech, collapse = ' '))
    
    #print(nrow(subset_data))
    pan_direction = obtain_directions_pan(subset_data)
    prd_direction = obtain_directions_prd(subset_data)
    
    dfm_ob <- dfm(subset_data$clean_speech)
    dfm_trim <- dfm_trim(dfm_ob, min_termfreq = 1, min_docfreq = 2)
    dfm_trim@docvars$docname_ <- subset_data$id
    dfm_trim@docvars$docid_ <- subset_data$id
    
    wf_q <- textmodel_wordfish(dfm_trim, dir = c(prd_direction, pan_direction))
    wf_q$docs <- subset_data$id
    
    df <- select(subset_data, id_legislador, inc_party)
    df$estimated_theta <- wf_q$theta
    df$legislatura <- j
    df$topic <- i
    
    models[[paste0(i, '_', j)]] <- wf_q
    estimates[[paste0(i, '_', j)]] <- df
    
    # name_file_model = paste0('./wordfish/2-model/wordfish_', j ,'legis', '.Rda')
    # name_file_estimates = paste0('./wordfish/2.1-model_estimates/estimates_', j ,'legis', '.Rda')
    # 
    # save(df, file = name_file_estimates)
    # save(wf_q, file = name_file_estimates)
    
    
    # features_plots <- textplot_scale1d(wf_q, 
    #                  margin = "features", 
    #                  highlighted_color = "black")
    # name_plot <- paste0('./wordfish/3-figures/features_', i, '_', j ,'legis', '.png')
    # ggsave(name_plot)
    # 
  }
}

save(models, file = 'wordfish/2-model/wordfish_model.Rda')

all_estimates <- bind_rows(estimates)
save(all_estimates, file = 'wordfish/2.1-model_estimates/all_estimates.Rda')


'./wordfish/3-figures/'

dfm_ob <- dfm(data_speech$clean_speech)
dfm_trim <- dfm_trim(dfm_ob, min_termfreq = 1, min_docfreq = 2)
dfm_trim@docvars$docname_ <- data_speech$id
dfm_trim@docvars$docid_ <- data_speech$id

wf_q <- textmodel_wordfish(dfm_trim, dir = c(328, 135))
wf_q$docs <- data_speech$id
textplot_scale1d(wf_q, margin = "documents")
textplot_scale1d(wf_q, margin = "features")

df <- as.data.frame(wf_q$features)
df$beta <- wf_q$beta

df %>% top_n(30)
df %>% top_n(-30)

df <- select(subset_data, id_legislador, inc_party)
df$estimated_theta <- wf_q$theta
df$legislatura <- j

select(subset_data, id_legislador, inc_party)

df_parties  <- df %>%
  left_join(id_data, by = 'id') 

ggplot(df_parties, aes(x=reorder(id, theta), y = theta, colour = inc_party)) +
  #scale_color_manual(values = pallette) +
  geom_point()+ 
  coord_flip() +
  theme(axis.text.y = element_blank()) #+
  #labs(x = 'Candidate', title  = paste0(all_topics[i], ': Legislatura LX')) + ggsave(filename = paste0(out_plots, 'wordfish60_',all_topics[i] , '.png'))



x <- 1
list_wf <- list()
for(i in 1:length(all_topics)){
  
  ## prep data  
  data_lda_topic <- data_speech %>% filter(topic_name == all_topics[i]) %>%
    group_by(id, inc_party) %>%
    summarise(clean_text= paste0(speech, collapse = " "))
  dfm_ob <- dfm(data_lda_topic$clean_text)
  dfm_trim <- dfm_trim(dfm_ob, min_termfreq = 1, min_docfreq = 2)
  dfm_trim@docvars$docname_ <- data_lda_topic$NAME_CAND
  dfm_trim@docvars$docid_ <- data_lda_topic$NAME_CAND
  
  # set direction
  ancla_lib = as.numeric(strsplit(anchors[i], '-')[[1]][1])
  ancla_cons = as.numeric(strsplit(anchors[i], '-')[[1]][2])
  
  ## wordfish
  wf_q <- textmodel_wordfish(dfm_trim, dir = c(ancla_cons, ancla_lib))
  wf_q$docs <- data_lda_topic$id
  textplot_scale1d(wf_q, margin = "documents")
  textplot_scale1d(wf_q, margin = "features")
  
  out_wordfish <- '/Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/wordfish/03_ideal_wordfish/'
  name_file = paste0(out_wordfish, 'wordfish_LX_', all_topics[i], '.RData')
  list_wf[[x]] <- wf_q
  x = x + 1
  
  df <- data.frame(matrix(unlist(data_lda_topic$id), nrow=length(data_lda_topic$id), byrow=T))
  df$estimated_theta <- wf_q$theta
  colnames(df) <- c('id', 'theta')
  
  
  party <- data_speech %>%
    select(id, inc_party, inc_name)
  all_df <- left_join(df, party, by = 'id')
  
  
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
  
  out_plots <- '/Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/wordfish/04_ideal_figures/'
  ggplot(all_df, aes(x=reorder(inc_name, theta), y = theta, colour = inc_party)) +
    #scale_color_manual(values = pallette) +
    geom_point()+ 
    coord_flip() +
    theme(axis.text.y = element_blank()) +
    labs(x = 'Candidate', title  = paste0(all_topics[i], ': Legislatura LX')) + ggsave(filename = paste0(out_plots, 'wordfish60_',all_topics[i] , '.png'))
}

save(list_wf, file = '/Users/cblanesg/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/01-text_analysis/wordfish/03_ideal_wordfish/wordfish_LX.Rda')



data_lda_topic <- data_speech %>% filter(topic_name == 'topic9') %>%
  group_by(id, inc_party) %>%
  summarise(clean_text= paste0(speech, collapse = " "))
nrow(data_lda_topic)

dfm_ob <- dfm(data_lda_topic$clean_text)
dfm_trim <-dfm_trim(dfm_ob, min_termfreq = 1, min_docfreq = 2)
dfm_trim@docvars$docname_ <- data_lda_topic$id
dfm_trim@docvars$docid_ <- data_lda_topic$id

dfm_trim@Dimnames$docs <- data_lda_topic$id

wordfish_output <- textmodel_wordfish(dfm_trim, dir = c(pan, prd))

textplot_scale1d(wordfish_output , margin = "documents")
textplot_scale1d(wordfish_output , margin = "features")

df <- data.frame(matrix(unlist(data_lda_topic$id), nrow=length(data_lda_topic$id), byrow=T))
df$estimated_theta <- wordfish_output$theta
colnames(df) <- c('id_candidate', 'theta')

all_df <- left_join(df, id_data, by = c('id_candidate' = 'id'))
id_data <- read_excel(paste0(input, 60, '_disaggregated.xlsx')) %>%
  select(id, inc_name, inc_party)  %>%
  distinct()

ggplot(all_df, aes(x=reorder(inc_name, theta), y = theta, colour = inc_party)) + 
  geom_point()+ 
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + coord_flip()
