
library(tidyverse)
library(stargazer)
library(ggplot2)
library(wnominate)
library(pscl)
library(oc)
library(data.table)

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

folder <- "../02-outcomes/01-policy_positioning/00-roll_call/00-prep_data/"
files <- list.files(folder, pattern = ".csv$")
legislaturas <- c('60', '61','62', '63', '64')


file_votes <- list()
for (i in 1:length(files)){
  d <- read_csv(file = paste0('../02-outcomes/01-policy_positioning/00-roll_call/00-prep_data/', files[[i]])) %>%
    select(-c(X1)) %>%
    mutate(voto_num = ifelse(vote == 'A favor', '1', 
                             ifelse(vote == 'En contra', '6', '9')), 
           legis = legislaturas[[i]]) %>%
    select(legis, id_legislador, partido, voto_num, bill)
  file_votes[[i]] <- d
}

###############################################
# 1.0 Anchor Directions- Optimal Classification
###############################################

obtain_rollcall_obj <- function(df){
  matrix_roll_call <- df %>%
    select(-c(legis)) %>%
    spread(bill, value = voto_num) %>%
    as.matrix()
  LEGnames <- matrix_roll_call[, 1]
  legData <- matrix(matrix_roll_call[, 2], length(matrix_roll_call[, 2]), 1)
  colnames(legData) <- "party"
  matrix_roll_call <- matrix_roll_call[, -c(1, 2)]
  
  rc <- rollcall(matrix_roll_call, 
                 yea = c('1'), 
                 nay = c('6'), 
                 missing = c('9'), 
                 notInLegis = 0,
                 legis.names = LEGnames, legis.data = legData,
                 desc = "Mexican Deputies Chamber"
  )
  return(rc)
}

obtain_anchors <- function(df){
  rc <- obtain_rollcall_obj(df)
  result <- oc(rc,polarity=1,dims=1)
  df_results <- result[["legislators"]]
  df_results <- setDT(df_results, keep.rownames = TRUE)[]
  
  
  legis_max_val <- df_results %>%
    filter(party == 'Partido Accion Nacional') %>%
    filter(coord1D == max(coord1D, na.rm = TRUE) ) %>%
    select(rn)
  legislator1 <- legis_max_val$rn[1]
  return(c(legislator1))
}

list_anchors <- list()
for (i in 1:length(file_votes)){
  list_anchors[[i]] <- obtain_anchors(file_votes[[i]])
}


#############################
# 1.1 Ideal Point Estimations
#############################

rc <- rollcall(matrix_roll_call, 
               yea = c('1'), 
               nay = c('6'), 
               missing = c('9'), 
               notInLegis = 0,
               legis.names = LEGnames, legis.data = legData,
               desc = "Mexican Deputies Chamber"
)


estimation_dwnominate <- function(votes,legislatura, anchors){
  matrix_roll_call <- votes %>%
    spread(bill, value = voto_num) %>%
    as.matrix()
  LEGnames <- matrix_roll_call[, 1]
  legData <- matrix(matrix_roll_call[, 2], length(matrix_roll_call[, 2]), 1)
  colnames(legData) <- "party"
  matrix_roll_call <- matrix_roll_call[, -c(1, 2)]
  
  rc <- rollcall(matrix_roll_call, 
                 yea = c('1'), 
                 nay = c('6'), 
                 missing = c('9'), 
                 notInLegis = 0,
                 legis.names = LEGnames, legis.data = legData,
                 desc = "Mexican Deputies Chamber"
  )
  result <- wnominate(rc, polarity = c(anchors), dim = 1)

    save(result, file = paste0('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/00-roll_call/01-ideal_points/dw_nominate', legislatura, '.Rda'))
  return(result)
}

list_results <- list()
for (i in 1:length(file_votes)){
  df <- file_votes[[i]]
  legis <- unique(df$legis)
  df <- df %>% select(-c(legis))
  r <- estimation_dwnominate(votes = df, legis, list_anchors[[i]][1])
  list_results[[i]] <- r
}

#####################
# 1.1 Make Nice Plots
#####################

rank_plots <- function(result, legis){
  df <- result$legislators %>%
    mutate(rank = min_rank(coord1D)) %>%
    arrange(rank)
  
  ggplot(data = df, aes(x = coord1D, y = rank, colour = party)) +
    scale_color_manual(values = pallette) +
    geom_point() +
    theme_minimal() +
    labs(title = paste0('dw-nominate ideal points: Legislatura ', legis)) +
    ggsave(filename = paste0('../02-outcomes/01-policy_positioning/00-roll_call/02-graphs/dw_nominate', legis, '.png'))
  
  ggplot(data = df, aes(x = coord1D, colour = party)) +
    scale_color_manual(values = pallette) +
    geom_density() +
    theme_minimal() +
    labs(title = paste0('dw-nominate ideal points: Legislatura ', legis)) +
    ggsave(filename = paste0('../02-outcomes/01-policy_positioning/00-roll_call/02-graphs/dw_nominate_density', legis, '.png'))
}

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

for (i in 1:length(list_results)){
  rank_plots(list_results[[i]], 
             legislaturas[[i]])
}