

library(tidyverse)
library(stargazer)
library(ggplot2)
library(wnominate)

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

votes <- read_csv('../02-outcomes/01-policy_positioning/00-roll_call/00-prep_data/roll_call_votes61.csv') %>%
  select(-c(X1)) %>%
  mutate(voto_num = ifelse(vote == 'A favor', '1', 
                           ifelse(vote == 'En contra', '6', '9'))) %>%
  select(id_legislador, partido, voto_num, bill)


#############################
# 1.0 Ideal Point Estimations
#############################

estimation_dwnominate <- function(votes,legislatura){
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
  result <- wnominate(rc, polarity = c(1, 1))
  
  df_results <- as.data.frame(as.list(result$legislators['coord1D']))
  df_results$coord2D <- as.list(result$legislators['coord2D'])$coord2D
  df_results$party <- as.list(result$legislators['party'])$party
  
  ggplot(data = df_results, mapping = aes(x = coord1D, y = coord2D, colour = party)) + 
    geom_point() + ggsave(filename = paste0('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/00-roll_call/02-graphs/dw-nominate', legislatura, '.png'))
  
  save(result, file = paste0('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/02-outcomes/01-policy_positioning/00-roll_call/01-ideal_points/dw_nominate', legislatura, '.Rda'))
  return(result)
}

r <- estimation_dwnominate(votes = votes, '61')

