rm(list=ls()) ## clean enviornment


setwd(dir = '~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/')

library(ggplot2)
library(tidyverse)
library(tidybayes)
library(plyr)
library(MASS)
library(data.table)
library(viridis)


#### Load data 

load('03-empirical_strategy/2-reg_results/reg_ideology_posit.Rda')
load('03-empirical_strategy/1-input_reg/data_reg.Rda', verbose = TRUE)
load('02-outcomes/01-policy_positioning/01-text_analysis/wordfish/05_party-discipline/wordfish_party_discipline.Rda', verbose = TRUE)


#### Prepare Data

data_num_members <- data_reg %>%
  dplyr::mutate(partido = ifelse(partido %in% c('pve', 'partido verde ecologista'), 'pvem',
                                 ifelse(partido == 'morenal', 'morena',
                                        ifelse(partido == 'encuentro social', 'es',
                                               ifelse(partido == 'sin partido', 'sp',
                                                      ifelse(partido == 'movimiento ciudadano', 'mc', partido)))))) %>%
  dplyr::group_by(legislatura, partido) %>%
  count() %>%
  dplyr::group_by(legislatura) %>%
  dplyr::mutate(mean_members = mean(n),
                party_size = ifelse(n >= mean_members, 1, 0)) %>%
  dplyr::select(legislatura, partido, party_size)


data_reg_clean <- data_reg  %>%
  mutate(partido = ifelse(partido %in% c('pve', 'partido verde ecologista'), 'pvem',
                          ifelse(partido == 'morenal', 'morena',
                                 ifelse(partido == 'encuentro social', 'es',
                                        ifelse(partido == 'sin partido', 'sp',
                                               ifelse(partido == 'movimiento ciudadano', 'mc', partido)))))) %>%
  mutate(partido = ifelse(nombre_completo == 'Morales Vázquez Carlos Alberto', 'prd',
                          ifelse(nombre_completo == 'Pérez Rivera Evaristo Lenin', 'pan',
                                 ifelse(nombre_completo == 'Riojas Martínez Ana Lucia', 'sp', partido)))) %>%
  left_join(data_num_members, by = c('legislatura', 'partido'))

unique(data_reg_clean$main_parties)

ideal_parties <- discipline_data %>%
  left_join(select(data_reg_clean, id_legislador, main_parties), 'id_legislador')%>%
  select(main_parties, legislatura, estimated_theta) %>%
  distinct() %>%
  dplyr::group_by(main_parties, legislatura) %>%
  dplyr::summarise(ideal_party = median(estimated_theta, na.rm = TRUE)) %>%
  ungroup() 


### simulations

means <- as.matrix(coef(fit.main[4][[1]]))
means <- means[!is.na(means)]
cov_matrix <- as.matrix(vcov(fit.main[4][[1]]))
cov_matrix <- cov_matrix[!is.na(cov_matrix)]
cov_matrix <- matrix(cov_matrix, length(means))

predicted_matrix <- mvrnorm(5000, mu = means, Sigma = cov_matrix) 
colnames(predicted_matrix) <- c(names(fit.main[4][[1]]$coefficients)[1:6], 
                                names(fit.main[4][[1]]$coefficients)[8:27])


### obtain mean legislador

pan_ssd <- data_reg %>%
  summarise(intercept = 1, 
            type = 1, 
            reeleccion = 0, 
            legis61 = 1, 
            legis62 = 0, 
            legis63 = 0, 
            morena = 0, 
            pan = 0, 
            prd = 1, 
            pri= 0, 
            pvem = 0, 
            small = 0, 
            gender = round(mean(female), 0), 
            grado_escolaridad = round(mean(grado_escolaridad), 0), 
            exp_pol = round(mean(exp_pol), 0), 
            exp_sec_privado = round(mean(exp_sec_privado), 0), 
            admon_publica = round(mean(admon_publica), 0), 
            asoc_civil = round(mean(asoc_civil), 0), 
            docencia = round(mean(docencia), 0), 
            ssd_reelec = 0, 
            ssd_morena = 0, 
            ssd_pan = 0, 
            ssd_prd = 1, 
            ssd_pri = 0, 
            ssd_pvem = 0, 
            ssd_small = 0) %>%
  as.matrix() 
pan_ssd <- t(pan_ssd)
colnames(pan_ssd)  <- c('ssd_pan')

pan_rp <- data_reg %>%
  summarise(intercept = 1, 
            type = 0, 
            reeleccion = 0, 
            legis61 = 1, 
            legis62 = 0, 
            legis63 = 0, 
            morena = 0, 
            pan = 0, 
            prd = 1, 
            pri= 0, 
            pvem = 0, 
            small = 0, 
            gender = round(mean(female), 0), 
            grado_escolaridad = round(mean(grado_escolaridad), 0), 
            exp_pol = round(mean(exp_pol), 0), 
            exp_sec_privado = round(mean(exp_sec_privado), 0), 
            admon_publica = round(mean(admon_publica), 0), 
            asoc_civil = round(mean(asoc_civil), 0), 
            docencia = round(mean(docencia), 0), 
            ssd_reelec = 0, 
            ssd_morena = 0, 
            ssd_pan = 0, 
            ssd_prd = 0, 
            ssd_pri = 0, 
            ssd_pvem = 0, 
            ssd_small = 0) %>%
  as.matrix() 
pan_rp <- t(pan_rp)
colnames(pan_rp)  <- c('pr_pan')


####
predicted_ssd <- (predicted_matrix %*% pan_ssd)[,'ssd_pan']
predicted_pr <- (predicted_matrix %*% pan_rp)[,'pr_pan']

####
sim_data <- cbind(cbind(predicted_matrix,predicted_ssd ), 
                  predicted_pr) 



df_predicted <- as_tibble(sim_data) %>%
  dplyr::select('predicted_ssd','predicted_pr') %>%
  # mutate(ideal_party = subset(subset(ideal_parties, main_parties == 'pan'), legislatura == 60)$ideal_party,
  #        ideal_ssd = ideal_party + predicted_ssd, 
  #        ideal_pr = ideal_party + predicted_pr) %>%
  #select(ideal_ssd, ideal_pr) %>%
  gather(predicted_ssd:predicted_pr, 
         key  ='legislador', 
         value = 'ideal') %>%
  mutate(type_legis = ifelse(legislador == 'predicted_ssd', 'SMD', 'PR'))

df_predicted %>%
  group_by(legislador) %>%
  summarise(mean(ideal))

colors <- viridis(2)
ggplot(df_predicted, aes(y = legislador, x = ideal)) +
  stat_halfeye(.width = c(0.975,.75), color = colors[1], #relative_scale = 0.8,
               fatten_point = 0.8)  +
  theme_bw() +
  #facet_grid(rows = vars(reelection), scales = "free") +
  labs(x = "Distance with Party", y = "Type Legislator")  +
  theme(text = element_text(size=rel(5),family="serif"), 
        panel.border = element_blank()
  ) 
# +
#   ggsave(width = 7, 
#          height = 9, 
#          '04-results/predicted_distance_type.pdf')



