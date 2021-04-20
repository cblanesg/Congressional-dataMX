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

load('03-empirical_strategy/2-reg_results/reg_main.Rda')
load('03-empirical_strategy/1-input_reg/data_reg.Rda', verbose = TRUE)
load('02-outcomes/01-policy_positioning/01-text_analysis/wordfish/05_party-discipline/wordfish_party_discipline.Rda', verbose = TRUE)

ideal_parties <- discipline_data %>%
  select(inc_party, legislatura, estimated_theta) %>%
  distinct() %>%
  mutate(main_parties = ifelse(inc_party == 'Partido Acción Nacional', 'pan', inc_party)) %>%
  dplyr::group_by(inc_party, legislatura) %>%
  dplyr::summarise(ideal_party = median(estimated_theta, na.rm = TRUE)) %>%
  ungroup() %>%
  select(legislatura, main_parties, ideal_party)


### simulations

means <- as.matrix(coef(fit.main1.2[3][[1]]))
means <- means[!is.na(means)]
cov_matrix <- as.matrix(vcov(fit.main1.2[3][[1]]))
cov_matrix <- cov_matrix[!is.na(cov_matrix)]
cov_matrix <- matrix(cov_matrix, length(means))

predicted_matrix <- mvrnorm(5000, mu = means, Sigma = cov_matrix) 
colnames(predicted_matrix) <- c(names(fit.main1.2[3][[1]]$coefficients)[1:6], 
                                names(fit.main1.2[3][[1]]$coefficients)[8:15])


### obtain mean legislador

vector_characteristics_ssd <- data_reg %>%
  summarise(intercept = 1, 
            type = 1, 
            reeleccion = 0, 
            legis61 = 0, 
            legis62 = 0, 
            legis63 = 0, 
            gender = round(mean(female), 0), 
            grado_escolaridad = round(mean(grado_escolaridad), 0), 
            exp_pol = round(mean(exp_pol), 0), 
            exp_sec_privado = round(mean(exp_sec_privado), 0), 
            admon_publica = round(mean(admon_publica), 0), 
            asoc_civil = round(mean(asoc_civil), 0), 
            docencia = round(mean(docencia), 0), 
            ssd_reelec = 0) %>%
  as.matrix() 
legis_ssd <- t(vector_characteristics_ssd)
colnames(legis_ssd)  <- c('ssd')

vector_characteristics_rp<- data_reg %>%
  summarise(intercept = 1, 
            type = 0, 
            reeleccion = 0, 
            legis61 = 0, 
            legis62 = 0, 
            legis63 = 0, 
            gender = round(mean(female), 0), 
            grado_escolaridad = round(mean(grado_escolaridad), 0), 
            exp_pol = round(mean(exp_pol), 0), 
            exp_sec_privado = round(mean(exp_sec_privado), 0), 
            admon_publica = round(mean(admon_publica), 0), 
            asoc_civil = round(mean(asoc_civil), 0), 
            docencia = round(mean(docencia), 0), 
            ssd_reelec = 0) %>%
  as.matrix() 
legis_rp <- t(vector_characteristics_rp)
colnames(legis_rp)  <- c('rp')


vector_characteristics_ssd_reelec <- data_reg %>%
  summarise(intercept = 1, 
            type = 1, 
            reeleccion = 1, 
            legis61 = 0, 
            legis62 = 0, 
            legis63 = 0, 
            gender = round(mean(female), 0), 
            grado_escolaridad = round(mean(grado_escolaridad), 0), 
            exp_pol = round(mean(exp_pol), 0), 
            exp_sec_privado = round(mean(exp_sec_privado), 0), 
            admon_publica = round(mean(admon_publica), 0), 
            asoc_civil = round(mean(asoc_civil), 0), 
            docencia = round(mean(docencia), 0), 
            ssd_reelec = 1) %>%
  as.matrix() 
legis_ssd_reelec <- t(vector_characteristics_ssd_reelec)
colnames(legis_ssd_reelec)  <- c('ssd_reelec')

vector_characteristics_rp_reelec<- data_reg %>%
  summarise(intercept = 1, 
            type = 0, 
            reeleccion = 1, 
            legis61 = 0, 
            legis62 = 0, 
            legis63 = 0, 
            gender = round(mean(female), 0), 
            grado_escolaridad = round(mean(grado_escolaridad), 0), 
            exp_pol = round(mean(exp_pol), 0), 
            exp_sec_privado = round(mean(exp_sec_privado), 0), 
            admon_publica = round(mean(admon_publica), 0), 
            asoc_civil = round(mean(asoc_civil), 0), 
            docencia = round(mean(docencia), 0), 
            ssd_reelec = 0) %>%
  as.matrix() 
legis_rp_reelec <- t(vector_characteristics_rp_reelec)
colnames(legis_rp_reelec)  <- c('rp_reelec')


####
predicted_ssd <- (predicted_matrix %*% legis_ssd)[,'ssd']
predicted_pr <- (predicted_matrix %*% legis_rp)[,'rp']
predicted_ssd_reelec <- (predicted_matrix %*% legis_ssd_reelec)[,'ssd_reelec']
predicted_pr_reelec <- (predicted_matrix %*% legis_rp_reelec)[,'rp_reelec']

####
sim_data <- cbind(cbind(predicted_matrix,predicted_ssd ), 
      predicted_pr) %>%
  cbind(predicted_ssd_reelec)%>%
  cbind(predicted_pr_reelec)
colnames(as_tibble(sim_data))

df_predicted <- as_tibble(sim_data) %>%
  dplyr::select('predicted_ssd','predicted_pr', 
                'predicted_ssd_reelec', 
                'predicted_pr_reelec') %>%
  gather(predicted_ssd:predicted_pr_reelec, 
         key  ='legislador', 
         value = 'predicted_distance') %>%
  mutate(type_legis = ifelse(legislador == 'predicted_ssd', 'SMD', 
                             ifelse(legislador == 'predicted_ssd_reelec', 'SMD', 'PR' )), 
         reelection = ifelse(legislador == 'predicted_ssd_reelec', 'Reelection', 
                             ifelse(legislador == 'predicted_pr_reelec', 'Reelection', 'No-Reelection')))

colors <- viridis(2)
ggplot(df_predicted, aes(y = type_legis, x = predicted_distance)) +
  stat_halfeye(.width = c(0.975,.75), color = colors[1], #relative_scale = 0.8,
               fatten_point = 0.8)  +
  theme_bw() +
  facet_grid(rows = vars(reelection), scales = "free") +
  labs(x = "Distance with Party", y = "Type Legislator")  +
  theme(text = element_text(size=rel(5),family="serif"), 
        panel.border = element_blank()
        ) +
  ggsave(width = 7, 
         height = 9, 
         '04-results/predicted_distance_type.pdf')



