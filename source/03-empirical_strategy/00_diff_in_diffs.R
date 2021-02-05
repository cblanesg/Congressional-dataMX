
setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/')

rm(list=ls())

## Output directory
output <- "data/02-outcomes/03-empirical_strategy/"

lis<t.packages <- c("ggplot2", 
                   'stargazer', 
                   'tidyverse')

lapply(list.packages,
       require,
       character.only = TRUE)

## prepare input data

### party discipline
load(file = 'data/02-outcomes/01-policy_positioning/00-roll_call/03-party_discipline/party_discipline_rollCall.Rda', verbose = TRUE)

prep_data <- df_ideal_points %>%
  mutate(reelection_dummy = ifelse(legis == 64, 1, 0), 
         ssd_legis = ifelse(tipo_legislador == 'Mayoria Relativa', 1, 0))

didreg1 <- lm(abs_dif_with_party ~ reelection_dummy + ssd_legis + reelection_dummy*ssd_legis, data = prep_data)
didreg2 <- lm(PC1 ~ reelection_dummy + ssd_legis + reelection_dummy*ssd_legis, data = data_effort)
data_effort$PC1

stargazer(didreg1, didreg2, type = 'text')

stargazer::stargazer(didreg1, didreg2)

### by outcomes of effort
didreg2 <- lm(PC1 ~ reelection_dummy + ssd_legis + reelection_dummy*ssd_legis, data = data_effort)

colnames(data_effort)
did_effort1 <-  lm(number_committees ~ reelection_dummy + ssd_legis + reelection_dummy*ssd_legis, data = data_effort)
did_effort2 <-  lm(share_speech ~ reelection_dummy + ssd_legis + reelection_dummy*ssd_legis, data = data_speech)
did_effort3 <-  lm(number_bills ~ reelection_dummy + ssd_legis + reelection_dummy*ssd_legis, data = data_effort)
did_effort4 <-  lm(pct_attendance ~ reelection_dummy + ssd_legis + reelection_dummy*ssd_legis, data = data_effort)

stargazer::stargazer(did_effort1, did_effort2, did_effort3, did_effort4)

stargazer::stargazer(didreg2, type= 'text')
