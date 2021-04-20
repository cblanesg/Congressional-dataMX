rm(list=ls()) ## clean enviornment


setwd(dir = '~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/')


list.packages <- c('tidyverse', 
                   'plm', 
                   'stats')

lapply(list.packages,
       require,
       character.only = TRUE)


### Load data

load('03-empirical_strategy/1-input_reg/data_reg.Rda', verbose = TRUE)

data_num_members <- data_reg %>%
  mutate(partido = ifelse(partido %in% c('pve', 'partido verde ecologista'), 'pvem',
                          ifelse(partido == 'morenal', 'morena',
                                 ifelse(partido == 'encuentro social', 'es',
                                        ifelse(partido == 'sin partido', 'sp',
                                               ifelse(partido == 'movimiento ciudadano', 'mc', partido)))))) %>%
  group_by(legislatura, partido) %>%
  count() %>%
  group_by(legislatura) %>%
  mutate(mean_members = mean(n),
         party_size = ifelse(n >= mean_members, 1, 0)) %>%
  select(legislatura, partido, party_size)


data_reg <- data_reg  %>%
  mutate(partido = ifelse(partido %in% c('pve', 'partido verde ecologista'), 'pvem',
                          ifelse(partido == 'morenal', 'morena',
                                 ifelse(partido == 'encuentro social', 'es',
                                        ifelse(partido == 'sin partido', 'sp',
                                               ifelse(partido == 'movimiento ciudadano', 'mc', partido)))))) %>%
  mutate(partido = ifelse(nombre_completo == 'Morales Vázquez Carlos Alberto', 'prd',
                          ifelse(nombre_completo == 'Pérez Rivera Evaristo Lenin', 'pan',
                                 ifelse(nombre_completo == 'Riojas Martínez Ana Lucia', 'sp', partido)))) %>%
  left_join(data_num_members, by = c('legislatura', 'partido'))

### Specification

dep.var <- c('pct_attendance', 
             'number_committees',
             'count_speech', 
             'number_bills')

group.controls <- 'as.factor(ssd_legis)'
reelection.treatment <- 'as.factor(reelection_dummy)'
control.party = 'as.factor(party_size)'

interaction.cov <- paste0('as.factor(ssd_legis)*', 
                          'as.factor(reelection_dummy)')

control.congress <- 'as.factor(legislatura)'
controls.1 <- c('as.factor(female)', ## male, female
                'grado_escolaridad', ## 1 basica, 2 media superior, 3 superior, 0 no data
                'exp_pol',## 1 if legislator have previous political experience  (political party, cargo eleccion popular)
                'exp_sec_privado', ##  experience in private sector
                'admon_publica', ##  experience in public administration
                'asoc_civil', ##  experience in asociacion civil
                'docencia' #, ##  experience in docencia
                # 'carrera_deportiva' ##  experience in sports
)

fit.main <- coef.main <- se.cse.disaggregated.1 <- vector("list", length(dep.var))

for(i in 1:length(dep.var)){
  formula.main <- as.formula(paste0(dep.var[i],  ## variable dependiente (productividad, disciplina partidista: voting, speech)
                                    '~', 
                                    group.controls, ##  MR o PR
                                    '+', 
                                    reelection.treatment, ## posibilidad de reeleccion 
                                    '+', 
                                    interaction.cov, ## interaction tipo y periodo
                                    '+',
                                    control.congress, ## FE by congress
                                    '+',
                                    control.party, ## FE by congress
                                    '+',
                                    paste(controls.1, ## covariates individual legislator
                                          collapse = "+")))
  
  fit.main[[i]] <- lm(formula.main, 
                      data = data_reg)
  
}

fit.main2 <- coef.main2 <- se.cse.disaggregated.2 <- vector("list", length(dep.var))

for(i in 1:length(dep.var)){
  formula.main <- as.formula(paste0(dep.var[i],  ## variable dependiente (productividad, disciplina partidista: voting, speech)
                                    '~', 
                                    group.controls, ##  MR o PR
                                    '+', 
                                    reelection.treatment, ## posibilidad de reeleccion 
                                    '+', 
                                    interaction.cov, ## interaction tipo y periodo
                                    '+',
                                    control.congress, ## FE by congress
                                    '+',
                                    # control.party, ## FE by congress
                                    # '+',
                                    paste(controls.1, ## covariates individual legislator
                                          collapse = "+")))
  
  fit.main2[[i]] <- lm(formula.main, 
                      data = data_reg)
  
}

stargazer::stargazer(fit.main[1], 
                     fit.main2[1], 
                     fit.main[2], 
                     fit.main2[2], 
                     fit.main[3], 
                     fit.main2[3], 
                     fit.main[4],
                     fit.main2[4], 
                     omit = 'legislatura',
                     add.lines = list(c('\\textbf{Congress controls}', rep('Yes', 4))), 
                     omit.stat = c("ser", "rsq","f"),
                     covariate.labels = c('SMD',
                                          'reelection',
                                          'party size',
                                          'female',
                                          'education',
                                          'political experience',
                                          'exp. private sector',
                                          'exp. public administration',
                                          'exp. non-profit org.',
                                          'academic experience',
                                          'SMD x reelection'),
                     dep.var.labels  = c('pct attendance', 
                                         'committees', 
                                         'speech in floor', 
                                         'bills submitted'),
                     notes = c("Standard Errors in parentheses")
                     #,type = 'text'
                     )
