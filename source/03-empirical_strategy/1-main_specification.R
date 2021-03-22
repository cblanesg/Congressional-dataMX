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

### Specification
dep.var <- c('index_effort', 
             'discipline_vote',
             'discipline_speech')

group.controls <- 'as.factor(ssd_legis)'
reelection.treatment <- 'as.factor(reelection_dummy)'
control.party = 'as.factor(main_parties)'

# data_reg <- data_reg %>%
#   mutate(main_parties = ifelse(main_parties != "small parties", 1, 0))

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
fit.main1.2 <- coef.main1.2 <- se.cse.disaggregated1.2 <- vector("list", length(dep.var))

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
                                     control.party, ## FE by party
                                     '+',
                                    paste(controls.1, ## covariates individual legislator
                                          collapse = "+")))
  
  fit.main[[i]] <- lm(formula.main, 
                   data = data_reg)
  
}

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
                                    # control.party, ## FE by party
                                    # '+',
                                    paste(controls.1, ## covariates individual legislator
                                          collapse = "+")))
  
  fit.main1.2[[i]] <- lm(formula.main, 
                      data = data_reg)
  
}

stargazer::stargazer(fit.main[1], 
                     fit.main[2], 
                     fit.main[3], 
                     type = 'text')

stargazer::stargazer(fit.main[1], 
                     fit.main1.2[1], 
                     fit.main[2], 
                     fit.main1.2[2], 
                     fit.main[3],
                     fit.main1.2[3], 
                     omit = 'legislatura',
                     add.lines = list(c('\\textbf{Congress controls}', rep('Yes', 6)), 
                                      c('\\textbf{Party controls}', 'Yes', 'No','Yes', 'No','Yes', 'No')), 
                     omit.stat = c("ser", "rsq","f"),
                     covariate.labels = c('SMD',
                                          'reelection',
                                          'main party',
                                          'female',
                                          'education',
                                          'political experience',
                                          'exp. private sector',
                                          'exp. public administration',
                                          'exp. non-profit org.',
                                          'academic experience',
                                          'SMD x reelection'),
                     dep.var.labels  = c('legislative effort', 
                                        'distance w/party in votes', 
                                        'distance w/party in speech'),
                     notes = c("Standard Errors in parentheses")
                     #,type = 'text'
                     )


##### Same specification with type of PR legislators

load('03-empirical_strategy/1-input_reg/data_reg.Rda', verbose = TRUE)

group.controls2 <- 'type_legislador'
interaction.cov2 <- paste0('type_legislador*', 
                          'as.factor(reelection_dummy)')

fit.main2 <- coef.main2 <- se.cse.disaggregated.2 <- vector("list", length(dep.var))

for(i in 1:length(dep.var)){
  formula.main <- as.formula(paste0(dep.var[i],  ## variable dependiente (productividad, disciplina partidista: voting, speech)
                                    '~', 
                                    group.controls2, ##  MR o PR
                                    '+', 
                                    reelection.treatment, ## posibilidad de reeleccion 
                                    '+', 
                                    interaction.cov2, ## interaction tipo y periodo
                                    '+',
                                    control.congress, ## FE by congress
                                     '+',
                                    control.party, ## FE by congress
                                    '+',
                                    paste(controls.1, ## covariates individual legislator
                                          collapse = "+")))
  
  fit.main2[[i]] <- lm(formula.main, 
                      data = data_reg)
  
}

stargazer::stargazer(fit.main2[1], 
                     fit.main2[2], 
                     fit.main2[3],
                     omit = 'legislatura|main_parties',
                     add.lines = list(c('\\textbf{Congress controls}', rep('Yes', 3)), 
                                      c('\\textbf{Party controls}', rep('Yes', 3))), 
                     omit.stat = c("ser", "rsq","f"),
                     covariate.labels = c('SMD', 
                                          'Top PR', 
                                          'reelection', 
                                          'female', 
                                          'education', 
                                          'political experience', 
                                          'exp. private sector', 
                                          'exp. public administration', 
                                          'exp. non-profit org.', 
                                          'academic experience', 
                                          'SMD x reelection',
                                          'Top PR x reelection'),
                     dep.var.labels  = c('legislative effort', 
                                         'distance w/party in votes', 
                                         'distance w/party in speech'),
                     notes = c("Standard Errors in parentheses")
                     #,type = 'text'
                     )


##### Same specification with type of SSD legislators
dep.var <- c('index_effort', 
             'discipline_vote',
             'discipline_speech')
type.ssd <- 'type_ssd'
interaction.cov2 <- paste0('type_ssd*', 
                           'as.factor(reelection_dummy)')

fit.main2 <- coef.main2 <- se.cse.disaggregated.2 <- vector("list", length(dep.var))

for(i in 1:length(dep.var)){
  formula.main <- as.formula(paste0(dep.var[i],  ## variable dependiente (productividad, disciplina partidista: voting, speech)
                                    '~', 
                                    type.ssd, ##  MR o PR
                                    '+', 
                                    reelection.treatment, ## posibilidad de reeleccion 
                                    '+', 
                                    interaction.cov2, ## interaction tipo y periodo
                                    '+',
                                    control.congress, ## FE by congress
                                    '+',
                                    control.party, ## FE by congress
                                    '+',
                                    paste(controls.1, ## covariates individual legislator
                                          collapse = "+")))
  
  fit.main2[[i]] <- lm(formula.main, 
                       data = data_reg)
  
}

stargazer::stargazer(fit.main2[1], 
                     fit.main2[2], 
                     fit.main2[3], 
                     type = 'text')

stargazer::stargazer(fit.main2[1], 
                     fit.main2[2], 
                     fit.main2[3],
                     omit = 'legislatura',
                     add.lines = list(c('\\textbf{Congress controls}', rep('Yes', 3)), 
                                      c('\\textbf{Party controls}', rep('Yes', 3))), 
                     omit.stat = c("ser", "rsq","f"),
                     covariate.labels = c('SMD loose election', 
                                          'PR', 
                                          'reelection', 
                                          'main party',
                                          'female', 
                                          'education', 
                                          'political experience', 
                                          'exp. private sector', 
                                          'exp. public administration', 
                                          'exp. non-profit org.', 
                                          'academic experience', 
                                          'SMD loose election x reelection',
                                          'PR x reelection'),
                     dep.var.labels  = c('legislative effort', 
                                         'distance w/party in votes', 
                                         'distance w/party in speech'),
                     notes = c("Standard Errors in parentheses")
                     #,type = 'text'
)



###### Main specification with party controls

##### 

dep.var <- c('index_effort', 
             'discipline_vote',
             'discipline_speech')

group.controls <- 'ssd_legis'
interaction.cov <- paste0('ssd_legis*', 
                           'as.factor(reelection_dummy)')
party.contols <- 'main_parties'

party.contols_interaction <- 'main_parties*ssd_legis'
reelection.treatment <- 'as.factor(reelection_dummy)'
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

fit.main3 <- coef.main3 <- se.cse.disaggregated.3 <- vector("list", length(dep.var))

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
                                    party.contols,
                                    '+',
                                    party.contols_interaction,
                                    '+',
                                    paste(controls.1, ## covariates individual legislator
                                          collapse = "+")))
  
  fit.main3[[i]] <- lm(formula.main, 
                       data = data_reg)
  
}

stargazer::stargazer(fit.main3[1], 
                     fit.main3[2],
                     fit.main3[3], type = 'text')

stargazer::stargazer(fit.main3[1], 
                     fit.main3[2],
                     fit.main3[3],
                     no.space = TRUE,
                     omit = 'legislatura',
                     add.lines = list(c('\\textbf{Congress controls}', rep('Yes', 3)), 
                                      c('\\textbf{Party controls}', rep('Yes', 3))), 
                     omit.stat = c("ser", "rsq","f"),
                     covariate.labels = c('SMD',
                                          'reelection',
                                          'morena',
                                          'pan',
                                          'prd',
                                          'pri',
                                          'pvem',
                                          'small parties',
                                          'female',
                                          'education',
                                          'political experience',
                                          'exp. private sector',
                                          'exp. public administration',
                                          'exp. non-profit org.',
                                          'academic experience',
                                          'SMD x reelection',
                                          'morena x reelection',
                                          'pan x reelection',
                                          'prd x reelection',
                                          'pri x reelection',
                                          'pvem x reelection',
                                          'small parties x reelection'),
                     dep.var.labels  = c('legislative effort', 
                                         'distance w/party in votes', 
                                         'distance w/party in speech'),
                     notes = c("Standard Errors in parentheses")
                     #,type = 'text'
                     )


