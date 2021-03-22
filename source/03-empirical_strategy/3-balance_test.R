

################
### Balance Test
################

setwd(dir = '~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/')


list.packages <- c('tidyverse', 
                   'plm', 
                   'stats')

lapply(list.packages,
       require,
       character.only = TRUE)

load('03-empirical_strategy/1-input_reg/data_reg.Rda', verbose = TRUE)

dep.var <- c('as.factor(ssd_legis)')
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



formula.main <- as.formula(paste0(dep.var,  ## variable dependiente (productividad, disciplina partidista: voting, speech)
                                  '~', 
                                  paste(controls.1, ## covariates individual legislator
                                        collapse = "+")))

reg_balance <- lm(formula.main, 
                    data = data_reg)

stargazer::stargazer(reg_balance, 
                     no.space = TRUE, 
                     dep.var.caption = c('SMD'),
                     covariate.labels = c('female',
                                          'education',
                                          'political experience',
                                          'exp. private sector',
                                          'exp. public administration',
                                          'exp. non-profit org.',
                                          'academic experience'))

