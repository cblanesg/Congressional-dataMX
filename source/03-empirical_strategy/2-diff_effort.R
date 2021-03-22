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

dep.var <- c('pct_attendance', 
             'number_committees',
             'count_speech', 
             'number_bills')

group.controls <- 'as.factor(ssd_legis)'
reelection.treatment <- 'as.factor(reelection_dummy)'
control.party = 'main_parties_raw'

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

stargazer::stargazer(fit.main[1], 
                     fit.main[2], 
                     fit.main[3], 
                     fit.main[4],
                     omit = 'legislatura|main_parties',
                     add.lines = list(c('\\textbf{Congress controls}', rep('Yes', 4)), 
                                      c('\\textbf{Party controls}', rep('Yes', 4))), 
                     omit.stat = c("ser", "rsq","f"),
                     covariate.labels = c('SMD', 
                                          'reelection', 
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
