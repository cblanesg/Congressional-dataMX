library(tidyverse)
library(stargazer)
library(ggplot2)
library(wnominate)
library(pscl)
library(oc)
library(data.table)

setwd('~/cam.blanes Dropbox/Camila Blanes/Congressional-dataMX/data/01-collection_data/')


id_data <- read_csv('06-aggregated-data/id_data.csv') %>%
  select(-c(X1))
colnames(id_data)

id_data <- id_data %>%
  filter(suplente_propietario == 'P') %>%
  mutate(reform = ifelse(legislatura < 64, 'pre', 'post')) %>%
  select(reform, legislatura, id_legislador, tipo_legislador)

df <- read_csv(file ='../02-outcomes/01-policy_positioning/00-roll_call/00-prep_data/roll_call_votes63.csv') %>%
  select(-c(X1)) %>%
  mutate(voto_num = ifelse(vote == 'A favor', '1', 
                           ifelse(vote == 'En contra', '6', 
                                  ifelse(vote == 'missing', NA, '9'))), 
         legis = 62) %>%
  select(legis, id_legislador, partido, voto_num, bill)

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
               notInLegis = NA,
               legis.names = LEGnames, legis.data = legData,
               desc = "Mexican Deputies Chamber")

result <- oc(rc,polarity=1,dims=1)
df_results <- result[["legislators"]]
df_results <- setDT(df_results, keep.rownames = TRUE)[]

plot(result)


df_results <- result[["legislators"]]
df_results <- setDT(df_results, keep.rownames = TRUE)[]
length(LEGnames)

df_results$id <- LEGnames
legis_max_val <- df_results %>%
  filter(party == 'Partido Accion Nacional') %>%
  filter(coord1D == max(coord1D, na.rm = TRUE) ) %>%
  select(id)
legislator1 <- legis_max_val$id[1]
unique(df_results$party)


legis_max_val2 <- df_results %>%
  filter(party == 'Partido de la Revolucion Democratica') %>%
  filter(coord1D == max(coord1D, na.rm = TRUE) ) %>%
  select(id)
legislator2 <- '4dc80e02-fe1c-11ea-95ca-acde48001122'

#######
# IDEAL
#######
cl2 <- constrain.legis(rc, x=list("f1daeedc-fd00-11ea-83d8-acde48001122"=c(-1,0),
                                    "08a459c8-fd01-11ea-83d8-acde48001122"=c(1,0),
                                    "f1daeedc-fd00-11ea-83d8-acde48001122"=c(0,1)), d=2)
cl <- constrain.legis(rc,
                      x=list('f1daeedc-fd00-11ea-83d8-acde48001122' = -1, 
                             '08a459c8-fd01-11ea-83d8-acde48001122' = 1),
                      d=1)

id1 <- ideal(rc, d = 2, priors = cl2, startvals = cl2)

df_party <- df %>%
  select(id_legislador, partido) %>%
  distinct()


df_betas <- as.data.frame(id1$xbar)
df_results <- setDT(df_betas, keep.rownames = TRUE)[]

df_results2 <- df_results %>%
  left_join(df_party, by = c('rn' = 'id_legislador')) %>%
  mutate(rank = min_rank(D1)) %>%
  arrange(rank)

ggplot(df_results2, aes(x = D1,  y = D2, colour = partido)) +
  geom_point()



res <- wnominate(rc, polarity = c('f1daeedc-fd00-11ea-83d8-acde48001122', 
                           'f1daeedc-fd00-11ea-83d8-acde48001122'), dim = 2)

df_results <- res$legislators
df_results$party


ggplot(df_results, aes(x = coord1D, y = coord2D, colour = party)) +
  geom_point()

plot(res)


