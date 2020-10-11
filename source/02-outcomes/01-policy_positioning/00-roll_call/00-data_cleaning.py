import pandas as pd
import json
import re
from tqdm import tqdm

with open('../../../../data/01-collection_data/06-aggregated-data/roll_call_votes.json') as d:
    votes = json.load(d)

with open('../../../../data/01-collection_data/06-aggregated-data/id_data.json') as d:
    id_data = json.load(d)

id_prop = []
for i in id_data:
    if i['legislatura'] == 64 and i['suplente_propietario'] == 'P':
        id_prop.append(i['id_legislador'])

len(id_prop)

votes61 = []
for i in votes:
    if i['legislatura'] == 64 and i['id'] in id_prop:
        votes61.append(i)

len(pd.DataFrame(votes61).titulo_votacion.unique())

bills = pd.DataFrame(votes61).titulo_votacion.unique()

### Complete Data

df = pd.DataFrame(votes61)

dict_votes = []
for i in tqdm(bills):
    for x in id_prop:
        temp = {}
        temp['id'] = x
        temp['bill'] = i

        subset_legis = df[df['id'] == x]
        try:
            voto = list(subset_legis[subset_legis['titulo_votacion'] == i].voto)[0]
            temp['vote'] = voto
            dict_votes.append(temp)
        except:
            temp['vote'] = 'missing'
            dict_votes.append(temp)

party_df = df[['id', 'party']].drop_duplicates()

df_id = pd.DataFrame(id_data)[['id_legislador', 'partido']].drop_duplicates()

data_all = pd.merge(left = pd.DataFrame(dict_votes),
        right = df_id,
        left_on = ['id'],
        right_on = ['id_legislador'],
        how = 'left')

data_all = data_all[['id_legislador', 'bill', 'vote', 'partido']]

data_all.to_csv('../../../../data/02-outcomes/01-policy_positioning/00-roll_call/00-prep_data/roll_call_votes64.csv', encoding = 'utf-8 sig')
