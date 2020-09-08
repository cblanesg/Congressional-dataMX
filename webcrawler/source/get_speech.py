import pandas as pd
import requests
import re
from bs4 import BeautifulSoup

def get_links_legislaturas(legislaturas):
    """
    Parametros:
    -----------
    legislatura: list of int numbers of legislatura

    Returns:
    --------
    lista de diccionario con los a el htmls de los debates legislativos
    """
    all_data = []

    for legislatura in legislaturas:
        link_debates = 'http://cronica.diputados.gob.mx/DDebates/' + str(legislatura) + '/index.html'

        r = requests.get(link_debates)
        soup = BeautifulSoup(r.content, 'html.parser')

        links_sesiones = []
        type_sesion = []
        urls = []
        for h in soup.findAll('a'):
            links_sesiones.append('http://cronica.diputados.gob.mx/DDebates/' + str(legislatura) + '/' + h.get('href'))
            type_sesion.append(re.sub('\xa0', '', h.text))
            urls.append(h.get('href'))


        for x, y, urls in zip(links_sesiones, type_sesion, urls):
            r = requests.get(x)
            soup = BeautifulSoup(r.content, 'html.parser')

            for i in soup.findAll('a'):
                year_session = re.findall('[0-9]{4}', soup.findAll('tbody')[1].find('tr').text)[0]
                link_ = 'http://cronica.diputados.gob.mx/DDebates/' + str(legislatura)+'/'+ re.sub('index[.]html', '', urls) +i.get('href')

                fecha_speech = get_date_speech(year_session, link_, i.text)

                temp = {}
                temp['id_debate'] = str(uuid.uuid3(uuid.NAMESPACE_URL,link_))
                temp['legislatura'] = legislatura
                temp['date_debate'] = fecha_speech
                temp['type_sesion'] = y
                temp['link_debate'] = link_
                all_data.append(temp)
    return(all_data)
