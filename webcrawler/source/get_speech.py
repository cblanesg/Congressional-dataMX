import pandas as pd
import requests
import uuid
import re
import datetime
from datetime import date
from bs4 import BeautifulSoup


def get_date_speech(year_session, link_speech, text_day):
    day_speech = re.findall('[0-9]+', text_day)[0]
    month_speech = month_to_number(link_speech.split('/')[-2:][0])
    return(datetime.date(int(year_session), int(month_speech), int(day_speech)))
def month_to_number(m):
    if m == 'ene':
        return(1)
    elif m == 'feb':
        return(2)
    elif m == 'mar':
        return(3)
    elif m == 'abr':
        return(4)
    elif m == 'may':
        return(5)
    elif m == 'jun':
        return(6)
    elif m == 'jul':
        return(7)
    elif m == 'ago':
        return(8)
    elif m == 'sep':
        return(9)
    elif m == 'oct':
        return(10)
    elif m == 'nov':
        return(11)
    elif m == 'dic':
        return(12)
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
def obtain_texto_debate(link):
    r = requests.get(link)
    soup = BeautifulSoup(r.content, 'html.parser')
    contenido_debate = soup.findAll("div", {"class": "Contenido"})[0].text

    dict_contenidos = {}
    for i in contenido_debate.split('\n\n\n \n'):
        try:
            name_tema = re.sub('\n', '', re.findall('^[A-Z].*\n',i)[0])
            text_all = re.sub(name_tema, '', i)
            text_all = re.sub('[ ]+$', '', re.sub('^[ ]+', '', re.sub('[ ]+', ' ', re.sub('\n+', ' ', text_all))))
            dict_contenidos[name_tema] = text_all
        except:
            continue
    dict_contenidos['id_debate'] = str(uuid.uuid3(uuid.NAMESPACE_URL,link))
    #main_text = []
    #for x in dict_contenidos.values():
    #    main_text.append(x)
    #return(re.sub('\n', ' ', ' '.join(main_text)))
    print(len(dict_contenidos.keys()))
    return(dict_contenidos)
def obtain_text_sesion(link):
    r = requests.get(link)
    soup = BeautifulSoup(r.content, 'html.parser')

    try:
        links_volumenes = []
        for h in soup.findAll('a'):
            if any(re.findall('Volúmen', h.text)):
                links_volumenes.append(h.get('href'))

        real_link = '/'.join(link.split('/')[:-1]) + '/' + links_volumenes[0]

        texto_debate = obtain_texto_debate(real_link)
        return(texto_debate)
    except:
        return(obtain_texto_debate(link))
