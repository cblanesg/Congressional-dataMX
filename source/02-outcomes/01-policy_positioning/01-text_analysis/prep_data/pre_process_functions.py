import spacy ## Lemmatizer
import re
from unidecode import unidecode
nlp_lemm = spacy.load("es_core_news_sm")

from spacy.lang.es import Spanish ## StopWords
nlp = Spanish()

from spacy.lang.es.stop_words import STOP_WORDS

def remove_stopwords(text):
    my_doc = nlp(text)

    token_list = []
    for token in my_doc:
        token_list.append(token.text)

    filtered_sentence =[]

    for word in token_list:
        lexeme = nlp.vocab[word]
        if lexeme.is_stop == False:
            filtered_sentence.append(word)

    full_text = ' '.join(filtered_sentence)
    full_text = re.sub('[ ]+', ' ', full_text)
    return(full_text)
def apply_Lemmatization(text):
    my_doc = nlp_lemm(text)

    lemma_word1 = []
    for token in my_doc:
        lemma_word1.append(token.lemma_)

    full_text = ' '.join(lemma_word1)
    full_text = re.sub('[ ]+', ' ', full_text)
    return(full_text)

def remove_punctuation(text):
    clean = re.sub(r'[^\w\s]', '', text)
    return(clean)

def remove_numbers(text):
    text = re.sub(r"\b\d+|\d+\b", "", text)
    text = re.sub('[0-9]+', '', text)
    return(text)

def strip_Whitespace(text):
    text = re.sub('[ ]+', ' ', text)
    text = re.sub('^[ ]+', '', text)
    text = re.sub('[ ]+$', '', text)
    return(text)
def remove_accents(text):
    text = unidecode(text)
    return(text)
def apply_lowercase(text):
    text = text.lower()
    return(text)

def pre_processText(text):
    clean = remove_accents(text)
    clean = apply_lowercase(clean)
    clean = remove_numbers(clean)
    clean = remove_punctuation(clean)
    clean = remove_numbers(clean)
    clean = remove_accents(clean)
    clean = strip_Whitespace(clean)
    clean = remove_stopwords(clean)
    clean = apply_Lemmatization(clean)
    return(clean)

def last_cleaning(text):
    clean = re.sub('\t', '', text)
    clean = re.sub('[0-9]+', "", clean)
    clean = re.sub(r"\b\d+|\d+\b", "", clean)
    clean = re.sub('ó', 'o', clean)
    clean = re.sub('í', 'i', clean)
    clean = re.sub('[-]', '', clean)
    clean = re.sub('-', '', clean)
    clean = re.sub('_', '', clean)
    clean = re.sub('[.]', '', clean)
    clean = remove_accents(clean)
    return(clean)
