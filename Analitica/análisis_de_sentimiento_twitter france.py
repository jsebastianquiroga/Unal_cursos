# -*- coding: utf-8 -*-
"""Análisis_de_Sentimiento_Twitter.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1hoBvpw2Q3KFg19PsQOYN4RqCqQdz0EIb

# Web Scraping y Análisis de Sentimientos en Python

En esta sesión se abordará el uso web scraping para una de las redes sociales con mayor tendencia a nivel mundial: Twitter. Posteriormente, se procederá a realizar un análisis de sentimientos a la información extraída.

### Instalación e importación de paquetes
"""

!pip install -q snscrape

import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import date

import nltk
nltk.download('vader_lexicon')
nltk.download('punkt')
from nltk.sentiment.vader import SentimentIntensityAnalyzer
from nltk import sentiment
from nltk import word_tokenize
analizador = SentimentIntensityAnalyzer()

"""## Análisis de sentimientos en inglés"""

!pip install --user nltk

"""### Por tema"""

hoy = date.today()
fecha_final = hoy
fecha_inicial = '2023-06-29'
max_resultados = 500

termino_busqueda = 'FranceRiots'

os.system(f"snscrape --since {fecha_inicial} twitter-search '{termino_busqueda} until:{fecha_final}' > result-tweets.txt")
if os.stat("result-tweets.txt").st_size == 0:
  counter = 0
else:
  df = pd.read_csv('result-tweets.txt', names=['link'])
  counter = df.size

print('Número de Tweets : '+ str(counter))

tweets_extraidos = "snscrape --format '{content!r}'"+ f" --max-results {max_resultados} --since {fecha_inicial} twitter-search '{termino_busqueda} until:{fecha_final}' > extracted-tweets.txt"
os.system(tweets_extraidos)

try:
    df = pd.read_csv('extracted-tweets.txt', names=['content'], error_bad_lines=False)
    if df.empty:
        print('No Tweets found')
    else:
        for row in df['content'].iteritems():
            print(row)
except pd.errors.ParserError:
    print('Error: ParserError')

tweets_extraidos = "snscrape --format '{content!r}'"+ f" --max-results {max_resultados} --since {fecha_inicial} twitter-search '{termino_busqueda} until:{fecha_final}' > extracted-tweets.txt"
os.system(tweets_extraidos)

try:
    df = pd.read_csv('extracted-tweets.txt', names=['content'], error_bad_lines=False)
    if df.empty:
        print('No Tweets found')
    else:
        df.to_csv('tweets.csv', index=False)
        print('CSV file saved successfully')
except pd.errors.ParserError:
    print('Error: ParserError')

from google.colab import files

files.download('extracted-tweets.txt')

for i in range(df.shape[0]):
  sentence = df['content'][i]
  print(sentence)
  scores = analizador.polarity_scores(sentence)
  for key in scores:
    print(key, ':', scores[key])
  print()

df['Sentimiento'] = df['content'].apply(lambda i: analizador.polarity_scores(i)['compound'])
df['Negativo'] = df['content'].apply(lambda i: analizador.polarity_scores(i)['neg'])
df['Neutral'] = df['content'].apply(lambda i: analizador.polarity_scores(i)['neu'])
df['Positivo'] = df['content'].apply(lambda i: analizador.polarity_scores(i)['pos'])
df

df[df['Sentimiento'] > 0]

print(df.iloc[df['Sentimiento'].idxmax(),:])
df['content'][421]

print(df.iloc[df['Sentimiento'].idxmin(),:])
df['content'][17]

plt.figure(figsize=(20, 7))
df['Positivo'].plot.area(colormap = 'Greens_r', alpha = 0.5)
df['Negativo'].plot.area(stacked=False, colormap = 'Reds_r', alpha = 0.5)
plt.legend()
plt.show()

df.loc[(df['Sentimiento'] >= 0.2), 'Final'] = 'Positivo'
df.loc[(df['Sentimiento'] < 0.2) & (df['Sentimiento'] > -0.2), 'Final'] = 'Neutral'
df.loc[(df['Sentimiento'] <= -0.2), 'Final'] = 'Negativo'
df

df.to_excel('Tweets.xlsx', index = False)

tabla_frecuencia = df.groupby('Final').agg(frequency=('Final', 'count'))
tabla_frecuencia['Porcentaje'] = (tabla_frecuencia['frequency']/tabla_frecuencia['frequency'].sum())*100
tabla_frecuencia['% Acumulativo'] = tabla_frecuencia['Porcentaje'].cumsum()
tabla_frecuencia

plt.figure(figsize=(10,8))
plt.pie(tabla_frecuencia['frequency'], labels = tabla_frecuencia.index.tolist(), autopct='%.2f%%',
        colors = ['#ad1313', '#1a35a1', '#24a348'], explode = [0.05, 0.05, 0.05])
plt.show()

"""### Por usuario"""

hoy = date.today()
fecha_final = hoy
fecha_inicial = '2021-10-15'
max_resultados = 500

nombre_usuario = "POTUS"

user_tweets = "snscrape --format '{content!r}'"+ f" --max-results {max_resultados} --since {fecha_inicial} twitter-user '{nombre_usuario} until:{fecha_final}' > user-tweets.txt"
os.system(user_tweets)
if os.stat("user-tweets.txt").st_size == 0:
  print('No se encontraron Tweets')
else:
  df = pd.read_csv('user-tweets.txt', names=['content'])
  for row in df['content'].iteritems():
    print(row)

for i in range(df.shape[0]):
  sentence = df['content'][i]
  print(sentence)
  scores = analizador.polarity_scores(sentence)
  for key in scores:
    print(key, ':', scores[key])
  print()

df['Sentimiento'] = df['content'].apply(lambda i: analizador.polarity_scores(i)['compound'])
df['Negativo'] = df['content'].apply(lambda i: analizador.polarity_scores(i)['neg'])
df['Neutral'] = df['content'].apply(lambda i: analizador.polarity_scores(i)['neu'])
df['Positivo'] = df['content'].apply(lambda i: analizador.polarity_scores(i)['pos'])
df

df[df['Sentimiento'] > 0]

print(df.iloc[df['Sentimiento'].idxmax(),:])
df['content'][107]

print(df.iloc[df['Sentimiento'].idxmin(),:])
df['content'][71]

plt.figure(figsize=(20,7))
df['Positivo'].plot.area(colormap = 'Greens_r', alpha = 0.5)
df['Negativo'].plot.area(stacked=False, colormap = 'Reds_r', alpha = 0.5)
plt.legend()
plt.show()

df.loc[(df['Sentimiento'] >= 0.2), 'Final'] = 'Positivo'
df.loc[(df['Sentimiento'] < 0.2) & (df['Sentimiento'] > -0.2), 'Final'] = 'Neutral'
df.loc[(df['Sentimiento'] <= -0.2), 'Final'] = 'Negativo'
df

tabla_frecuencia = df.groupby('Final').agg(frequency=('Final', 'count'))
tabla_frecuencia['Porcentaje'] = (tabla_frecuencia['frequency']/tabla_frecuencia['frequency'].sum())*100
tabla_frecuencia['% Acumulativo'] = tabla_frecuencia['Porcentaje'].cumsum()
tabla_frecuencia

plt.figure(figsize=(10,8))
plt.pie(tabla_frecuencia['frequency'], labels = tabla_frecuencia.index.tolist(), autopct='%.2f%%',
        colors = ['#ad1313', '#1a35a1', '#24a348'], explode = [0.05, 0.05, 0.05])
plt.show()

"""## Análisis de Sentimientos en español"""

!pip install deep_translator
from deep_translator import GoogleTranslator

"""### Por tema"""

hoy = date.today()
fecha_final = hoy
fecha_inicial = '2021-11-24'
max_resultados = 100

termino_busqueda = 'temblores'

os.system(f"snscrape --since {fecha_inicial} twitter-search '{termino_busqueda} until:{fecha_final}' > result-tweets.txt")
if os.stat("result-tweets.txt").st_size == 0:
  counter = 0
else:
  df = pd.read_csv('result-tweets.txt', names=['link'])
  counter = df.size

print('Número de Tweets : '+ str(counter))

tweets_extraidos = "snscrape --format '{content!r}'"+ f" --max-results {max_resultados} --since {fecha_inicial} twitter-search '{termino_busqueda} until:{fecha_final}' > extracted-tweets.txt"
os.system(tweets_extraidos)
if os.stat("extracted-tweets.txt").st_size == 0:
  print('No Tweets found')
else:
  df = pd.read_csv('extracted-tweets.txt', names=['content'])
  for row in df['content'].iteritems():
    print(row)

for i in range(df.shape[0]):
  sentence = df['content'][i]
  traductor = GoogleTranslator(source='es', target='en')
  sentence = traductor.translate(sentence)
  print(sentence)
  scores = analizador.polarity_scores(sentence)
  for key in scores:
    print(key, ':', scores[key])
  print()

español = []
ingles = []
for i in range(df.shape[0]):
  frase = df['content'][i]
  traductor = GoogleTranslator(source='es', target='en')
  sentence = traductor.translate(frase)
  español.append(frase)
  ingles.append(sentence)
df_ingles = pd.DataFrame({'Tweet_ingles':ingles})
df_español = pd.DataFrame({'Tweet_español':español})
df_idiomas = pd.concat([df_español, df_ingles], axis = 1)
df_idiomas

df_idiomas['Sentimiento'] = df_idiomas['Tweet_ingles'].apply(lambda i: analizador.polarity_scores(i)['compound'])
df_idiomas['Negativo'] = df_idiomas['Tweet_ingles'].apply(lambda i: analizador.polarity_scores(i)['neg'])
df_idiomas['Neutral'] = df_idiomas['Tweet_ingles'].apply(lambda i: analizador.polarity_scores(i)['neu'])
df_idiomas['Positivo'] = df_idiomas['Tweet_ingles'].apply(lambda i: analizador.polarity_scores(i)['pos'])
df_idiomas

print(df_idiomas.iloc[df_idiomas['Sentimiento'].idxmax(),:])
df_idiomas['Tweet_español'][96]

print(df_idiomas.iloc[df_idiomas['Sentimiento'].idxmin(),:])
df_idiomas['Tweet_español'][27]

plt.figure(figsize=(20, 7))
df_idiomas['Positivo'].plot.area(colormap = 'Greens_r', alpha = 0.5)
df_idiomas['Negativo'].plot.area(stacked=False, colormap = 'Reds_r', alpha = 0.5)
plt.legend()
plt.show()

df_idiomas.loc[(df_idiomas['Sentimiento'] >= 0.2), 'Final'] = 'Positivo'
df_idiomas.loc[(df_idiomas['Sentimiento'] < 0.2) & (df_idiomas['Sentimiento'] > -0.2), 'Final'] = 'Neutral'
df_idiomas.loc[(df_idiomas['Sentimiento'] <= -0.2), 'Final'] = 'Negativo'
df_idiomas

tabla_frecuencia = df_idiomas.groupby('Final').agg(frequency=('Final', 'count'))
tabla_frecuencia['Porcentaje'] = (tabla_frecuencia['frequency']/tabla_frecuencia['frequency'].sum())*100
tabla_frecuencia['% Acumulativo'] = tabla_frecuencia['Porcentaje'].cumsum()
tabla_frecuencia

plt.figure(figsize=(10,8))
plt.pie(tabla_frecuencia['frequency'], labels = tabla_frecuencia.index.tolist(), autopct='%.2f%%',
        colors = ['#ad1313', '#1a35a1', '#24a348'], explode = [0.02, 0.02, 0.02])
plt.show()

"""### Por usuario"""

hoy = date.today()
fecha_final = hoy
fecha_inicial = '2021-11-24'
max_resultados = 100

nombre_usuario = "ELTIEMPO"

user_tweets = "snscrape --format '{content!r}'"+ f" --max-results {max_resultados} --since {fecha_inicial} twitter-user '{nombre_usuario} until:{fecha_final}' > user-tweets.txt"
os.system(user_tweets)
if os.stat("user-tweets.txt").st_size == 0:
  print('No se encontraron Tweets')
else:
  df = pd.read_csv('user-tweets.txt', names=['content'])
  for row in df['content'].iteritems():
    print(row)

for i in range(df.shape[0]):
  sentence = df['content'][i]
  traductor = GoogleTranslator(source='es', target='en')
  sentence = traductor.translate(sentence)
  print(sentence)
  scores = analizador.polarity_scores(sentence)
  for key in scores:
    print(key, ':', scores[key])
  print()

español = []
ingles = []
for i in range(df.shape[0]):
  frase = df['content'][i]
  traductor = GoogleTranslator(source='es', target='en')
  sentence = traductor.translate(frase)
  español.append(frase)
  ingles.append(sentence)
df_ingles = pd.DataFrame({'Tweet_ingles':ingles})
df_español = pd.DataFrame({'Tweet_español':español})
df_idiomas = pd.concat([df_español, df_ingles], axis = 1)
df_idiomas

df_idiomas['Sentimiento'] = df_idiomas['Tweet_ingles'].apply(lambda i: analizador.polarity_scores(i)['compound'])
df_idiomas['Negativo'] = df_idiomas['Tweet_ingles'].apply(lambda i: analizador.polarity_scores(i)['neg'])
df_idiomas['Neutral'] = df_idiomas['Tweet_ingles'].apply(lambda i: analizador.polarity_scores(i)['neu'])
df_idiomas['Positivo'] = df_idiomas['Tweet_ingles'].apply(lambda i: analizador.polarity_scores(i)['pos'])
df_idiomas

print(df_idiomas.iloc[df_idiomas['Sentimiento'].idxmax(),:])
df_idiomas['Tweet_español'][78]

print(df_idiomas.iloc[df_idiomas['Sentimiento'].idxmin(),:])
df_idiomas['Tweet_español'][45]

plt.figure(figsize=(20, 7))
df_idiomas['Positivo'].plot.area(colormap = 'Greens_r', alpha = 0.5)
df_idiomas['Negativo'].plot.area(stacked=False, colormap = 'Reds_r', alpha = 0.5)
plt.legend()
plt.show()

df_idiomas.loc[(df_idiomas['Sentimiento'] >= 0.2), 'Final'] = 'Positivo'
df_idiomas.loc[(df_idiomas['Sentimiento'] < 0.2) & (df_idiomas['Sentimiento'] > -0.2), 'Final'] = 'Neutral'
df_idiomas.loc[(df_idiomas['Sentimiento'] <= -0.2), 'Final'] = 'Negativo'
df_idiomas

tabla_frecuencia = df_idiomas.groupby('Final').agg(frequency=('Final', 'count'))
tabla_frecuencia['Porcentaje'] = (tabla_frecuencia['frequency']/tabla_frecuencia['frequency'].sum())*100
tabla_frecuencia['% Acumulativo'] = tabla_frecuencia['Porcentaje'].cumsum()
tabla_frecuencia

plt.figure(figsize=(10,8))
plt.pie(tabla_frecuencia['frequency'], labels = tabla_frecuencia.index.tolist(), autopct='%.2f%%',
        colors = ['#ad1313', '#1a35a1', '#24a348'], explode = [0.02, 0.02, 0.02])
plt.show()

df_idiomas.to_excel('Twitter.xlsx', index = False)