#=========================================================================================================#
#                  Facultad de Ciencias Económicas - Universidad Nacional de Colombia
#                                 Analítica Cualitativa de Redes Sociales
#                                       Análisis de Sentimientos
#                                                             
#=========================================================================================================#

#==============================#
# Limpiar el entorno
rm(list = ls())
# Limpiar la consola: ctrl+L
#==============================#


# Paquetes

install.packages('tidytext')
install.packages('tidyverse')
install.packages('wordcloud')
install.packages('reshape2')
install.packages('RColorBrewer')
install.packages('readxl')
install.packages("textdata")

library(tidytext)
library(tidyverse)
library(wordcloud)
library(reshape2)
library(RColorBrewer)
library(readxl)
library(textdata)

# Importación de datos

setwd('C:/Users/Usuario/Mi unidad/EXTENSION/PEEC/ANALÍTICA/ANALÍTICA CUALI/Material/PYTHON')
datos = read_excel('Tweets.xlsx')
colnames(datos)

# Extracción de palabras

table_token = unnest_tokens(tbl = datos,
                              output = 'word',
                              input = 'content',
                              token = 'words')

# Dimensión de la tabla con las palabras
dim(table_token)

table_token = anti_join(x = table_token,
                          y = stop_words,
                          by = 'word')

# Contar las palabras
count(table_token,
      word,
      sort = TRUE)

# Eliminar las palabras no útiles
table_token = filter(table_token, word!='https' & word!='t.co')

# Gráfico con la cantidad de palabras
table_token %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_text(aes(label=n), hjust= -0.2) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  theme_minimal()

# Palabras de bing
get_sentiments('bing')

# Palabras de nrc
get_sentiments('nrc')


# Relacionar las palabras de nuestros datos con las palabras de bing
table_bing = table_token %>%
  inner_join(get_sentiments('bing'))

# Relacionar las palabras de nuestros datos con las palabras de nrc
table_nrc = table_token %>%
  inner_join(get_sentiments('nrc'))

# Contar las palabras con bing
table_bing %>%
  count(word,sentiment,sort=TRUE)

# Contar las palabras con nrc
table_nrc %>%
  count(word,sentiment,sort=TRUE)

# Gráfico con las palabras negativas y postivas de bing
table_bing %>%
  count(word,sentiment,sort=TRUE) %>%             
  group_by(sentiment) %>%                         
  top_n(15) %>%                                         
  ungroup() %>%                                 
  mutate(word=reorder(word,n)) %>%                    
  ggplot(aes(word,n,fill=sentiment))+       
  geom_col(show.legend = FALSE)+              
  geom_text(aes(label=n), hjust= 1.2) +       
  facet_wrap(~sentiment,scales = 'free_y') +  
  coord_flip() +                              
  xlab(NULL)          

# Categorías nrc
unique(table_nrc$sentiment)


# Gráfica por categorías nrc
table_nrc %>%
  filter(sentiment!='negative' & sentiment!='positive') %>%
  count(word,sentiment,sort=TRUE) %>%             
  group_by(sentiment) %>%                        
  top_n(5) %>%                                                     
  ungroup() %>%                                   
  mutate(word=reorder(word,n)) %>%                
  ggplot(aes(word,n,fill=sentiment))+           
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n), hjust= 0) +
  facet_wrap(~sentiment,scales = 'free_y')+  
  coord_flip() +
  xlab(NULL)


# Gráfico con las palabras negativas y postivas de nrc
table_nrc %>%
  filter(sentiment=='negative' | sentiment=='positive') %>%
  count(word,sentiment,sort=TRUE) %>%             
  group_by(sentiment) %>%                        
  top_n(5) %>%                                                   
  ungroup() %>%                                   
  mutate(word=reorder(word,n)) %>%                
  ggplot(aes(word,n,fill=sentiment))+           
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n), hjust= 0) +
  facet_wrap(~sentiment,scales = 'free_y')+  
  coord_flip() +
  xlab(NULL)


# Nube de palabras
table_token %>%
  count(word) %>%
  with(wordcloud(words=word,
                 freq=n,
                 max.words = 250,
                 scale = c(3,1),
                 rot.per = 0.3,
                 random.order = FALSE,
                 colors=brewer.pal(6,'Dark2')))


# Nube de palabras positivas de bing
table_bing%>%
  count(word,sentiment) %>%
  filter(sentiment=='positive') %>%  
  with(wordcloud(words=word,
                 freq=n,
                 max.words = 250,
                 scale = c(3,1),
                 rot.per = 0.3,
                 random.order = FALSE,
                 colors=brewer.pal(6,'Dark2')))

# Nube de palabras negativas de bing
table_bing%>%
  count(word,sentiment) %>%
  filter(sentiment=='negative') %>%  
  with(wordcloud(words=word,
                 freq=n,
                 max.words = 250,
                 scale = c(3,1),
                 rot.per = 0.3,
                 random.order = FALSE,
                 colors=brewer.pal(6,'Dark2')))

# Nube de palabras positivas y negativas (comparación) de bing
table_bing %>%
  count(word,sentiment,sort=TRUE) %>%
  acast(word~sentiment,value.var = 'n', fill = 0) %>%
  comparison.cloud(colors = c('red','green'), 
                   max.words = 300,
                   title.size = 2)


# Nube de palabras por categorías de nrc
table_nrc %>%
  count(word,sentiment,sort=TRUE) %>%
  filter(sentiment!='positive' & sentiment!='negative') %>% 
  acast(word~sentiment,value.var = 'n', fill = 0) %>%
  comparison.cloud(title.size = 1.5)
