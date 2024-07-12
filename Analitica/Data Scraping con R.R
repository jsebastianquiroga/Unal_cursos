#=========================================================================================================#
#                  Facultad de Ciencias Económicas - Universidad Nacional de Colombia
#                                 Analítica Cualitativa de Redes Sociales
#                                              Web Scraping
#                                 Monitor: Jaime Andrés Fierro Aponte                                 
#=========================================================================================================#

#==============================#
# Limpiar el entorno
rm(list = ls())
# Limpiar la consola: ctrl+L
#==============================#

# Paquetes
install.packages("rvest")
install.packages("tidyverse")
install.packages('xml2')


library(rvest)
library(tidyverse)
library(xml2)


# Almacenar los datos

url <- "https://www.imdb.com/search/title/?title_type=movie&genres=horror&sort=user_rating,desc&explore=title_type,genres"
pagina <- read_html(url)


# Obtener las variables

name <- pagina %>% html_nodes(".lister-item-header a") %>% html_text  
year <- pagina %>% html_nodes(".text-muted.unbold") %>% html_text
rating <- pagina %>% html_nodes(".ratings-imdb-rating strong") %>% html_text
synopsis <- pagina %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text


# Conversión a dataframe

df_01 <- data.frame(name, year, rating, synopsis, stringsAsFactors = FALSE)
View(df_01)

# Generar los enlaces de cada película

movie_link <- pagina %>% html_nodes(".lister-item-header a") %>% 
  html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
movie_link
df_02 <- data.frame(name, year, rating, synopsis, movie_link, stringsAsFactors = FALSE)
View(df_02)

write.table(df_02, file = 'Películas de terror.txt')





