pkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse","raster","sf","ggspatial","cluster","factoextra",
              "NbClust","tidyr","forecast","semTools","corrplot",
              "corrr","haven","psych","dplyr","lavaan","readr","cvms","tm",
              "NLP","SnowballC","RColorBrewer","wordcloud","wordcloud2",
              "RefManageR","bibliometrix","GGally","quanteda","ggplot2",
              "ggpubr","Factoshiny","syuzhet","RColorBrewer","tokenizers",
              "stringr","sentimentr","stringi","stopwords","twitteR",
              "mscstexta4r","plyr","psych","corrr","latticeExtra",
              "semPlot","lavaan","readr","lme4","sjPlot","gvlma","Rcmdr",
              "tidymodels","caret","lmtest","gapminder","png","rtweet","knitr")

pkg(packages)

#BASE DE TRABAJO: Twiter calls----------------

setup_twitter_oauth("DP7F7o2r8bS5hophlgGdi9Kqd",#api key
                    "OQcWOdJG2JqAa8370w5Zu0UseQHu3QM7ChJ9xUTNTXmbAMzGWc",#api secret key
                    "284827529-LORYnWAHxeYBUpm3PqnQ3EqD5JCa46jfVaSFnv0F",#acces token
                    "TOoNhDxMcSnxlYBguc7xyIK4bAABnSwoSbiFuFRoERnps")#acces token secret

#PRIMER PUNTO: llamado 1 con 10 listas.
llamado1<- searchTwitter("Colombia", n=10)

llamado2<-searchTwitter('Russia', n=50, resultType = "popular")

llamado3<-userTimeline('@ClaudiaLopez', n=500)

#SEGUNDO PUNTO: Lists---------------------------------

#comando1 rectifico que haya quedado bien extraido
llamado1[1:10]
#comando2, extraigo los objetos 5 y 7 en nuevos objetos llamados pa y po
pa<-llamado1[5]
po<-llamado1[7]
#comando 3 comrpuebo el largo de la lista
length(llamado1)
#comando 4 comrpuerbo el tipo de elementos de mis listas
str(llamado1)
str(po)
#comando 5 creo una lista con 3 elementos
lista <- list("lista", 1, 2)
#comando 6 adjuntando elementos a mi lista.
nuevalista<- list(lista, po)
#comando 7 compruebo el largo de mi nueva lista
length(nuevalista)
#comando 8 ahora compruebo los elementos de esa lista nueva
nuevalista[1:2]
#comando 9 ahora nombrare los cajoncitos dentro de la lista
names(nuevalista)<-c("listaA", "listaB")
#comando 10 ahora verifico los nombres de mis listas
nuevalista[1:2]
#comando 11 extraigo un elemento de mi lista
extraido<-nuevalista$listaA
#comando 12 inspecciono el objeto nuevo
view(extraido)
#comando 13 no me gusto el resultado, lo inspecciono de otra forma
#pero primero miro su longitud
length(extraido)
#comando 14 ahora lo inspecciono
extraido[1:3]
#comando 15, el objeto "extraido" tiene los elementos de la lista A asi que lo eliminare
rm(extraido)

#acabo de hacer 15 comandos con listas que me ayudan a practicar estas funciones 
#y a entender como funcionan las listas internamente. 

#TERCER PUNTO: data_twitter-----------------------------------------

#transformo el objeto a un df. 
data_twitter<-twListToDF(llamado2)

#ahora hago al menos 10 comandos usando tuberias

#pero para eso necesito entender mi dataframe(df)

#con este comando inspecciono el largo y ancho del objeto
view_df(data_twitter)
#ahora lo inspecciono
view(data_twitter)

#Comando 1 aplico un filtro para la variable conteo de favoritos (likes)
#mayores o iguales a 1200 y le digo que me los extraiga en un nuevo objeto llamado FV
fv<-data_twitter %>% 
  filter(favoriteCount >= 1200)

#Comando2
#Ahora me interesa conocer la media de la variable FV para saber cual es la
#media de likes que la gente da a estos tweets, pero debo recordar que la columna sobre 
#la que voy a sacar la media es la misma de antes "favoriteCount" porque el filtro
# lo que hace es una extracci[on completa
Fvmedia<-fv %>% 
  summarise(mean(favoriteCount))
#ahora reviso mi resultado
#la media de likes que la gente da para estos tweets es de 14459 

#Comando 3
#Ahora de la variable data_twitter quiero saber que tweets contienen la palabra KyivIndependent
contenido<-data_twitter %>%
  filter(screenName %in% c("KyivIndependent")) 

#para continuar voy a logearme con el paquete twitteR
create_token(app="ClasII",
             consumer_key = "DP7F7o2r8bS5hophlgGdi9Kqd",#api key
             consumer_secret = "OQcWOdJG2JqAa8370w5Zu0UseQHu3QM7ChJ9xUTNTXmbAMzGWc",#api secret key
             access_token ="284827529-LORYnWAHxeYBUpm3PqnQ3EqD5JCa46jfVaSFnv0F",#acces token
             access_secret ="TOoNhDxMcSnxlYBguc7xyIK4bAABnSwoSbiFuFRoERnps")#acces token secret

#voy a es=xtraer el dataframe con caracter[isticas similares y lo llamare data_twitter2
#pero esta vez extraere el perfil de Claudia Lopez y ampliare la busqueda a 500
data_twitter2 <- get_timeline(user = "@ClaudiaLopez", n = 500, parse = TRUE, check = FALSE)

#Si inspeccionan el objeto data_twitter2 se van a dar cuenta que les da acceso a mas informacion
#por lo que a mi parecer es mas util para analisis de datos.

#comando 4
#le pido que extraiga en una columna nueva llamada fuente un filtro con 
#todos los tweets  que hayan sido creados y que contengan la palabra 
#"Petro" dentro de la variable texto y me digas si la contiene o no

fuente <- data_twitter2 %>%
  mutate(
    fuente = grepl('Petro', text)) %>%
  select('text', 'fuente')

#comando 5 ahora le digo que me haga un filtro para saber de todos los tweets seleccionados
#cual cumple la condicion que le di previamente

true <- fuente %>% 
  filter( fuente == FALSE)

#comando 6 y 7 tomo los resultados de la variable anterior y hago una sumatoria de los 
#que fueron retwiteados y los llevo a una variable nueva donde se presente ese valor

Retwiteados <- data_twitter2 %>% 
  filter(is_retweet == TRUE) %>% 
  mutate(nueva = sum(is_retweet))

#comando 8 busco cuantos tweets se publicaron desde New York

location <- data_twitter2 %>% 
  filter(quoted_location == 'New York, NY')

#comando 9 y 10 saco medias y modas de algunas variables. 

conteo <- data_twitter2 %>%
  summarise(mean(favorite_count))

conteo1 <- data_twitter2 %>%
  mutate(moda=mode(favorite_count)) #multimodal, numerico. 


#CUARTO PUNTO: Analisis de sentimiento y mineria de texto ---------------

#extraere la variable texto
te<-data_twitter2$text

#Ahora hagamos un data mining -----------------------------------------------
#elimino puntución y caracteres especiales
discurso <- gsub("[[:cntrl:]]", " ", te)
#Convertimos todo a minúsculas.
discurso <- tolower(discurso)
#quto las stopwords("spanish").
discurso <- removeWords(discurso, words = stopwords("spanish"))
discurso[99]
discurso <- removeWords(discurso, words = c("usted","!","va"))
#Nos deshacemos de la puntuación.
discurso <- removePunctuation(discurso)
#removemos los números.
discurso <- removeNumbers(discurso)
#conformo un vector de palabras
discurso <- Corpus(VectorSource(discurso)) 

discurso=tm_map(discurso, removeWords, c("comunidad", "gracias",
                                         "labogotáqueestamosconstruyendo",
                                         "bogota"))
#organizo el compendio de palabras en un objeto tipo matriz de datos
letras<- TermDocumentMatrix(discurso)
letrasmatrix <- as.matrix(letras) 
'''hago un vector que va a sumar la repetición de palabras en la matriz y así
consigo la frecuencia total de palabras que hay para cada término, despues le digo
que las sume y las organice'''
vector <- rowSums(letrasmatrix) 
vectorr<- sort(vector, decreasing = T)
'''ahora bien, cabe revisar la frecuencia de palabras para así poder identificar 
cuales de estas palabras son y cuales de estas palabras no me son útiles por eso 
imprimo la matriz antes de que podamos sacar conclusiones del analisis'''

#inspecciono el vector para conocer el orden de las palabras
view(vectorr)
#Diferente modelo de inspeccion de datos dentro del vector, para ver si el orden marcha bien.
vectorr[1:20]

'''AHORA ES IMPORTANTISIMO: si veo que en el vector de palabras hay algun termino
que esté molestando demasiado, la forma mas fácil de eliminarlo es regresar al 
corpus y quitarlo arriba con el comando removeWords y repetir los pasos hasta 
aquí, pero se debe tener en cuenta que es mejor retroceder y correr todos los
comandos de limpeza nuevamente, desde el princpio, es decir, desde
aquí discurso <- gsub("[[:cntrl:]]", " ", a) pero ahora incluyendo los terminos
que se desea eliminar del comando removeWords'''

#transformo todo en un dataframe
dataletras <- data.frame(word= names(vectorr),freq=vectorr)  

barplot(dataletras[1:20,]$freq, las = 2, names.arg = dataletras[1:20,]$word, 
        col = brewer.pal(n = 100, name = "Set3"), main ="Pabalras mas frecuentes",
        ylab = "Frecuencia de palabras") 


wordcloud(words = dataletras$word, freq = dataletras$freq, min.freq = 2,
          max.words=50, random.order=FALSE, rot.per=0.35,  
          colors=brewer.pal(7, "Dark2"), scale=c(3.5,1.25))


wordcloud2(data=dataletras, size=0.7,
           color='random-dark', shape = 'triangle')

'''Veamos ahora cómo se asocian algunas palabras (terms) en Niebla con la 
función findAssocs. Como podemos introducir un vector, podemos obtener las 
asociaciones de varias palabras a la vez. He elegido 
"Petro", "petro","Uribe", "uribe"

Es importante recordar que con esto no estamos pidiendo la asociacion de estas
cuatro palabras entre si, sino las asociaciones para cada una de las cuatro, que
no necesariamente deben coincidir.

Esta también nos pide el límite inferior de correlación (corlimit)
para mostrarnos. Valores cercanos a 1 indican que las palabras aparecen casi
siempre asociadas una con otra, valores cercanos a 0 nos indican que nunca o
casi nunca lo hacen.

El valor que decidamos depende del tipo de documento y el tipo de asociaciones
que nos interesen. para nuestros fines, lo he fijado en .25.'''
findAssocs(letras, terms = c("casa", "comunidad",
                             "mujer", "bogota"), corlimit = .25)
#CLUSTERING DE PALABRAS------------------------------------------------------
'''ahora vamos a eliminar primero todos los terminos dispersos paraque no jodan,
como se trata de una correlación los valores que manejaremos serán de 0 a 1'''
nov_new <- removeSparseTerms(letras, sparse = .05)
#llevamos el objeto a matriz
nov_new <- nov_new %>% as.matrix()
#Matriz de distancia--------------------------------------------------------
'''Necesitamos crear una matriz de distancias para empezar agrupar, lo cual 
requiere que los valores en las celdas sean estandarizados de alguna manera.

Podríamos usar la función scale, pero realiza la estandarización usando la media
de cada columna como referencia, mientras que nosotros necesitamos como 
referencia la media de cada renglón.

Así que obtenemos una estandarización por renglones de manera manual.'''
nov_new <- nov_new / rowSums(nov_new)
#Hecho esto, nuestra matriz ha sido estandarizada.
'''Procedemos a obtener una matriz de distancia a partir de ella, con el método
de distancias euclidianas y la asignamos al objeto nov_dist.'''
nov_dist <- dist(nov_new, method = "euclidian")
'''Realizaremos nuestro agrupamiento jerárquico usando la función hclust, de la
base de R. Este es en realidad un procedimiento muy sencillo una vez que hemos
realizado la preparación.

Usaremos el método de Ward (ward.D), que es el método por defecto de la función
hclust y asignaremos sus resultados al objeto nov_hclust.'''
nov_hclust <-  hclust(nov_dist, method = "ward.D")
#Graficamos los resultados usando plot para generar un dendrograma.
plot(nov_hclust, main = "Dendrograma de Niebla - hclust", sub = "", xlab = "")
'''De este modo podemos observar los grupos de palabras que existen en Niebla. 
Por ejemplo, "augusto" y "eugenia" forman un grupo, "puede" y "ser", forman otro
grupo ("puede ser" es una frase común en este libro).

Además, podemos ver qué palabras pertenecen a grupos lejanos entre sí, 
por ejemplo, "quiero" y "verdad".

Podemos enfatizar los grupos de palabras trazando un rectángulo usando
rect.hclust y con especificando cuántos grupos (k) deseamos resaltar.

Crearemos el mismo gráfico pidiendo diez grupos.'''

plot(nov_hclust, main = "Dendrograma de Niebla - hclust", sub = "", xlab = "")
rect.hclust(nov_hclust, k = 10, border="blue")

