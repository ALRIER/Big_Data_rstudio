rm(c)
#del dataframe extraigo las variables de interes
a=as.numeric(data$lifeExp)
b=as.numeric(data$gdpPercap)
c=as.numeric(data$pop)
#Testeo las variables de interes para saber que si hay relacion entre ellas.
cor.test(a,b)
#organizo las variables con un cbind para que queden en columnas
d=cbind(a,b,c)
#con las variables de interes creo un dataframe
e=as.data.frame(d)

#Supuestos de normalidad
#kolmogorov Smirnov  > 50
#Shapiro wilk  < 50



#Ahora introduzco mi dataframe en una matriz de correlacion. 
cortest=cor(e)

library(corrplot)
#dibujo mi primer grafico para ver como se va viendo la cosa. 
corrplot(cortest)

#No me gusta el resultado, quiero cambiarlo por otro metodo. 
corrplot(cortest, method = "number")

#Del dataframe C que contiene las 2 variables de mi interes, voy a dibujar
#una matriz de correlacion partida a la mitad. 

#le puedo meter colores, solamente si las correlaciones son positivas. (todas)
pairs(e,                     # Data frame de variables
      labels = colnames(e),  # Nombres de las variables
      pch = 25,                 # Símbolo pch
      #bg = rainbow(3)[cortest],  # Color de fondo del símbolo (pch 21 a 25)
      #col = rainbow(3)[cortest], # Color de borde del símbolo
      main = "",            # Título del gráfico
      row1attop = TRUE,         # Si FALSE, cambia la dirección de la diagonal
      gap = 1,                  # Distancia entre subplots
      cex.labels = NULL,        # Tamaño del texto de la diagonal
      font.labels = 1)          # Estilo de fuente del texto de la diagonal


## Función para agregar coeficientes de correlación
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Elimina la función abs si lo prefieres
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Escala el texto al nivel de correlación
}

# Dibujamos la matriz de correlación
pairs(e,
      upper.panel = panel.cor,    # Panel de correlación
      lower.panel = panel.smooth) # Curvas de regresión suavizadas

