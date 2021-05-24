# Datos hidrologicos ejercicio explorativo
#### Se debe de cargar el archivo csv, para tener los datos disponibles para trabajar

inp <- read.csv("FDC.csv", na.strings = "")

#### Con las dos siguientes funciones se visualizaran los encabezados y las dimnesiones
head(inp)  
dim(inp)

#### Un ejemplo abreviado de como se podia indagar en un archivo que contiene NA
inp[!complete.cases(inp), ]

#### Como alternativa se puede usar newinp <- na.omit(inp), para eliminar filas con NA

#### A continuacion, se visualizaran rapidamente los datos de las cuencas
plot(  
  inp[, 2], type = "l", col = "blue", main = 'Volumen de agua por tiempo',  
  xlab = 'Fecha',  
  ylab = 'Caudal en mm por dia ',  
      )  
lines(inp[, 3], col = "green")

legend(  
  x = "topleft",  
  inset = 0.05,  
  legend = c("Estrella", "Banano"),  
  fill = c("blue", "green"),  
  horiz = FALSE  
)

#### Ahora bien, se va a ver una estadistica, un promedio, de los caudales diarios de cada rio con la siguiente funcion
summary(inp[, 2:3])

#### Acontinucacion, se van a visualizar los datos estadisticos de ambos caudales
hist(inp[, 2],  
     main = 'Estrella',  
     xlab = 'Cant. de mm por dia',  
     ylab = 'Frecuencia'  
     )  
hist(inp[, 3],  
     main = 'Banano',  
     xlab = 'Cant. de mm por dia',  
     ylab = 'Frecuencia'  
     )

#### Para un mayor manejo de la informacion y del trabajo se van a nombrar los encabezados de las columnas
names(inp) <- c("fecha", "Estrella", "Banano")

#### Con el comando attach se podran evaluar datos en el csv y visualizarlos, por ejemplo:
attach(inp)   
plot(Estrella,  
     main = 'Rio Estrella',  
     xlab = 'Fecha',  
     ylab = 'Caudal en mm por dia'  
     )

#### Se va a crear archivo intermedio, en el que se usara una funcion para especificar el tiempo con el que se trabajara y el formato de las fechas
Tempdate <- strptime(inp[, 1], format = "%d/%m/%Y")

#### Con esta funcion pasada se especifico que las fechas seran dia/mes/ano, a continuacion, se usaran funciones vectorizadas, funciones tapply anuales
MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%Y"), FUN = sum)  
MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%Y"), FUN = sum)

#### En estas especificamos que para cada rio se va a requerir toda la informacion de los caudales por mm diarias por ano. Ademas, se va a exportar el csv con esa informacion
write.csv(rbind(MAQ_Estrella, MAQ_Banano), file = "MAQ.csv")

#### Ahora se van a Visualizar los MAQ (valores anuales de caudal) que se calcularon anteriormente
plot(  
  MAQ_Banano, ylim = c(0, 3000),  
  main = 'Valores anuales en mm por año',  
  xlab = 'Fechas',  
  ylab = 'Caudal anual en mm'  
  )  
lines(MAQ_Estrella, col = 2)

legend(  
  x = "topright",  
  inset = 0.05,  
  legend = c("Estrella", "Banano"),  
  fill = c("red", "black"),  
  horiz = FALSE  
)

#### Se volvera a usar la funcion vectorizada, funciones tapply y esta vez se haran mensuales
MMQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%m"), FUN = sum)    
MMQ_Banano <- tapply(Banano, format(Tempdate, format = "%m"), FUN = sum)



### Analisis de correlacion en los datos del archivo csv.
#### Se usara la funcion "cor" que dara datos de correlacion en el input y se usara el metodo "spearman" ya que distribuira de forma normal los datos y de forma no parametrica
corinp <- cor(inp[, 2:3], method = "spearman")

#### Ahora se visualizara la Correlacion
plot(Estrella, Banano,  
     main = 'Correlacion entre la cuenca del Rio Estrella contra el Rio Banano',  
     )

#### Adicionalmente se creara un modelo de regresion lineal con la funcion de grupo lm, es decir, para relacionar los caudales de ambos rios
inp.lm <- lm(Estrella ~ Banano, data = inp)  
summary(inp.lm)

#### Finalmente, se va a visualizar el modelo, que dara como resultado los diagnosticos del analisis y una distribucion empirica relacionando los residuos de las dos variables
plot(inp.lm)

