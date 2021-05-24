# Datos hidrológicos ejercicio explorativo
#### Se debe de cargar el archivo csv, para tener los datos disponibles para trabajar

inp <- read.csv("FDC.csv", na.strings = "")

#### Con las dos siguientes funciones se visualizarán los encabezados y las dimensiones
head(inp)  
dim(inp)

#### Un ejemplo abreviado de cómo se podría indagar en un archivo que contiene NA
inp[!complete.cases(inp), ]

#### Como alternativa se puede usar newinp <- na.omit(inp), para eliminar filas con NA

#### A continuación, se visualizarán rápidamente los datos de las cuencas
plot(  
  inp[ , 2], type = "l", col = "blue", main = 'Volumen de agua por tiempo',  
  xlab = 'Fecha',  
  ylab = 'Caudal en mm por dia '  
      )  
lines(inp[ , 3], col = "green")

legend(  
  x = "topleft",  
  inset = 0.05,  
  legend = c("Estrella", "Banano"),  
  fill = c("blue", "green"),  
  horiz = FALSE  
)  
![Grafico 1](https://user-images.githubusercontent.com/82826848/119309168-2f69a380-bc2b-11eb-931c-d723d995a1af.png)

#### Ahora bien, se va a ver una estadística, un promedio, de los caudales diarios de cada río con la siguiente función
summary(inp[ , 2:3])

#### A continuación, se van a visualizar los datos estadísticos de ambos caudales
hist(inp[ , 2],  
     main = 'Estrella',  
     xlab = 'Cant. de mm por dia',  
     ylab = 'Frecuencia'  
     )  
     
![Grafico 2](https://user-images.githubusercontent.com/82826848/119301564-eb24d600-bc1f-11eb-9ddf-03d1e195f5ad.png)     
  
hist(inp[ , 3],  
     main = 'Banano',  
     xlab = 'Cant. de mm por dia',  
     ylab = 'Frecuencia'  
     )
     
![Grafico 3](https://user-images.githubusercontent.com/82826848/119301565-eb24d600-bc1f-11eb-9d98-e075887277ff.png)


#### Para un mayor manejo de la información y del trabajo, se van a nombrar los encabezados de las columnas
names(inp) <- c("fecha", "Estrella", "Banano")

#### Con el comando attach se podran evaluar datos en el archivo csv y visualizarlos, por ejemplo:
attach(inp)   
plot(Estrella,  
     main = 'Rio Estrella',  
     xlab = 'Fecha',  
     ylab = 'Caudal en mm por dia'  
     )
     
![Grafico 4](https://user-images.githubusercontent.com/82826848/119301566-eb24d600-bc1f-11eb-825f-1a515d520ea8.png)


#### Se va a crear archivo intermedio, en el que se usará una función para especificar el tiempo con el que se trabajará y el formato de las fechas
Tempdate <- strptime(inp[ , 1], format = "%d/%m/%Y")

#### Con esta función pasada se especificó que las fechas seran día/mes/año, a continuacion, se usarán funciones vectorizadas, funciones tapply para periodos anuales
MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%Y"), FUN = sum)  
MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%Y"), FUN = sum)

#### En estas especificamos que para cada río se va a requerir toda la información de los caudales por mm diarias por año. Además, se va a exportar el csv con esa información.
write.csv(rbind(MAQ_Estrella, MAQ_Banano), file = "MAQ.csv")

#### Ahora se van a visualizar los MAQ (valores anuales de caudal) que se calcularon anteriormente
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
![Grafica 5](https://user-images.githubusercontent.com/82826848/119301560-ea8c3f80-bc1f-11eb-8333-649d089b8157.png)

#### Se volverá a usar la función vectorizada, funciones tapply y esta vez se harán en periodos mensuales
MMQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%m"), FUN = sum)    
MMQ_Banano <- tapply(Banano, format(Tempdate, format = "%m"), FUN = sum)



### Análisis de correlación en los datos del archivo csv.
#### Se usará la función "cor" que dará datos de correlación en el input y se usará el método "spearman" ya que distribuirá de forma normal los datos y de forma no paramétrica
corinp <- cor(inp[ , 2:3], method = "spearman")

#### Ahora se visualizará la correlación
plot(Estrella, Banano,  
     main = 'Correlacion entre la cuenca del Rio Estrella contra el Rio Banano',  
     )
     
![Grafico 10](https://user-images.githubusercontent.com/82826848/119310529-18c44c00-bc2d-11eb-8a90-cf730a93fd7c.png)

#### Adicionalmente, se creará un modelo de regresión lineal con la función de grupo lm, es decir, para relacionar los caudales de ambos ríos
inp.lm <- lm(Estrella ~ Banano, data = inp)  
summary(inp.lm)

#### Finalmente, se va a visualizar el modelo, que dará como resultado los diagnósticos del análisis y una distribución empírica relacionando los residuos de las dos variables
plot(inp.lm)

![Grafico 6](https://user-images.githubusercontent.com/82826848/119301568-ebbd6c80-bc1f-11eb-9847-d26d80dc1403.png)
![Grafico 7](https://user-images.githubusercontent.com/82826848/119301569-ebbd6c80-bc1f-11eb-8638-6495094720ba.png)
![Grafico 8](https://user-images.githubusercontent.com/82826848/119301571-ebbd6c80-bc1f-11eb-9dd8-a907087a38d9.png)
![Grafico 9](https://user-images.githubusercontent.com/82826848/119301572-ec560300-bc1f-11eb-9ba7-a088ff83a602.png)
