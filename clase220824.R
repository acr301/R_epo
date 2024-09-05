

#one-way table


#ayuda a que los resultados sean reproducibles a pesar que el código implique escribir variables que toman
#valores aleatorios
#set.seed(0)


#marco de datos dataframe
#variables de un conjunto de datos como columnas

#8 2,5,6,7,7,9,9 = 82,85,86,87,87,89,89
#9 0,


#40 compañías, 40 casos

#Rango R = Valor Mayor - Valor Menor

#Unidad U=1
#Amplitud del intervalo
#Amplitud = Intervalo + Unidad
#Amplitud = 5

#Numero de intervalos N=6

#Intervalo = Rango / N
#I = 29/6 = 4.83333
#N = Numero de intervalos


#funcion c base, c(objetos a ser concatenados)
datos <- c(82,85,86,87,87,89,89,
                90,91,91,92,93,94,95,95,95,95,95,97,98,99,99,
                100,100,101,101,103,103,103,104,105,105,106,107,107,107,109,
                110,110,111)

valor_min <- min(datos)
valor_max <- max(datos)

#breaks son los límites Inferiores y límites superiores
limites<-seq(from = valor_min, to = valor_max, length.out = 7)
#seq(de,hasta,puntos)

marcos_clases <- round((head(limites,-1) + tail(limites,-1)) / 2,0)



tabla_frecuencia <- table(cut(datos, breaks=limites, include.lowest = TRUE))
#función cut, convertir marco_datos (lista numérica) a factor

frecuencia_acumulada <- cumsum(tabla_frecuencia)
limites_redondeados <- round(limites,0)

intervalos <- paste(limites_redondeados[-length(limites_redondeados)], "-", limites_redondeados[-1])

#proporcion de la tabla
frecuencia_relativa <- prop.table(tabla_frecuencia) * 100

#juntar en un solo marco de datos

#problema de la confusión entre marca de clase e intervalo inicial

resultado_marco_datos <- data.frame(Intervalo = intervalos,
                                    Limites_Reales_Intervalos = paste0("[", limites_redondeados[-length(limites_redondeados)],",",limites_redondeados[-1],"]"),
                                    Marco_Clases= marcos_clases,
                                    Frecuencia= as.vector(tabla_frecuencia),
                                    Frecuencia_Relativa= round(as.vector(frecuencia_relativa),2),
                                    Frecuencia_Acumulada= frecuencia_acumulada
                                    
)

#marco de clase para cada intervalo, calculado como el promedio del limite inferior y superior
# de cada intervalo


resultado_marco_datos
plot(resultado_marco_datos)