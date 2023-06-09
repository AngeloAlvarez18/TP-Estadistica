#---------------------------------------PREGUNTAS-------------------------------------------------
# 2. como poner q se haga la suma acumulativa solamente de las primeras 3 columnas
# 3. cada cuanto tomar los intervalos (si tomo uno en especifico, los ultimos valores del 
# intervalo no los toma)

#-------------------------------------------------------------------------------------------------
library(readxl)
ruta_archivo <- "~/Desktop/codes/TP-Estadistica/Base4.xls"
datos <- read_excel(ruta_archivo, col_names = TRUE)
#acceder a las columnas por nombre
attach(datos) 
#length_interval = round(sqrt(altura))

#---------------------------------------ALTURA-------------------------------------------------

#--------------------------------------------
# tabla de frecuencia altura
breaks_altura = seq(0, 40, 5) # Intervalos para la tabla de freq

rango_altura = cut(altura, breaks = breaks_altura, right = TRUE)
freq_abs_altura = table(rango_altura)

freq_rel_altura = round(freq_abs_altura / 350, 4)
freq_abs_ac_altura = cumsum(freq_abs_altura)
freq_rel_ac_altura = cumsum(freq_rel_altura)
porc_altura = freq_rel_altura * 100
porc_ac_altura = freq_rel_ac_altura * 100
tabla_altura = cbind(freq_abs_altura, freq_rel_altura, porc_altura, freq_abs_ac_altura,
                     freq_rel_ac_altura, porc_ac_altura)
total_altura = apply(tabla_altura, 2, sum)
tabla_altura_total = rbind(tabla_altura, total_altura)
#--------------------------------------------

#mean(altura)
#median(altura)
# histograma de altura

hist(altura, breaks = 8, col = "green", ylab = "Cantidad",
    xlab = "Altura", main = "Frecuencia absoluta de la altura", axes = FALSE)
axis(1, at=breaks_altura)
axis(2, at=c(seq(0,100,10)))


freq_rel_altura = prop.table(altura)
freq_rel_acum_altura = cumsum(freq_rel_altura)
# histograma de frecuencia relativa altura
hist(freq_rel_altura, breaks = 8, col = "red", ylab = "Cantidad", xlab = "Altura",right = FALSE)
title_box = strsplit(c("Altura en metros de los arboles; de la ciudad de Buenos Aires"), split = ";")
boxplot(altura, col="orange", ylab="Frecuencia", main=title_box)


#---------------------------------------ESPECIE-------------------------------------------------
#grafico para las especies
especie_order = ordered(especie, levels = c("Ceibo", "Eucalipto", "Álamo", "Palo borracho",
                                      "Acacia", "Jacarandá", "Casuarina", "Ficus", "Fresno"))
freq_abs = table(especie_order)
freq_rel = prop.table(table(especie_order))
porc = freq_rel * 100
tabla = cbind(freq_abs, freq_rel, porc)
#--------------------------------------------
freq_abs_especie = table(especie_order)
# tabla de frecuencia especie
freq_rel_especie = round(freq_abs_especie / sum(freq_abs_especie), 9) # un 9 xq sino no da exacta la tabla
freq_abs_ac_especie = cumsum(freq_abs_especie)
freq_rel_ac_especie = cumsum(freq_rel_especie)
porc_especie = freq_rel_especie * 100
porc_ac_especie = freq_rel_ac_especie * 100
tabla_especie = cbind(freq_abs_especie, freq_rel_especie, porc_especie)
total = apply(tabla_especie, 2, sum)
tabla_especie_total = rbind(tabla_especie, total)
tabla_especie_total2 = rbind(tabla_especie)
#--------------------------------------------
hist(freq_abs_especie)

boxplot(altura~especie)

#---------------------------------------DIAMETRO-------------------------------------------------
# tabla de frecuencia diametro
breaks_diametro = seq(0, max(diametro), 20) # Intervalos para la tabla de freq
rango_diametro = cut(diametro, breaks = breaks_diametro, right = TRUE)
freq_abs_diametro = table(rango_diametro)
freq_rel_diametro = round(freq_abs_diametro / sum(freq_abs_diametro), 4)
freq_abs_ac_diametro = cumsum(freq_abs_diametro)
freq_rel_ac_diametro = cumsum(freq_rel_diametro)
porc_diametro = freq_rel_diametro * 100
porc_ac_diametro = freq_rel_ac_diametro * 100
tabla_diametro = cbind(freq_abs_diametro, freq_rel_diametro, porc_diametro, freq_abs_ac_diametro,
                     freq_rel_ac_diametro, porc_ac_diametro)
total_diametro = apply(tabla_diametro, 2, sum)
tabla_diametro_total = rbind(tabla_diametro, total_diametro)
#--------------------------------------------
tallo_y_hoja_diametro = barplot(freq_abs_diametro, col = "blue", ylab = "Cantidad", xlab = "Diametro")

plot(freq_rel_diametro, col="blue", ylim=c(0,0.5), ylab = "Frecuencia", xlab= "Intervalos diametro")
title_diam = strsplit(c("Diametro en metros de los arboles; de la ciudad de Buenos Aires"), split = ";")
hist(diametro, col="red", ylim=c(0,170), ylab="Frecuencia", xlab="Diametro", main=title_diam)

#---------------------------------------INCLINACION-------------------------------------------------
# tabla de frecuencia inclinacion
breaks_inclinacion = seq(0, max(inclinacio)+5, 5) # Intervalos para la tabla de freq
rango_inclinacion = cut(inclinacio, breaks = breaks_inclinacion, right = FALSE)
freq_abs_inclinacion = table(rango_inclinacion)
freq_rel_inclinacion = round(freq_abs_inclinacion / sum(freq_abs_inclinacion), 4)
freq_abs_ac_inclinacion = cumsum(freq_abs_inclinacion)
freq_rel_ac_inclinacion = cumsum(freq_rel_inclinacion)
porc_inclinacion = freq_rel_inclinacion * 100
porc_ac_inclinacion = freq_rel_ac_inclinacion * 100
tabla_inclinacion = cbind(freq_abs_inclinacion, freq_rel_inclinacion, porc_inclinacion, freq_abs_ac_inclinacion,
                       freq_rel_ac_inclinacion, porc_ac_inclinacion)
total_inclinacion = apply(tabla_inclinacion, 2, sum)
tabla_inclinacion_total = rbind(tabla_inclinacion, total_inclinacion)




dotchart(freq_abs_inclinacion)
barplot(freq_abs_inclinacion, xlab = "Inclinacion", ylab="Frecuencia", ylim = c(0,300)
        , col="red")
## FALTA GRAFICO
summary(inclinacio)





#---------------------------------------ORIGEN-------------------------------------------------
#grafico para las origen
origen_order = ordered(origen, levels = c("Nativo/Aut¾ctono", "Ex¾tico"))
freq_abs_origen = table(origen_order)
freq_rel_origen = prop.table(table(origen_order))
porc = freq_rel_origen * 100
tabla_origen = cbind(freq_abs_origen, freq_rel_origen, porc)

freq_abs_origen = table(origen_order)
# tabla de frecuencia origen
freq_rel_origen = round(freq_abs_origen / sum(freq_abs_origen), 9) # un 9 xq sino no da exacta la tabla
freq_abs_ac_origen = cumsum(freq_abs_origen)
freq_rel_ac_origen = cumsum(freq_rel_origen)
porc_origen = freq_rel_origen * 100
porc_ac_origen = freq_rel_ac_origen * 100
tabla_origen = cbind(freq_abs_origen, freq_rel_origen, porc_origen)
total_origen = apply(tabla_origen, 2, sum)
tabla_origen_total = rbind(tabla_origen, total)

#grafico de torta para el origen
colores = c("red", "blue")

porcentajes <- round(freq_rel_origen *100, 1)
etiquetas_origen = c("Nativo Autoctono", "Exotico")
pie(freq_rel_origen, labels = paste(etiquetas_origen, porcentajes, "%"), col = colores)

#pie(freq_abs_origen, col = colores)

#---------------------------------------BROTES-------------------------------------------------
# tabla de frecuencia brotes
breaks_brotes = seq.int(min(brotes), max(brotes), 1) # Intervalos para la tabla de freq
#breaks_brotes = c(1:max(brotes), 1)
rango_brotes = cut(brotes, breaks = breaks_brotes, right = TRUE)
freq_abs_brotes = table(rango_brotes)
freq_rel_brotes = round(freq_abs_brotes / sum(freq_abs_brotes), 4)
freq_abs_ac_brotes = cumsum(freq_abs_brotes)
freq_rel_ac_brotes = cumsum(freq_rel_brotes)
porc_brotes = freq_rel_brotes * 100
porc_ac_brotes = freq_rel_ac_brotes * 100
tabla_brotes = cbind(freq_abs_brotes, freq_rel_brotes, porc_brotes, freq_abs_ac_brotes,
                          freq_rel_ac_brotes, porc_ac_brotes)
total_brotes = apply(tabla_brotes, 2, sum)
tabla_brotes_total = rbind(tabla_brotes, total_brotes)

#barplot(freq_abs_brotes, names = c(1, 2, 3, 4,5,6,7,8), ylim = c(0,120), col = "green", ylab="Frecuencia")

summary(brotes)
# Histograma de brotes
hist(brotes, breaks = breaks_brotes, col="green", ylab="Frecuencia",
     xlab="Brotes", ylim=c(0,140))

#axis(2, at=c(seq(0,120,10)))
#---------------------------------------comparar brotes con diametro-------------------------------------------------
# Crear los datos
datos1 <- c(10, 20, 30, 40, 50)
datos2 <- c(100, 200, 300, 400, 500)
nombres <- c("A", "B", "C", "D", "E")

# Crear el gráfico de barras agrupadas
#barplot(
  #rbind(freq_rel_diametro, freq_rel_altura),
  #beside = TRUE,
  #names.arg = nombres,
  #ylab = "Datos 1 / Datos 2",
  #col = c("blue", "red")
#)
legend("topright", legend = c("Datos 1", "Datos 2"), fill = c("blue", "red"))

