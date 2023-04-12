#---------------------------------------PREGUNTAS-------------------------------------------------
# 1. en la tabla de frecuencia, frecuencia relativa termina en 1.0001 o 0.9999, bien / mal?
# 2. como poner q se haga la suma acumulativa solamente de las primeras 3 columnas
# 3. cada cuanto tomar los intervalos (si tomo uno en especifico, los ultimos valores del 
# intervalo no los toma)
# 4. box plot para variables continuas, pero no hay (nora dijo de usar eso)
# 5. en la funcion seq hace intervalo abierto a izquierda, pero ese valor se necesita (como se arregla)
# 6. preguntar por comparacion de graficos, error "number of columns of result is not a multiple of vector length"

#-------------------------------------------------------------------------------------------------
library(readxl)
datos <- read_excel("Base4.xls", col_names = TRUE)
#acceder a las columnas por nombre
attach(datos) 

#---------------------------------------ALTURA-------------------------------------------------

#--------------------------------------------
# tabla de frecuencia altura
breaks_altura = seq(0, max(altura), 5) # Intervalos para la tabla de freq

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
rango_altura = cut(altura, breaks = breaks_altura, right = TRUE)
freq_abs_altura = table(rango_altura)

mean(altura)
median(altura)
# histograma de altura

hist(altura, breaks = length_interval, col = "green", ylab = "Cantidad",
    xlab = "Altura", main = "Frecuencia absoluta de la altura")


freq_rel = prop.table(altura)
freq_rel_acum = cumsum(freq_rel)
# histograma de frecuencia relativa altura
hist(freq_rel, breaks = length_interval, col = "red", ylab = "Cantidad", xlab = "Altura",right = FALSE)
#preguntar por grafico de freq_rel_acum



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

#---------------------------------------INCLINACION-------------------------------------------------
# tabla de frecuencia inclinacion
breaks_inclinacion = seq(0, max(inclinacion), 5) # Intervalos para la tabla de freq
rango_inclinacion = cut(inclinacion, breaks = breaks_inclinacion, right = TRUE)
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
barplot(freq_abs_inclinacion)
## FALTA GRAFICO






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

barplot(freq_rel_brotes)


#---------------------------------------comparar brotes con diametro-------------------------------------------------
# Crear los datos
datos1 <- c(10, 20, 30, 40, 50)
datos2 <- c(100, 200, 300, 400, 500)
nombres <- c("A", "B", "C", "D", "E")

# Crear el gráfico de barras agrupadas
barplot(
  #rbind(datos1, datos2),
  rbind(freq_rel_diametro, freq_rel_altura),
  beside = TRUE,
  names.arg = nombres,
  ylab = "Datos 1 / Datos 2",
  col = c("blue", "red")
)
legend("topright", legend = c("Datos 1", "Datos 2"), fill = c("blue", "red"))

