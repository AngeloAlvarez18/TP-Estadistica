#---------------------------------------PREGUNTAS-------------------------------------------------
# 1. en la tabla de frecuencia, frecuencia relativa termina en 1.0001 o 0.9999, bien / mal?
# 2. como poner q se haga la suma acumulativa solamente de las primeras 3 columnas
# 3. cada cuanto tomar los intervalos (si tomo uno en especifico, los ultimos valores del 
# intervalo no los toma)

#-------------------------------------------------------------------------------------------------
library(readxl)
datos <- read_excel("Base4.xls", col_names = TRUE)
#acceder a las columnas por nombre
attach(datos) 

#---------------------------------------ALTURA-------------------------------------------------
#summary(altura)
length_interval = trunc(sqrt(length(altura)))
breaks_altura = seq(min(altura), max(altura), 5) # Intervalos para la tabla de freq
#breaks_altura = seq(min(altura), max(altura), 5) # Intervalos para la tabla de freq

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

#--------------------------------------------
# tabla de frecuencia altura
freq_rel_altura = round(freq_abs_altura / sum(freq_abs_altura), 4)
freq_abs_ac_altura = cumsum(freq_abs_altura)
freq_rel_ac_altura = cumsum(freq_rel_altura)
porc_altura = freq_rel_altura * 100
porc_ac_altura = freq_rel_ac_altura * 100
tabla_altura = cbind(freq_abs_altura, freq_rel_altura, porc_altura, freq_abs_ac_altura,
                     freq_rel_ac_altura, porc_ac_altura)
total_altura = apply(tabla_altura, 2, sum)
tabla_altura_total = rbind(tabla_altura, total_altura)
#--------------------------------------------

#grafico para las especies
especie_order = ordered(especie, levels = c("Ceibo", "Eucalipto", "Álamo", "Palo borracho",
                                      "Acacia", "Jacarandá", "Casuarina", "Ficus", "Fresno"))
freq_abs = table(especie_order)
freq_rel = prop.table(table(especie_order))
porc = freq_rel * 100
tabla = cbind(freq_abs, freq_rel, porc)


#plot(altura, inclinacio)

#---------------------------------------ESPECIE-------------------------------------------------

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

#--------------------------------------------
# tabla de frecuencia diametro
breaks_diametro = seq(min(diametro), max(diametro), 20) # Intervalos para la tabla de freq
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

