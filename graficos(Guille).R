#---------------------------------------PREGUNTAS-------------------------------------------------
# 2. como poner q se haga la suma acumulativa solamente de las primeras 3 columnas
# 3. cada cuanto tomar los intervalos (si tomo uno en especifico, los ultimos valores del 
# intervalo no los toma)

#-------------------------------------------------------------------------------------------------
library(readxl)
datos <- read_excel("Base4.xls", col_names = TRUE)
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

hist(altura, breaks = 8, col = "tan", ylab = "Cantidad",
    xlab = "Altura", main = "Frecuencia absoluta de la altura", axes = FALSE)
axis(1, at=breaks_altura)
axis(2, at=c(seq(0,100,10)))

lines(altura, col = "red", lwd = 2)




#====

x <- 1:10
y <- c(1, 3, 2, 5, 4, 6, 8, 7, 9, 10)

# Crear gráfico de líneas
plot(altura, c(0,350), type = "l", col = "blue", xlab = "Eje X", ylab = "Eje Y", main = "Gráfico de líneas")


x=c(5,10,15,20,25,30,35,40)
y=c(0.05,0.1, 0.15,0.20, 0.25, 0.3)
plot(x,freq_rel_altura,type="l",xlab="Altura (metros)",ylab="Frecuencia relativa",main="Altura total (en metros) de árboles",xaxt="n",yaxt="n")
axis(side=1,c(5,10,15,20,25,30,35,40),labels=TRUE, ylim = c(0,0.3))
#axis(side=2, at=seq(0,0.25,0.5), labels=seq(0,0.25,0.5), labels=TRUE)
axis(side=2,y,labels=TRUE,las=2)
#====


freq_rel_altura = prop.table(altura)
freq_rel_acum_altura = cumsum(freq_rel_altura)
# histograma de frecuencia relativa altura
hist(freq_rel_altura, breaks = 8, col = "red", ylab = "Cantidad", xlab = "Altura",right = FALSE)


boxplot(freq_rel_altura, col = "orange", axes = FALSE)
axis(2, at = c(seq(0,1,0.001)))


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

valores <- c(seq(0,80,10))
paleta <- colorRampPalette(colors = c("orange", "red"))
par(cex.axis = 0.7)
barplot(freq_abs_especie, ylab = "Cantidad de árboles", main = "Cantidad de árboles segun su especie", las = 2,
        axes = FALSE, col = paleta(9))
axis(side = 2, at=valores, labels = valores)


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
hist(freq_rel_diametro, col = "blue")


#==============================grafico de poligono de diametro====================
title_hist_diametro = strsplit(c("Distribución de la frecuencia del ;diámetro de los árboles"), split = ";")
plot(freq_rel_diametro, type = "l", xaxt="n", main = title_hist_diametro, ylab = "Frecuencia relativa", xlab = "(En cm)",
     sub = fuente, cex.sub = 0.7)
axis(side = 1, at = 1:14, labels = breaks_diametro)
grid()



frec_abs_diam <- table(diametro)
frec_rel_diametro <- round(frec_abs_diam / sum(frec_abs_diam), 2)

plot(cumsum(frec_rel_diametro), type = "l", 
     xlab = "Diámetro", ylab = "Frecuencia relativa acumulada", xaxt = "n", 
     font.lab = 2, col = "red")

axis(side = 1, c(0,25, 50,75,100), labels = TRUE)


x <- freq_rel_diametro
n <- length(x)
freq_relativa <- table(x) / n

# Crear gráfico de dispersión con frecuencia relativa
plot(names(freq_relativa), freq_relativa, type = "b", col = "blue", ylim = c(0, max(freq_relativa) + 0.1),
     xlab = "Variable", ylab = "Frecuencia relativa", main = "Gráfico de dispersión con frecuencia relativa")
#==========================================================================================

hist(diametro, col="red", ylab="Frecuencia", xlab="Diámetro", axes = FALSE, main = "Histograma del diametro")
axis(side = 1, c(seq(0,260,40)))
axis(side = 2, at=c(seq(0,150,50)))
axis(1, c(20, 60, 100, 140,180,220, 260))

#---------------------------------------INCLINACION-------------------------------------------------
# tabla de frecuencia inclinacion
breaks_inclinacion = seq(0, max(inclinacio), 5) # Intervalos para la tabla de freq
rango_inclinacion = cut(inclinacio, breaks = breaks_inclinacion, right = TRUE)
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


hist(freq_abs_inclinacion)


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
pie(freq_rel_origen, labels = paste(etiquetas_origen, porcentajes, "%"),main = "Distribución de los árboles según su origen", col = colores)

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

hist(freq_abs_brotes, ylab = "Cantidad de brotes", xlab = "Frecuencia de arboles", col = "green")


barplot(freq_abs_brotes, names = c(1, 2, 3, 4,5,6,7,8), ylim = c(0,120), col = "green", ylab="Frecuencia")
axis(2, at=c(seq(0,120,10)))
summary(brotes)
#---------------------------------------comparar Altura con Especie-------------------------------------------------

boxplot(altura~especie, col = "purple", xlab = "", ylab = "Altura", las = 2, 
        names = c("Ceibo", "Eucalipto", "Álamo", "P.B.",
                  "Acacia", "Jacarandá", "Casuarina", "Ficus", "Fresno"))

#---------------------------------------comparar cantidad de brotes con Especie-------------------------------------------------
boxplot(brotes~especie, col = "magenta", xlab = "", ylab = "Brotes", las = 2, 
        names = c("Ceibo", "Eucalipto", "Álamo", "P.B.",
                  "Acacia", "Jacarandá", "Casuarina", "Ficus", "Fresno"))
axis(2, at=seq(1,8,1), las = 2)
#---------------------------------------comparar altura con Origen-------------------------------------------------

boxplot(altura~origen, col = rainbow(2), xlab = "Origen", ylab = "Altura", names = c("Exótico", "Nativo Autóctono"), las = 1)
axis(2, at=seq(0,40,5), las = 1)

#"Ex¾tico", "Nativo/Aut¾ctono"



