library(readxl)
datos <- read_excel("Base4.xls", col_names = TRUE)
#acceder a las columnas por nombre
attach(datos) 

#summary(altura)
length_interval = trunc(sqrt(length(altura)))
breaks_altura = seq(min(altura), max(altura), length_interval) # Intervalos para la tabla de freq
#breaks_altura = seq(min(altura), max(altura), 5) # Intervalos para la tabla de freq

#install.packages("qcc")
#library(qcc)

algo = cut(altura, breaks = breaks_altura, right = TRUE)

freq_abs_altura = table(algo)
tam_muestra_edades = length(altura)
ancho_barras = sqrt(tam_muestra_edades)

mean(altura)
median(altura)
hist(altura, breaks = length_interval, col = "pink", ylab = "Cantidad")
freq_rel = prop.table(altura)
freq_rel_acum = cumsum(freq_rel)
hist(freq_rel, breaks = length_interval, col = "red", ylab = "Cantidad", right = FALSE)
#preguntar por grafico de freq_rel_acum

especie_order = ordered(especie, levels = c("Ceibo", "Eucalipto", "Álamo", "Palo borracho",
                                      "Acacia", "Jacarandá", "Casuarina", "Ficus", "Fresno"))
freq_abs = table(especie_order)
freq_rel = prop.table(table(especie_order))
porc = freq_rel * 100
tabla = cbind(freq_abs, freq_rel, porc)


plot(altura, inclinacio)
