# ejercicio 1

# La variable en estudio son las edades. Es una variable de tipo
# cuantitativa continua.


edades <- c(22, 22, 23, 24, 25, 25, 26, 27, 28, 29, 29, 29, 29, 29, 31, 31, 32,
           33, 34, 35, 35,35, 36, 38, 39, 39, 42, 42, 44, 44, 45, 45, 45, 47,
           48, 52, 59, 66, 67, 69, 69)

tam_muestra <- length(edades)
media <- mean(edades)
mediana <- median(edades)


moda <- function(vector, longitud){
  valor_idx_actual <- 0
  valor2 <- 0
  moda_actual <- 0
  apariciones_moda <- 0
  apariciones_actual <- 1
  elementos_chequeados <- c()
  for (idx in 1:longitud) {
    valor_idx_actual <- vector[idx]
    apariciones_actual <- 1
    if(valor_idx_actual %in% elementos_chequeados){
      next
    }
    else{
      elementos_chequeados <- c(elementos_chequeados, valor_idx_actual)
      for(idx2 in idx:longitud){
        valor2 <- vector[idx2]
        if(valor_idx_actual == valor2){
          apariciones_actual <- apariciones_actual + 1
        }
        if(apariciones_actual > apariciones_moda){
          moda_actual <- valor_idx_actual
          apariciones_moda <- apariciones_actual
        }
      }
    }
  }
  return(moda_actual)
}

moda <- moda(edades, tam_muestra)
print(moda)


