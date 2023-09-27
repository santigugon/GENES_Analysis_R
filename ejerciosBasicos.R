---
  title: "My PDF Report with R Markdown"
author:
  - "Santiago Gutierrez Gonzalez"
output: pdf_document
fontsize: 12
---

# Ej 1. Crear vector x
set.seed(124) # Ponemos semilla para obtener resultados aleatorios pero que obtengamos los mismos numeros aleatorios
x <- sample(-100:50, 10)
print(x)

#Ej 2. Calcular media, desviación estándar y varianza
media_x <- mean(x)
print(media_x)
desv_x <- sd(x)
print(desv_x)
var_x <- var(x)
print(var_x)

#Ej  3. Crear vector est.x
est.x <- c(media_x, desv_x, var_x)
print(est.x)

#Ej  4. Calcular suma de x
suma_x <- sum(x)
print(suma_x)

#Ej 5. Valor máximo y mínimo
max_x <- max(x)
print(max_x)
min_x <- min(x)
print(min_x)

#Ej 6. Crear vectores a y b, multiplicarlos y mostrar resultado
a <- sample(1:10, 2)
print(a)
b <- sample(1:10, 2)
print(b)
mult_ab <- a * b
print(mult_ab)

#Ej 7. Crear vector z
z <- seq(1, 10, 0.1)
print(z)

#Ej 8. Crear matriz m
m <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
print(m)

#Ej 9. Crear marco de datos mdd
mdd <- data.frame(col1 = 1:4, col2 = c("a", "b", "c", "d"), col3 = c(FALSE, FALSE, TRUE, TRUE))
#La columna 1 es de numeros,la columna 2 de caracter y la columna 3 de datos booleanos 

#Ej 10. Crear lista con objetos e imprimir contenido
lista <- list(vector_x = x, vector_est_x = est.x, vector_a = a, vector_b = b, secuencia_z = z, matriz_m = m, marcodeDatos_mdd = mdd)
#En esta lista usamos muchos tipos de objetos, como los son vectores, matrices, marcos de Datos,etc
print(lista)

1<-c(1:5)
t <- c(1, 2, 3, 4, 5)
print(class(t))

      