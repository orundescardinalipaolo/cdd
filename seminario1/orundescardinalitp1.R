# Maestría en Ciencia de Datos - FCyT - UADER
# ORUNDES CARDINALI PAOLO
# COMENTAR BLOQUES DE CÓDIGO CONTROL+SHITF+C
# AUTOAJUSTAR TEXTO (QUITAR BARRA HORIZONTAL) TOOLS / GLOBAL OPTIONS / CODE / DISPLAY / SOFT-WRAP SOURCE FILES
#CODIGO EN REPO https://github.com/orundescardinalipaolo/cdd


# Seminario 1. Técnicas de Análisis en Ciencias de Datos
# Trabajo Práactico 1. Introducción a RStudio. Operaciones con Vectores, Matrices y Datasets

# Ejercicio 1. Vectores y Matrices
# a) Construya un vector cuyas componentes sean: -1, 1, 2, 6, 10, 15, 21, 25, 100.

vectorA <- c(-1, 1, 2, 6, 10, 15, 21, 25, 100)
print(vectorA)

# b) Construya un vector cuyas componentes sean los números enteros entre 0 y 50.
vectorB <- 0:50 #tambien puede ser vectorB <- seq(0, 50)
print(vectorB)

# c) Construya un vector cuyas componentes sean números reales entre 0 y 1 utilizando un incremento de 0,01. # ¿Qué longitud tiene este vector?
vectorC <- seq(0,1, by=0.01)
print(vectorC)
length(vectorC)

#d) Construya un vector que repita 100 veces el número 2.
vectorD <- rep(2,100)
print(vectorD)

#e) Construya un vector categórico cuyas componentes sean: Lunes, Martes, Miercoles, Jueves, Viernes.

vectorE <- factor(c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes"))
print(vectorE)

# f) Construya las matrices A, B, C y E
A <- matrix(c(2, 4, 6,
              8, 10, 12), 
            nrow = 2, byrow = TRUE)

B <- matrix(c(2, 6,
              4, 8), 
            nrow = 2, byrow = TRUE)

C <- matrix(c(1, 2, 3,
              4, 5, 6,
              -2, -4, -6), 
            nrow = 3, byrow = TRUE)

E <- matrix(0, nrow = 4, ncol = 5)

print(A)
print(B)
print(C)
print(E)

# Ejercicio 2. Operaciones con Vectores y Matrices Indique:

# a) Las dimensiones de la matriz A.
#Error: objeto 'A' no encontrado si da ese error es porque hay que ejecutar la linea 36 (CONTROL + ENTER) y luego si ejecutar dim(A)
dim(A) 
#Resultado: [1] 2 3

# b) La componente c2,2 de la matriz C.
C[2,2] 
#Resultado: [1] 5

# c) La fila 2 de la matriz A.
A[2, ] 
#Resultado: [1]  8 10 12

# d) La columna 3 de la matriz C.
C[, 3]
#Resultado: [1]  3  6 -6

# e) La matriz que resulta de quitar la fila 2 de la matriz C.
C[-2, ]
#Resultado:      [,1]  [,2]  [,3]
#           [1,]    1    2    3
#           [2,]   -2   -4   -6

# f) La matriz que resulta de quitar la columna 3 de la matriz A.
A[, -3]
#Resultado: [,1] [,2]
#     [1,]    2    4
#     [2,]    8   10

# g) La matriz que resulta de agregar la columna (1, 1)t a la matriz A.
cbind(A, c(1,1))
#Resultado: [,1] [,2] [,3] [,4]
#     [1,]    2    4    6    1
#     [2,]    8   10   12    1


# h) La matriz que resulta de agregar la fila (10, 11, 12) a la matriz C.
rbind(C, c(10,11,12))

# i) Calcule, si es posible: AC, -3B.
#Si hago elemento por elemento
A * C
#Error en A * C: arreglos de dimensón no compatibles

#En cambio si hago el producto matricial
A %*% C
# Resultado: [,1] [,2] [,3]
#     [1,]    6    0   -6
#     [2,]   24   18   12

#acá tengo dudas como resolverlo

#Producto -3B
-3 * B
#Resultado: [,1] [,2]
#     [1,]   -6  -18
#     [2,]  -12  -24

# j) Divida cada elemento de la matriz B por 10.
B / 10
#Resultado: [,1] [,2]
#     [1,]  0.2  0.6
#     [2,]  0.4  0.8

# k) La inversa de la matriz B y la inversa de la matriz C, la inversa de la matriz EEt en caso de que existan.

solve(B)
solve(C)
EEt <- E %*% t(E)
EEt

# > solve(B)
#       [,1]  [,2]
# [1,] -1.0  0.75
# [2,]  0.5 -0.25

# > solve(C)
# Error en solve.default(C): 
#   Lapack routine dgesv: system is exactly singular: U[3,3] = 0
det(C)
#[1] 0 La determinante es = 0, entiendo que ese es el error


# > EEt <- E %*% t(E)
# > EEt
#       [,1] [,2] [,3] [,4]
# [1,]    0    0    0    0
# [2,]    0    0    0    0
# [3,]    0    0    0    0
# [4,]    0    0    0    0


# Ejercicio 3. Inspección de paquetes

#para instalar en una única línea se puede hacer así: install.packages(c("car", "mosaic", "probs", "ISLR", "multcomp")) o bien instalar paquete por paquete install.packages("car") ...

install.packages(c("car", "mosaic", "probs", "ISLR", "multcomp"))

library(car)
library(mosaic)
library(probs)
library(ISLR)
library(multcomp)

help(package = "car")
help(package = "mosaic")
help(package = "probs")
help(package = "ISLR")
help(package = "multcomp")
