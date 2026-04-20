# Maestría en Ciencia de Datos - FCyT - UADER
# Seminario 1. Técnicas de Análisis en Ciencias de Datos
# Trabajo Práactico 1. Introducción a RStudio. Operaciones con Vectores, Matrices y Datasets
# 
# ORUNDES CARDINALI PAOLO
# 
# Ejercicico 1. Vectores y Matrices
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

