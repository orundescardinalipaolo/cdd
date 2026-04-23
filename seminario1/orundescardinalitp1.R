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

#Busqué para que sirve cada uno y en resumen es:
# car: paquete orientado a regresión aplicada y análisis estadístico, con funciones para diagnóstico de modelos.
# mosaic: paquete pensado para facilitar el aprendizaje y uso de estadística, cálculo y modelado de manera integrada.
# probs: paquete vinculado a probabilidad, útil para trabajar con distribuciones y simulaciones.
# ISLR: colección de datasets para acompañar el libro An Introduction to Statistical Learning with Applications in R.
# multcomp: paquete destinado a comparaciones múltiples en modelos estadísticos.


#Ejercicio 4. Trabajo con un dataset: case0102

#a) Utilice los comandos y comente lo que realiza cada uno library(Sleuth3), data(case0102), View(case0102) head(case0102), str(case0102).

install.packages("Sleuth3")
library(Sleuth3)
help(package = "Sleuth3")

data(case0102) 
#Cuando ejecuto este entiendo que lo inicializa o carga para en memoria para ser usado, la consolta no me devolvió nada pero si vi en el Envirnment que cargo case0102 <Promise>

View(case0102) 
#Cuando ejecture este me abrió una pestaña con una planilla de dos columnas (salary, sex) con 93 registros

head(case0102)
# Me mostró las primeras 6 filas del dataset
# > head(case0102)
# Salary    Sex
# 1   3900 Female
# 2   4020 Female
# 3   4290 Female
# 4   4380 Female
# 5   4380 Female
# 6   4380 Female


str(case0102)
# En la salida por consola me mostró
# > str(case0102)
# 'data.frame':	93 obs. of  2 variables:
#   $ Salary: int  3900 4020 4290 4380 4380 4380 4380 4380 4440 4500 ...
# $ Sex   : Factor w/ 2 levels "Female","Male": 1 1 1 1 1 1 1 1 1 1 ...
# Me mostró la estructura del dataset: tipo de objeto: cantidad de observaciones de variables y tipo de cada variable (int a salary y Factor a Sex, con dos niveles).

#b) ¿Cuáles son las variables que forman esta base de datos?
names(case0102)
#Respuesta: [1] "Salary" "Sex" 

#c) Visualice el encabezado de los datos
head(case0102)

# d) Visualice la descripción de los datos
# help(case0102) o también ?case0102
?case0102
#Me mostró en la pestaña Help la descripción de los datos

#e) Determine la dimensión de la matriz de datos
#se puede ejecutar también nrow(case0102) ncol(case0102)
dim(case0102)
#Respuesta: [1] 93  2 (93 filas x 2 columnas)

#f) ¿Cuáles son las variables cualitativas de la base de datos? ¿Qué categorías tienen?
str(case0102)

# 'data.frame':	93 obs. of  2 variables:
#   $ Salary: int  3900 4020 4290 4380 4380 4380 4380 4380 4440 4500 ...
# $ Sex   : Factor w/ 2 levels "Female","Male": 1 1 1 1 1 1 1 1 1 1 ...
#puedo usar las variabbles como case0102$Sex o Sex

#g) La variable Salary, ¿tiene valores faltantes?
#Para verificar is.na(case0102$Salary)
#Para saber cuántos faltantes hay sum(is.na(case0102$Salary)) o any(is.na(case0102$Salary))

is.na(case0102$Salary)
# [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [14] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [27] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [40] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [53] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [66] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [79] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [92] FALSE FALSE

sum(is.na(case0102$Salary))
#[1] 0 Si el resultado es 0, entonces no tiene valores faltantes.

any(is.na(case0102$Salary))
#[1] FALSE (TRUE sí hay faltantes, FALSE no hay faltantes)

#h) Obtenga el salario medio por sexo
# Salary ~ Sex se lee como calcular Salary agrupando por Sex de la base (data = case0102) 
# mean(...) calcula la media del salario para cada categoría de sexo.
mean(Salary ~ Sex, data = case0102)
#Me retornó: 
# Aviso:
# In mean.default(Salary ~ Sex, data = case0102) :
#   argument is not numeric or logical: returning NA

#Investigo el aviso y es porque me faltó cargar la librería mosaic
library(mosaic)
mean(Salary ~ Sex, data = case0102)
#Resultado:  
#Devuelve el salario promedio de mujeres y varones.
# Female     Male 
# 5138.852 5956.875 

#i) Cree un vector que almacene el salario (Salary) para los varones
salario_varones <- case0102$Salary[case0102$Sex == "Male"]
salario_varones
#Resultado:
# [1] 4620 5040 5100 5100 5220 5400 5400 5400 5400 5400 5700 6000 6000 6000 6000 6000
# [17] 6000 6000 6000 6000 6000 6000 6000 6000 6300 6600 6600 6600 6840 6900 6900 8100


#j) Cree un vector que almacene el sexo (Sex) para aquellos empleados con Salary mayor a 6000
sexo_mayor_6000 <- case0102$Sex[case0102$Salary > 6000]
sexo_mayor_6000
#[1] Female Female Female Male   Male   Male   Male   Male   Male   Male   Male 
#Levels: Female Male //NO ENTENDÍ ÉSTA SALIDA  

# k) Visualice la variable Salary para aquellos empleados correspondientes a los índices 2, 10, 26 y 55
salarios_indices <- case0102$Salary[c(2, 10, 26, 55)]
salarios_indices
#Respuesta: [1] 4020 4500 5100 5700
