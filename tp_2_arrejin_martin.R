# ==============================================================================
# TRABAJO PRÁCTICO 2: Estimación, Correlaciones y Pruebas de Hipótesis
# Maestría en Ciencia de Datos - FCyT - UADER
# ==============================================================================

# a) Ejecute comandos para visualizar su base de datos y las variables: attach(datos), View(datos), head(datos),
#str(datos).

datos <- read.table("Student0405.txt", header = TRUE, sep = "\t") 
# --- Punto a) Visualización inicial ---
attach(datos)
View(datos)
head(datos)
str(datos)

#b) Identifique tamano muestral y cantidad de variables.
dimensiones <- dim(datos) #dimención 
names(dimensiones) <- c("Filas (Tamaño Muestral)", "Columnas (Variables)")
print(dimensiones)


#d) Obtenga un resumen numerico para GPA que contenga: mınimo, maximo, q1, q3, mediana, media, desvıo
#estandar y varianza. Ademas, realice dos graficos distintos que le permitan describir la distribucion de esta
#variable. ¿Que puede decir acerca de su distribucion?
# Cálculo promedio
resumen <- summary(GPA)
desvio <- sd(GPA, na.rm = TRUE)
varianza <- var(GPA, na.rm = TRUE)
print(resumen)
which(is.na(GPA)) #para saber que filar tiene datos N'A en nuetro caso eran 3 
cat("Desvío Estándar:", desvio, "\n")
cat("Varianza:", varianza, "\n")
# Quitamos el mfrow para que use toda la pantalla
par(mar = c(5, 4, 4, 2) + 0.1)

hist(GPA, 
     main = "Histograma de GPA", 
     xlab = "Promedio de Calificaciones (GPA)",
     ylab = "Frecuencia (Cantidad de alumnos)",
     col = "skyblue", 
     border = "white",
     xaxt = "n") # 'n' significa 'none', quita los números de abajo

# 2. Ponemos los números nosotros, marcando cada 0.5 (1.5, 2.0, 2.5, 3.0, etc.)
axis(side = 1, at = seq(1.5, 4.0, by = 0.5))

#Se observa que la mayoría de los estudiantes se agrupan en las barras superiores a 3.0.
#El gráfico muestra una asimetría negativa (hacia la izquierda), ya que la "cola" de la distribución se extiende hacia las notas más bajas. 
#Esto indica que hay pocos alumnos con notas bajas y muchos con notas altas.
#Si esta este estudio sumamos la desviación estandar
#nos dio que entre el 2.7 y el 3.6 (que es el rango de Promedio ± 1 Desvío) 
#confirma que los datos tienen una dispersión baja
#El desvío estándar de 0.45 se refleja en el histograma 
#como una concentración alta de alumnos alrededor del promedio.


boxplot(GPA, 
        main = "Distribución del Rendimiento Académico (GPA)",
        ylab = "Promedio de Calificaciones",
        col = "lightcoral",
        border = "brown",
        horizontal = FALSE,
        notch = TRUE,        # Agrega una muesca en la mediana
        las = 1)             # Pone los números del eje Y horizontales

#La caja muestra que el 50% central de los alumnos tiene un rendimiento muy parejo (entre 2.93 y 3.51). 
#Lo más destacable son los 4 valores atípicos (outliers) representados por los puntos inferiores (por debajo de 2.1). 
#Estos alumnos tienen un rendimiento inusualmente bajo en comparación con la tendencia general del curso.

#e) Realice un resumen num´erico y un gr´afico que le permita conjeturar si las mujeres van m´as a fiestas que los
#varones. Describa lo que observa.