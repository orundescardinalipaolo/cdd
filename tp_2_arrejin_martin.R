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

#e) Realice un resumen numerico y un grafico que le permita conjeturar si las mujeres van mas a fiestas que los
#varones. Describa lo que observa.

# Resumen numérico: media y cuartiles por sexo
aggregate(PartyDays ~ Sex, data = datos, FUN = summary)

# Gráfico: Boxplot comparativo
boxplot(PartyDays ~ Sex, 
        main = "Días de fiesta por semana según Sexo",
        ylab = "Días de fiesta", 
        col = c("lightpink", "lightblue"))

#A partir del análisis realizado, se conjetura que los varones asisten a fiestas 
#con una frecuencia levemente mayor que las mujeres, dado que su mediana es superior. 
#No obstante, el grupo femenino presenta una mayor variabilidad, 
#evidenciada en una caja más alta; esto indica que el 50% central de 
#las mujeres tiene hábitos sociales muy diversos entre sí. 
#Por el contrario, el grupo masculino es más homogéneo (caja más compacta), 
#concentrando a la mayoría de sus integrantes en un rango de días más estrecho,
#con la excepción de tres casos atípicos que presentan frecuencias de 
#salida extremadamente elevadas

#f) Realice un resumen numerico y un grafico que le permita conjeturar si las calificaciones GPA dependen de la
#ubicacion de aula del alumno. Describa lo que observa.

# Resumen numérico: promedios por ubicación
tapply(GPA, Seat, mean, na.rm = TRUE)

# Gráfico: Boxplot por ubicación
boxplot(GPA ~ Seat, 
        main = "Relación entre GPA y Ubicación en el aula",
        col = "lightgreen", 
        ylab = "Calificaciones (GPA)")

# Se observa  en relación a la mediana que el grupo "Front" posee la calificación más alta, 
#seguido por "Middle" y finalmente "Back". Esto permite conjeturar que la cercanía al frente se asocia con un mejor GPA.
# El grupo "Back" es el más HOMOGÉNEO (caja más compacta), lo que indica que 
# las notas de estos alumnos son más parecidas entre sí. 
# Los grupos "Front" y "Middle" presentan mayor HETEROGENEIDAD (cajas más altas),
# reflejando un rendimiento académico más diverso en esas zonas. Por otro lado, 
# El grupo "Middle" presenta la mayor cantidad de valores atípicos inferiores, 
# con alumnos que poseen notas significativamente bajas (entre 1.5 y 2.0). 
# El grupo "Back" no presenta valores atípicos, siendo el más predecible.
# Por útlimo, se observa una categoría sin etiqueta (linea gruesa) que corresponde a 
#los valores 'NA' de la tabla original 


#Realice una tabla de frecuencias absolutas y relativas para la variable Seat y realice dos graficos distintos
#que le permitan visualizar esta variable. Describa lo que observa

# 2. Tablas de Frecuencia
tabla_abs <- table(datos$Seat)
tabla_rel <- prop.table(tabla_abs)
tabla_pct <- round(tabla_rel * 100, 1) # Porcentajes redondeados

# Aumentamos los márgenes y dividimos en 1 fila x 2 columnas

# 3. Configuración de la ventana gráfica
# Aumentamos los márgenes y dividimos en 1 fila x 2 columnas
par(mfrow=c(1,2), mar=c(5, 4, 4, 2) + 0.1)

# GRÁFICO 1: Diagrama de Barras
# Usamos ylim para dar aire arriba y que entren bien los títulos
bp <- barplot(tabla_abs, 
              main="Frecuencias Absolutas", 
              col=c("#FC8D62", "#66C2A5", "#8DA0CB"), # Colores profesionales
              ylab="Cantidad de Alumnos",
              xlab="Ubicación",
              ylim=c(0, max(tabla_abs) * 1.2))
# Truco pro: ponemos el número exacto arriba de cada barra
text(x = bp, y = tabla_abs, label = tabla_abs, pos = 3, cex = 0.8)

# GRÁFICO 2: Gráfico de Torta
# Usamos 'cex' para achicar la letra y 'radius' para agrandar el círculo
pie(tabla_rel, 
    main="Proporción (%)", 
    col=c("#FC8D62", "#66C2A5", "#8DA0CB"),
    labels = paste0(names(tabla_pct), "\n", tabla_pct, "%"),
    cex = 0.9,      # Tamaño de la letra de las etiquetas
    radius = 1.0)   # Tamaño de la torta

# Reset de la configuración
par(mfrow=c(1,1))

# Se observa un claro predominio de la zona media (Middle), que concentra
# casi el 59% del alumnado, seguido por el front con un 21,9% y por último 
# los que su ubicarón en black con 19,4% . El gráfico de barras facilita la lectura de 
# frecuencias absolutas (404 alumnos en el centro), mientras que el de 
# torta permite visualizar la estructura porcentual del aula. 
# Se detecta un dato daltante. 
