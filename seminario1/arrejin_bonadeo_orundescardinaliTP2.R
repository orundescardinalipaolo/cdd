# Maestría en Ciencia de Datos - FCyT - UADER
# Seminario 1. Técnicas de Análisis en Ciencias de Datos
# Trabajo Práactico 2. Estimación de parámetros. Correlaciones entre variables. Gráficos. Pruebas de Hipóotesis.


# MARTIN ARREJIN - PAMELA BONADEO - ORUNDES CARDINALI PAOLO
#CODIGO EN REPO https://github.com/orundescardinalipaolo/cdd

# Trabajando con una base de datos.
# Carge en su área de trabajo el archivo Student0405.txt. Esta base contiene datos recolectados durante 2004 a 2005 de estudiantes en clases de estdística de una Universidad de Estados Unidos. Las variables consideradas son: género (Sex), promedio de calificaciones (GPA), importancia de la religión en sus vidas (ReligImp), cantidad de faltas en promedio por semana (MissClass), ubicación en el aula (Seat), días de fiesta en un mes (PartyDays), horas semanales de estudio (StudyHrs). Importe los datos y luego realice las siguientes actividades:


# a) Ejecute comandos para visualizar su base de datos y las variables: attach(datos), View(datos), head(datos), str(datos).

  # Cargar la base de datos
  datos <- read.table("seminario1/Student0405.txt", header = TRUE)

  # Nos muestra el siguiente error: 
  # Error en scan(file = file, what = what, sep = sep, quote = quote, dec = dec, : line 98 did not have 7 elements

  
# b) Identifique tamaño muestral y cantidad de variables.
 
# c) Determine qué variables son cuantitativas y cuáles son cualitativas. Entre las cuantitativas, ¿hay variables discretas? Las variables cualitativas, ¿qué categorías tienen?
   
#   d) Obtenga un resumen numérico para GPA que contenga: mínimo, máximo, Q1, Q3, mediana, media, desvío estándar y varianza. Además, realice dos gráficos distintos que le permitan describir la distribución de esta variable. ¿Qué puede decir acerca de su distribución?
   
#   e) Realice un resumen numérico y un gráfico que le permita conjeturar si las mujeres van más a fiestas que los varones. Describa lo que observa.
 
# f) Realice un resumen numérico y un gráfico que le permita conjeturar si las calificaciones GPA dependen de la ubicación de aula del alumno. Describa lo que observa.
 
# g) Realice una tabla de frecuencias absolutas y relativas para la variable Seat y realice dos gráficos distintos que le permitan visualizar esta variable. Describa lo que observa.
 
# h) Realice un gráfico que le permita ver cómo se comporta la variable GPA en función de la cantidad de horas de estudio. ¿Qué observa? ¿Es posible establecer una relación entre ambas?
   
#   i) Realice un gráfico que le permita ver cómo se comporta la variable GPA en función de la cantidad de horas de estudio, discriminando por sexo. ¿Qué observa?
   
#   j) Realice una prueba de hipótesis que le permita comparar el promedio de las calificaciones por género; no hay sospechas previas. Escriba H0 y H1. Concluya teniendo en cuenta el p-value. Analice el cumplimiento de supuestos: normalidad de cada grupo y homogeneidad de varianzas.

# k) Si no hay diferencias significativas del promedio de calificaciones según el género, realice una estimación a través de un IC del 98 % para la verdadera calificación media.