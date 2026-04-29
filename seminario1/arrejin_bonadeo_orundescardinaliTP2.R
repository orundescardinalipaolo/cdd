# Maestría en Ciencia de Datos - FCyT - UADER
# Seminario 1. Técnicas de Análisis en Ciencias de Datos
# Trabajo Práactico 2. Estimación de parámetros. Correlaciones entre variables. Gráficos. Pruebas de Hipóotesis.


# MARTIN ARREJIN - PAMELA BONADEO - ORUNDES CARDINALI PAOLO
#CODIGO EN REPO https://github.com/orundescardinalipaolo/cdd

# Trabajando con una base de datos.
# Carge en su área de trabajo el archivo Student0405.txt. Esta base contiene datos recolectados durante 2004 a 2005 de estudiantes en clases de estdística de una Universidad de Estados Unidos. Las variables consideradas son: género (Sex), promedio de calificaciones (GPA), importancia de la religión en sus vidas (ReligImp), cantidad de faltas en promedio por semana (MissClass), ubicación en el aula (Seat), días de fiesta en un mes (PartyDays), horas semanales de estudio (StudyHrs). Importe los datos y luego realice las siguientes actividades:


# a) Ejecute comandos para visualizar su base de datos y las variables: attach(datos), View(datos), head(datos), str(datos).

  # Cargar la base de datos
  # datos <- read.table("seminario1/Student0405.txt", header = TRUE)
  datos <- read.delim("seminario1/Student0405.txt", sep="\t", header = TRUE)
  #View(datos)
  

  # Nos muestra el siguiente error: 
  # Error en scan(file = file, what = what, sep = sep, quote = quote, dec = dec, : line 98 did not have 7 elements

  #¿solucion provisoria? ¿O hay que revisar la fuente?
  datos <- read.table("seminario1/Student0405.txt", header = TRUE, fill = TRUE, strip.white = TRUE)


  #adjunta la base de datos
  attach(datos)
  
  #abre una nueva ventana tipo excel con los datos
  View(datos)

  str(datos)
  #devuelve:
  # 'data.frame':	690 obs. of  7 variables:
  #   $ Sex      : chr  "Female" "Male" "Female" "Female" ...
  # $ GPA      : chr  "3.70" "3.20" "3.01" "3.77" ...
  # $ ReligImp : chr  "Fairly" "Fairly" "Fairly" "Not" ...
  # $ MissClass: chr  "1" "3" "0" "0" ...
  # $ Seat     : chr  "Back" "Front" "Middle" "Middle" ...
  # $ PartyDays: int  5 3 8 0 8 2 1 2 15 1 ...
  # $ StudyHrs : int  3 30 16 4 12 20 4 15 7 40 ...
   
  #muestra los encabezados
  head(datos)
  
  # Sex  GPA ReligImp MissClass   Seat PartyDays StudyHrs
  # 1 Female 3.70   Fairly         1   Back         5        3
  # 2   Male 3.20   Fairly         3  Front         3       30
  # 3 Female 3.01   Fairly         0 Middle         8       16
  # 4 Female 3.77      Not         0 Middle         0        4
  # 5   Male 3.28      Not         0 Middle         8       12
  # 6 Female 2.80   Fairly         0 Middle         2       20
  # 
  
  
  

  
# b) Identifique tamaño muestral y cantidad de variables.
  
  nrow(datos)   # cantidad de observaciones (total = 690)
  #salida: [1] 690
  
  ncol(datos)   # cantidad de variables (total = 7)
  #salida: [1] 7
  
  dim(datos)    # muestra ambas cosas: filas y columnas (total 690 7)
  #salida: [1] 690   7
  
 
# c) Determine qué variables son cuantitativas y cuáles son cualitativas. Entre las cuantitativas, ¿hay variables discretas? Las variables cualitativas, ¿qué categorías tienen?
  
  #primero revisamos la estructura
  str(datos)
  summary(datos)
  
  
  # >   str(datos)
  # 'data.frame':	690 obs. of  7 variables:
  #   $ Sex      : chr  "Female" "Male" "Female" "Female" ...
  # $ GPA      : chr  "3.70" "3.20" "3.01" "3.77" ...
  # $ ReligImp : chr  "Fairly" "Fairly" "Fairly" "Not" ...
  # $ MissClass: chr  "1" "3" "0" "0" ...
  # $ Seat     : chr  "Back" "Front" "Middle" "Middle" ...
  # $ PartyDays: int  5 3 8 0 8 2 1 2 15 1 ...
  # $ StudyHrs : int  3 30 16 4 12 20 4 15 7 40 ...
  # >   summary(datos)
  # Sex                GPA              ReligImp        
  # Length:690         Length:690         Length:690        
  # Class :character   Class :character   Class :character  
  # Mode  :character   Mode  :character   Mode  :character  
  # 
  
   
  # 
  # MissClass             Seat             PartyDays         StudyHrs    
  # Length:690         Length:690         Min.   : 0.000   Min.   : 0.00  
  # Class :character   Class :character   1st Qu.: 3.000   1st Qu.: 6.00  
  # Mode  :character   Mode  :character   Median : 7.000   Median :10.00  
  # Mean   : 7.571   Mean   :13.17  
  # 3rd Qu.:11.000   3rd Qu.:16.00  
  # Max.   :31.000   Max.   :70.00  
  # NA's   :9  
  
  #Convertir en tabla para el Rmd:
  # Variable    | Descripción                | Tipo                                                            |
  #   | ----------- | -------------------------- | --------------------------------------------------------------- |
  #   | `Sex`       | Género                     | Cualitativa                                                     |
  #   | `GPA`       | Promedio de calificaciones | Cuantitativa continua                                           |
  #   | `ReligImp`  | Importancia de la religión | Cualitativa ordinal o cuantitativa discreta, según codificación |
  #   | `MissClass` | Faltas promedio por semana | Cuantitativa discreta                                           |
  #   | `Seat`      | Ubicación en el aula       | Cualitativa                                                     |
  #   | `PartyDays` | Días de fiesta en un mes   | Cuantitativa discreta                                           |
  #   | `StudyHrs`  | Horas semanales de estudio | Cuantitativa continua o discreta, según registro                |
     
  
  # Las variables cuantitativas son GPA, MissClass, PartyDays y StudyHrs, ya que representan mediciones o conteos numéricos. Entre ellas, MissClass y PartyDays pueden considerarse discretas porque expresan cantidades contables.
  # Las variables cualitativas son Sex, Seat y ReligIm. La variable Sex clasifica a los estudiantes según género, Seat indica la ubicación en el aula y ReligImp representa una escala de importancia, por lo que se interpreta como cualitativa ordinal.
  
  #Las variables cualitativas, ¿qué categorías tienen?
  
  table(datos$Sex)
  #salida:
  #Female   Male 
  #382    308 
  
  table(datos$Seat)
  #salida:
  # 0      1      2      4   Back  Front Middle 
  # 2      1      1      1    134    149    402 
  
  table(datos$ReligImp)
  # 0      1      2 Fairly    Not   Very 
  # 1      1      1    319    221    147 

  #acá preguntar en la clase de consulta por los 0, 1 y 2 que tal vez al estar "dañada" la bd (ejercicio a), está mal intepretando los valores y por eso trae 0, 1 y 2 cuando no debería...
  
  
     
#   d) Obtenga un resumen numérico para GPA que contenga: mínimo, máximo, Q1, Q3, mediana, media, desvío estándar y varianza. Además, realice dos gráficos distintos que le permitan describir la distribución de esta variable. ¿Qué puede decir acerca de su distribución?
   
#   e) Realice un resumen numérico y un gráfico que le permita conjeturar si las mujeres van más a fiestas que los varones. Describa lo que observa.
 
# f) Realice un resumen numérico y un gráfico que le permita conjeturar si las calificaciones GPA dependen de la ubicación de aula del alumno. Describa lo que observa.
 
# g) Realice una tabla de frecuencias absolutas y relativas para la variable Seat y realice dos gráficos distintos que le permitan visualizar esta variable. Describa lo que observa.
 
# h) Realice un gráfico que le permita ver cómo se comporta la variable GPA en función de la cantidad de horas de estudio. ¿Qué observa? ¿Es posible establecer una relación entre ambas?
   
#   i) Realice un gráfico que le permita ver cómo se comporta la variable GPA en función de la cantidad de horas de estudio, discriminando por sexo. ¿Qué observa?
   
#   j) Realice una prueba de hipótesis que le permita comparar el promedio de las calificaciones por género; no hay sospechas previas. Escriba H0 y H1. Concluya teniendo en cuenta el p-value. Analice el cumplimiento de supuestos: normalidad de cada grupo y homogeneidad de varianzas.

# k) Si no hay diferencias significativas del promedio de calificaciones según el género, realice una estimación a través de un IC del 98 % para la verdadera calificación media.