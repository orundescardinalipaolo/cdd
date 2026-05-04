# Maestría en Ciencia de Datos - FCyT - UADER
# Seminario 1. Técnicas de Análisis en Ciencias de Datos
# Trabajo Práactico 2. Estimación de parámetros. Correlaciones entre variables. Gráficos. Pruebas de Hipóotesis.


# MARTIN ARREJIN - PAMELA BONADEO - SANTIAGO GRINÓVERO - PAOLO ORUNDES CARDINALI
#CODIGO EN REPO https://github.com/orundescardinalipaolo/cdd

# Trabajando con una base de datos.
# Carge en su área de trabajo el archivo Student0405.txt. Esta base contiene datos recolectados durante 2004 a 2005 de estudiantes en clases de estdística de una Universidad de Estados Unidos. Las variables consideradas son: género (Sex), promedio de calificaciones (GPA), importancia de la religión en sus vidas (ReligImp), cantidad de faltas en promedio por semana (MissClass), ubicación en el aula (Seat), días de fiesta en un mes (PartyDays), horas semanales de estudio (StudyHrs). Importe los datos y luego realice las siguientes actividades:


# a) Ejecute comandos para visualizar su base de datos y las variables: attach(datos), View(datos), head(datos), str(datos).

  # Cargar la base de datos
  # datos <- read.table("seminario1/Student0405.txt", header = TRUE)
  
  #View(datos)
  
  # Nos muestra el siguiente error: 
  # Error en scan(file = file, what = what, sep = sep, quote = quote, dec = dec, : line 98 did not have 7 elements

  #¿solucion provisoria? ¿O hay que revisar la fuente?
  #datos <- read.table("Student0405.txt", header = TRUE, fill = TRUE, strip.white = TRUE)

  #solución definitiva usar sep="\t" para separar por tabulaciones
  datos <- read.delim("Student0405.txt", sep="\t", header = TRUE)

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
  # $ GPA      : num  3.7 3.2 3.01 3.77 3.28 2.8 2.5 3.11 3.15 3.44 ...
  # $ ReligImp : chr  "Fairly" "Fairly" "Fairly" "Not" ...
  # $ MissClass: int  1 3 0 0 0 0 3 0 2 0 ...
  # $ Seat     : chr  "Back" "Front" "Middle" "Middle" ...
  # $ PartyDays: int  5 3 8 0 8 2 1 2 15 1 ...
  # $ StudyHrs : int  3 30 16 4 12 20 4 15 7 40 ...

   
  # >   summary(datos)
  # Sex                 GPA          ReligImp           MissClass     
  # Length:690         Min.   :1.500   Length:690         Min.   :0.0000  
  # Class :character   1st Qu.:2.930   Class :character   1st Qu.:0.0000  
  # Mode  :character   Median :3.200   Mode  :character   Median :1.0000  
  # Mean   :3.179                      Mean   :0.9086  
  # 3rd Qu.:3.515                      3rd Qu.:1.0000  
  # Max.   :4.000                      Max.   :6.0000  
  # NA's   :3                          NA's   :1       
  # Seat             PartyDays         StudyHrs    
  # Length:690         Min.   : 0.000   Min.   : 0.00  
  # Class :character   1st Qu.: 3.000   1st Qu.: 6.25  
  # Mode  :character   Median : 7.000   Median :10.00  
  # Mean   : 7.501   Mean   :13.16  
  # 3rd Qu.:11.000   3rd Qu.:16.00  
  # Max.   :31.000   Max.   :70.00  
  # NA's   :4 
  
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
  # Back  Front Middle 
  # 1    134    151    404 
  
  table(datos$ReligImp)
  # Fairly    Not   Very 
  # 319    222    149 

  #acá preguntar en la clase de consulta por los 0, 1 y 2 que tal vez al estar "dañada" la bd (ejercicio a), está mal intepretando los valores y por eso trae 0, 1 y 2 cuando no debería... (Resuelto en la clase de consulta con Santiago Diaz el 29 de abril de 2026)
  
     
#   d) Obtenga un resumen numérico para GPA que contenga: mínimo, máximo, Q1, Q3, mediana, media, desvío estándar y varianza. Además, realice dos gráficos distintos que le permitan describir la distribución de esta variable. ¿Qué puede decir acerca de su distribución?
  
  resumen_GPA <- data.frame(
    Minimo = min(datos$GPA, na.rm = TRUE),
    Q1 = quantile(datos$GPA, 0.25, na.rm = TRUE),
    Mediana = median(datos$GPA, na.rm = TRUE),
    Media = mean(datos$GPA, na.rm = TRUE),
    Q3 = quantile(datos$GPA, 0.75, na.rm = TRUE),
    Maximo = max(datos$GPA, na.rm = TRUE),
    Desvio_Estandar = sd(datos$GPA, na.rm = TRUE),
    Varianza = var(datos$GPA, na.rm = TRUE)
  )
  
  resumen_GPA
  #Salida:  
  # Minimo  Q1     Mediana    Media    Q3       Maximo   Desvio_Estandar  Varianza
  # 25%     1.5     2.93      3.2     3.179214  3.515 4
  #0.4533599    0.2055352
  

  #El histograma permite analizar la forma general de la distribución, mientras que el boxplot permite identificar la mediana, la dispersión y la posible existencia de valores atípicos.
  
  #Gráfico 1: Histograma
  hist(datos$GPA,
       main = "Distribución de GPA",
       xlab = "GPA (Promedio de calificaciones)",
       ylab = "Frecuencia",
       col = "lightblue",
       border = "white")
  
  #En el histograma se observa que la variable GPA presenta valores concentrados alrededor de la zona central hacia la derecha de la distribución más precisamente en el ranzo entre 2.9 y 3.5
  
  #Gráfico 2: Boxplot
  boxplot(datos$GPA,
          main = "Boxplot de GPA",
          ylab = "GPA (Promedio de calificaciones)",
          col = "lightgreen")
  
    #En el boxplot de GPA se observa que la mayoría de las calificaciones se concentran entre aproximadamente 2.9 y 3.5 (coincidente con el diagrama anterior), con una mediana cercana a 3.2 (donde está linea negra en el centro del recuadro, esto quiere decir que la mitad está por debajo de ésta línea y la otra mitad por encima). Además, aparecen algunos valores atípicos inferiores, lo que indica que existen pocos estudiantes con GPA bastante más bajo que el resto. En general, los datos se concentran en valores medios-altos.
  
   
#   e) Realice un resumen numérico y un gráfico que le permita conjeturar si las mujeres van más a fiestas que los varones. Describa lo que observa.
  
  aggregate(PartyDays ~ Sex, data = datos, summary)
  #Salida
  # Sex PartyDays.Min. PartyDays.1st Qu. PartyDays.Median PartyDays.Mean
  # 1 Female       0.000000          3.000000         6.500000       7.539267
  # 2   Male       0.000000          3.000000         7.000000       7.454545
  # PartyDays.3rd Qu. PartyDays.Max.
  # 1         12.000000      31.000000
  # 2         10.000000      31.000000
  
  aggregate(PartyDays ~ Sex, data = datos, mean, na.rm = TRUE)
  #Salida
  # Sex PartyDays
  # 1 Female  7.539267
  # 2   Male  7.454545
  
  
  aggregate(PartyDays ~ Sex, data = datos, median, na.rm = TRUE)
  #Salida
  # Sex PartyDays
  # 1 Female       6.5
  # 2   Male       7.0
  
  aggregate(PartyDays ~ Sex, data = datos, sd, na.rm = TRUE)
  #Salida
  # Sex PartyDays
  # 1 Female  5.458724
  # 2   Male  5.454689
  
  #Gráfico
  boxplot(PartyDays ~ Sex,
          data = datos,
          main = "Días de fiesta por mes según género",
          xlab = "Género",
          ylab = "Días de fiesta por mes",
          names = c("Femenino", "Masculino"),
          col = c("lightpink", "lightblue"))
  
  #otro gráfico
  promedios_party$Genero <- factor(promedios_party$Sex,
                                   levels = c("Female", "Male"),
                                   labels = c("Femenino", "Masculino"))
  
  barplot(promedios_party$PartyDays,
          names.arg = promedios_party$Genero,
          main = "Promedio de días de fiesta según género",
          xlab = "Género",
          ylab = "Promedio de días de fiesta",
          col = c("lightpink", "lightblue"))
  # Éste gráfico no muestra diferencias significativas como el anterior
  
  # Que observamos: Para analizar si las mujeres van más a fiestas que los varones, se comparó la variable PartyDays según la variable Sex. A partir del resumen numérico y del boxplot, se observa si alguno de los grupos presenta una mediana o media superior. 
  #Si el grupo correspondiente a mujeres presenta valores centrales más altos, podría decirse que asisten a fiestas con mayor frecuencia. Sin embargo, esta observación es descriptiva y no implica por sí sola una diferencia estadísticamente significativa (se aprecia mucho mejor en el segundo gráfico).
 
  
  
# f) Realice un resumen numérico y un gráfico que le permita conjeturar si las calificaciones GPA dependen de la ubicación de aula del alumno. Describa lo que observa.
 
  aggregate(GPA ~ Seat, data = datos, summary)
  #Salida: 
  # Seat GPA.Min. GPA.1st Qu. GPA.Median GPA.Mean GPA.3rd Qu. GPA.Max.
  # 1        2.250000    2.250000   2.250000 2.250000    2.250000 2.250000
  # 2   Back 1.910000    2.805000   3.100000 3.077015    3.400000 4.000000
  # 3  Front 1.740000    3.000000   3.265000 3.251067    3.600000 4.000000
  # 4 Middle 1.500000    2.940000   3.205000 3.188781    3.517500 4.000000
  
  aggregate(GPA ~ Seat, data = datos, mean, na.rm = TRUE)
  #Salida: 
  # Seat      GPA
  # 1        2.250000
  # 2   Back 3.077015
  # 3  Front 3.251067
  # 4 Middle 3.188781
  
  aggregate(GPA ~ Seat, data = datos, median, na.rm = TRUE)
  #Salida: 
  # Seat   GPA
  # 1        2.250
  # 2   Back 3.100
  # 3  Front 3.265
  # 4 Middle 3.205
  
  aggregate(GPA ~ Seat, data = datos, sd, na.rm = TRUE)
  #Salida: 
  # Seat       GPA
  # 1               NA
  # 2   Back 0.4748027
  # 3  Front 0.4486537
  # 4 Middle 0.4408886
  
  
  #Aparecen columnas sin etiquetas porque probablemente en el archivo hay valores vacíos, espacios en blanco o datos faltantes mal leídos.
  
  #Comprobación: 
  table(datos$Seat, useNA = "ifany")
  
  #Salida
  # Back  Front Middle 
  # 1    134    151    404
  
  #Para ver las filas problemáticas
  datos[datos$Seat == "" | is.na(datos$Seat), ]
  # Sex  GPA ReligImp MissClass Seat PartyDays StudyHrs
  # 305 Male 2.25     Very         1              4       10
  
  unique(datos$Seat)
  #Limpiamos los espacios en blanco
  datos$Seat <- trimws(datos$Seat)
  
  #Recomprobación: 
  table(datos$Seat, useNA = "ifany")
  #sigue fallando... preguntar en clases
  
  
  #Armamos el gráfico con los datos que tenemos
  boxplot(GPA ~ Seat,
          data = datos,
          main = "GPA según ubicación en el aula",
          xlab = "Ubicación en el aula",
          ylab = "GPA",
          col = "lightblue")
  
  # Limpiamos posibles espacios en blanco
  datos$Seat <- trimws(datos$Seat)
  
  # Convertimos categorías vacías en NA
  datos$Seat[datos$Seat == ""] <- NA
  
  # Verificamos la tabla
  table(datos$Seat, useNA = "ifany")
  
  datos_seat <- subset(datos, !is.na(Seat))
  
  aggregate(GPA ~ Seat, data = datos_seat, summary)
  aggregate(GPA ~ Seat, data = datos_seat, mean, na.rm = TRUE)
  aggregate(GPA ~ Seat, data = datos_seat, sd, na.rm = TRUE)
  
  boxplot(GPA ~ Seat,
          data = datos_seat,
          main = "GPA según ubicación en el aula",
          xlab = "Ubicación en el aula",
          ylab = "GPA",
          names = c("Al fondo", "Al frente", "En el medio"),
          col = "lightblue")
  
  #Finalmente este gráfico funciona bien excluyendo el NA. Al analizar la variable Seat, se detectó un registro sin categoría asignada. Dado que no es posible identificar la ubicación del estudiante en el aula, dicho valor se consideró como dato faltante (NA) y se excluyó del análisis comparativo de GPA según ubicación. Esta decisión evita interpretar erróneamente el valor vacío como una categoría válida.
  
  #Los puntos inferiores observados en la categoría “En el medio” representan valores atípicos: estudiantes con GPA más bajo que el resto del grupo. La mayoría de los valores se concentra por encima de esos puntos, dentro del rango principal mostrado por la caja.
  
    #En el resumen numérico se observa que los estudiantes ubicados en Front (Al frente) presentan el promedio de GPA más alto, seguidos por Middle (En el medio) y luego Back (Al fonde). Sin embargo, las diferencias entre los promedios no parecen ser muy marcadas, por lo que descriptivamente no se observa una separación fuerte entre los grupos.
  

  
  
# g) Realice una tabla de frecuencias absolutas y relativas para la variable Seat y realice dos gráficos distintos que le permitan visualizar esta variable. Describa lo que observa.
  
  #Frecuencias absolutas:
  freq_abs_seat <- table(datos$Seat)
  freq_abs_seat
  #Salida:
  # Back  Front Middle 
  # 134    151    404
  
  #Frecuencias relativas:
  freq_rel_seat <- prop.table(freq_abs_seat)
  freq_rel_seat
  #Salida
  # Back     Front    Middle 
  # 0.1944848 0.2191582 0.5863570 
  
  #Para hacerlo en una tabla completa:
  tabla_seat <- data.frame(
    Seat = names(freq_abs_seat),
    Frecuencia_Absoluta = as.vector(freq_abs_seat),
    Frecuencia_Relativa = as.vector(freq_rel_seat)
  )
  
  tabla_seat
  #Salida
  # Seat Frecuencia_Absoluta Frecuencia_Relativa
  # 1   Back                 134           0.1944848
  # 2  Front                 151           0.2191582
  # 3 Middle                 404           0.5863570
  
  #También podemos expresar las frecuencias relativas en porcentaje:
  tabla_seat$Porcentaje <- tabla_seat$Frecuencia_Relativa * 100
  tabla_seat
  #Salida:
  # Back                 134           0.1944848   19.44848
  # 2  Front                 151           0.2191582   21.91582
  # 3 Middle                 404           0.5863570   58.63570
  
  #Gráfico 1: Barras
  barplot(freq_abs_seat,
          names.arg = c("Al fondo", "Al frente", "En el medio"),
          main = "Frecuencia absoluta de ubicación en el aula",
          xlab = "Ubicación",
          ylab = "Frecuencia",
          col = "lightblue")
  
  #Gráfico 2: Gráfico circular
  pie(freq_abs_seat,
      labels = c("Al fondo", "Al frente", "En el medio"),
      main = "Distribución porcentual de ubicación en el aula",
      col = c("lightblue", "lightgreen", "lightpink"),
      cex = 0.9)
  
  #Queda muy feo, así que lo retocamos un poco ya que no se ven bien las etiquetas y las cambiamos mejor a leyendas
  
  porcentajes_seat <- round(prop.table(freq_abs_seat) * 100, 1)
  
  etiquetas_seat <- paste0(
    c("Al fondo", "Al frente", "En el medio"),
    ": ",
    porcentajes_seat,
    "%"
  )
  
  colores_pastel <- c("lightblue", "lightgreen", "lightpink")
  
  pie(freq_abs_seat,
      labels = NA,
      main = "Distribución porcentual de ubicación en el aula",
      col = colores_pastel,
      radius = 0.8)
  
  legend("topright",
         legend = etiquetas_seat,
         fill = colores_pastel,
         cex = 0.8)
  
  #Observaciones: 
  # La variable Seat muestra la distribución de los estudiantes según la ubicación que ocupan en el aula. A partir de la tabla de frecuencias absolutas y relativas se puede identificar cuál es la ubicación más frecuente.
  # Los gráficos permiten visualizar rápidamente si los estudiantes se distribuyen de manera equilibrada entre las categorías o si existe una mayor concentración en alguna ubicación específica.
  #Se observa en el gráfico que al fondo y al frente tienen casi la misma distribución, representando entre ambos un 40% del total, en cambio casi el 60% se ubica en el medio del aula
  
  
  
# h) Realice un gráfico que le permita ver cómo se comporta la variable GPA en función de la cantidad de horas de estudio. ¿Qué observa? ¿Es posible establecer una relación entre ambas?
   
#   i) Realice un gráfico que le permita ver cómo se comporta la variable GPA en función de la cantidad de horas de estudio, discriminando por sexo. ¿Qué observa?
   
#   j) Realice una prueba de hipótesis que le permita comparar el promedio de las calificaciones por género; no hay sospechas previas. Escriba H0 y H1. Concluya teniendo en cuenta el p-value. Analice el cumplimiento de supuestos: normalidad de cada grupo y homogeneidad de varianzas.

# k) Si no hay diferencias significativas del promedio de calificaciones según el género, realice una estimación a través de un IC del 98 % para la verdadera calificación media.