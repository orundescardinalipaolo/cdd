# Maestría en Ciencia de Datos - FCyT - UADER
# Seminario 1. Técnicas de Análisis en Ciencias de Datos
# Trabajo Práctico Final.


# PAMELA BONADEO - ORUNDES CARDINALI PAOLO
# COMENTAR BLOQUES DE CÓDIGO CONTROL+SHITF+C
# AUTOAJUSTAR TEXTO (QUITAR BARRA HORIZONTAL) TOOLS / GLOBAL OPTIONS / CODE / DISPLAY / SOFT-WRAP SOURCE FILES
#CODIGO EN REPO https://github.com/orundescardinalipaolo/cdd
#FUENTE DEL DATASET http://datos.energia.gob.ar/dataset/1c181390-5045-475e-94dc-410429be4b17/archivo/f8dda0d5-2a9f-4d34-b79b-4e63de3995df

#Ejercicio: Presentar la base de datos, una breve descripción de lo que contiene. Describir las variables, clasificarlas y dar la dimesnión del problema. Si corresponde, analice el porcentaje de datos faltantes, por variable y en el total del dataset.

# 1. Cargar paquetes necesarios
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)

# 2. Cargar el dataset
combustibles <- read_delim(
  "seminario1/tp_final/data-set-precios-historicos-combustibles-parana.csv",
  delim = ";",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
)

# 3. Ver primeras filas
head(combustibles)

# 4. Ver estructura general
str(combustibles)

# 5. Ver dimensiones del dataset
dim(combustibles)

# 6. Ver nombres de variables
names(combustibles)

# 7. Resumen general
summary(combustibles)


# LIMPIEZA Y PREPARACIÓN DE VARIABLES

combustibles <- combustibles %>%
  mutate(
    # Convertir variables categóricas
    empresa = as.factor(empresa),
    producto = as.factor(producto),
    empresabandera = as.factor(empresabandera),
    
    # Convertir fecha y hora
    fecha = dmy_hm(fecha_vigencia),
    
    # Crear variables temporales auxiliares
    anio = as.integer(anio),
    mes = as.integer(mes),
    mes_nombre = month(fecha, label = TRUE, abbr = FALSE),
    
    # Limpiar coordenadas geográficas
    latitud_num = as.numeric(gsub("\\.", "", latitud)) / 100000,
    longitud_num = as.numeric(gsub("\\.", "", longitud)) / 100000
  )

# Verificar estructura luego de la limpieza
str(combustibles)

# Ver primeras filas limpias
head(combustibles)


# ANÁLISIS DE DATOS FALTANTES

# Cantidad de valores faltantes por variable
faltantes_por_variable <- combustibles %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "cantidad_faltantes"
  ) %>%
  mutate(
    porcentaje_faltantes = round(cantidad_faltantes / nrow(combustibles) * 100, 2)
  )

faltantes_por_variable

# Total de datos faltantes en todo el dataset
total_faltantes <- sum(is.na(combustibles))

total_faltantes


# TABLAS DESCRIPTIVAS INICIALES

# Cantidad de registros por producto
tabla_producto <- combustibles %>%
  count(producto, sort = TRUE)

tabla_producto

# Cantidad de registros por empresa
tabla_empresa <- combustibles %>%
  count(empresa, sort = TRUE)

tabla_empresa

# Cantidad de registros por bandera
tabla_bandera <- combustibles %>%
  count(empresabandera, sort = TRUE)

tabla_bandera

# Cantidad de registros por año
tabla_anio <- combustibles %>%
  count(anio, sort = FALSE)

tabla_anio

# Estadísticos descriptivos del precio
resumen_precio <- combustibles %>%
  summarise(
    cantidad = n(),
    minimo = min(precio, na.rm = TRUE),
    q1 = quantile(precio, 0.25, na.rm = TRUE),
    media = mean(precio, na.rm = TRUE),
    mediana = median(precio, na.rm = TRUE),
    q3 = quantile(precio, 0.75, na.rm = TRUE),
    maximo = max(precio, na.rm = TRUE),
    desvio = sd(precio, na.rm = TRUE)
  )

resumen_precio

# GRÁFICOS UNIVARIADOS

# # Gráfico de barras por producto
# ggplot(combustibles, aes(x = producto)) +
#   geom_bar() +
#   coord_flip() +
#   labs(
#     title = "Cantidad de registros por tipo de producto",
#     x = "Producto",
#     y = "Cantidad de registros"
#   )
# 
# # Gráfico de barras por bandera
# ggplot(combustibles, aes(x = empresabandera)) +
#   geom_bar() +
#   coord_flip() +
#   labs(
#     title = "Cantidad de registros por bandera comercial",
#     x = "Bandera",
#     y = "Cantidad de registros"
#   )
# 
# # Histograma del precio
# ggplot(combustibles, aes(x = precio)) +
#   geom_histogram(bins = 30) +
#   labs(
#     title = "Distribución general de precios",
#     x = "Precio",
#     y = "Frecuencia"
#   )
# 
# # Boxplot del precio
# ggplot(combustibles, aes(y = precio)) +
#   geom_boxplot() +
#   labs(
#     title = "Boxplot general del precio",
#     y = "Precio"
#   )


#Ejercicio: Construcción de tablas y gráficos para variables univariadas

# Tabla de frecuencia por producto
tabla_producto <- combustibles %>%
  count(producto, sort = TRUE) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)
  )

tabla_producto

# Gráfico de barras por producto
ggplot(tabla_producto, aes(x = reorder(producto, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Cantidad de registros por tipo de producto",
    x = "Producto",
    y = "Cantidad de registros"
  )

# Tabla de frecuencia por bandera comercial
tabla_bandera <- combustibles %>%
  count(empresabandera, sort = TRUE) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)
  )

tabla_bandera

# Gráfico de barras por bandera comercial
ggplot(tabla_bandera, aes(x = reorder(empresabandera, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Cantidad de registros por bandera comercial",
    x = "Bandera comercial",
    y = "Cantidad de registros"
  )

# Tabla de frecuencia por empresa
tabla_empresa <- combustibles %>%
  count(empresa, sort = TRUE) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)
  )

tabla_empresa

# Gráfico de barras por empresa
ggplot(tabla_empresa, aes(x = reorder(empresa, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Cantidad de registros por empresa",
    x = "Empresa",
    y = "Cantidad de registros"
  )

#Variable cuantitativa: precio
# Resumen estadístico del precio
resumen_precio <- combustibles %>%
  summarise(
    cantidad = n(),
    minimo = min(precio, na.rm = TRUE),
    q1 = quantile(precio, 0.25, na.rm = TRUE),
    media = mean(precio, na.rm = TRUE),
    mediana = median(precio, na.rm = TRUE),
    q3 = quantile(precio, 0.75, na.rm = TRUE),
    maximo = max(precio, na.rm = TRUE),
    desvio = sd(precio, na.rm = TRUE)
  )

resumen_precio

# Histograma general del precio
ggplot(combustibles, aes(x = precio)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribución general de precios",
    x = "Precio",
    y = "Frecuencia"
  )


# Boxplot general del precio
ggplot(combustibles, aes(y = precio)) +
  geom_boxplot() +
  labs(
    title = "Boxplot general del precio",
    y = "Precio"
  )

# Precio por producto

# Estadísticos descriptivos del precio por producto
resumen_precio_producto <- combustibles %>%
  group_by(producto) %>%
  summarise(
    cantidad = n(),
    minimo = min(precio, na.rm = TRUE),
    media = round(mean(precio, na.rm = TRUE), 2),
    mediana = round(median(precio, na.rm = TRUE), 2),
    maximo = max(precio, na.rm = TRUE),
    desvio = round(sd(precio, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(media))

resumen_precio_producto

# Boxplot del precio según producto
ggplot(combustibles, aes(x = producto, y = precio)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Distribución del precio según tipo de producto",
    x = "Producto",
    y = "Precio"
  )


#Variables temporales: año y mes
# Tabla de registros por año
tabla_anio <- combustibles %>%
  count(anio) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)
  )

tabla_anio

# Gráfico de barras por año
ggplot(tabla_anio, aes(x = factor(anio), y = n)) +
  geom_col() +
  labs(
    title = "Cantidad de registros por año",
    x = "Año",
    y = "Cantidad de registros"
  )


#Evolución del precio promedio por año

# Precio promedio por año
precio_promedio_anio <- combustibles %>%
  group_by(anio) %>%
  summarise(
    precio_promedio = round(mean(precio, na.rm = TRUE), 2),
    mediana = round(median(precio, na.rm = TRUE), 2),
    cantidad = n()
  )

precio_promedio_anio

# Gráfico de evolución del precio promedio por año
ggplot(precio_promedio_anio, aes(x = anio, y = precio_promedio)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución del precio promedio por año",
    x = "Año",
    y = "Precio promedio"
  )

# Precio promedio por producto y año
precio_producto_anio <- combustibles %>%
  group_by(anio, producto) %>%
  summarise(
    precio_promedio = round(mean(precio, na.rm = TRUE), 2),
    .groups = "drop"
  )

precio_producto_anio

# Gráfico de evolución por producto
ggplot(precio_producto_anio, aes(x = anio, y = precio_promedio, group = producto)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución del precio promedio por producto y año",
    x = "Año",
    y = "Precio promedio"
  ) +
  facet_wrap(~ producto)
