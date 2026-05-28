# Maestría en Ciencia de Datos - FCyT - UADER
# Seminario 1. Técnicas de Análisis en Ciencias de Datos
# Trabajo Práctico Final.


# PAMELA BONADEO - ORUNDES CARDINALI PAOLO
# COMENTAR BLOQUES DE CÓDIGO CONTROL+SHITF+C
# AUTOAJUSTAR TEXTO (QUITAR BARRA HORIZONTAL) TOOLS / GLOBAL OPTIONS / CODE / DISPLAY / SOFT-WRAP SOURCE FILES
#CODIGO EN REPO https://github.com/orundescardinalipaolo/cdd
#FUENTE DEL DATASET http://datos.energia.gob.ar/dataset/1c181390-5045-475e-94dc-410429be4b17/archivo/f8dda0d5-2a9f-4d34-b79b-4e63de3995df


# 1. Cargar paquetes necesarios
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)

# 2. Cargar el dataset
combustibles <- read_delim(
  "data-set-precios-historicos-combustibles-parana.csv",
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