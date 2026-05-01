# ==============================================================================
# TRABAJO PRÁCTICO 2: Estimación, Correlaciones y Pruebas de Hipótesis
# Maestría en Ciencia de Datos - FCyT - UADER
# ==============================================================================
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
