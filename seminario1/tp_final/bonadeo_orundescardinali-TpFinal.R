# Maestría en Ciencia de Datos - FCyT - UADER
# Seminario 1 - Trabajo Práctico Final
# PAMELA BONADEO - ORUNDES CARDINALI PAOLO
# Dataset: Precios históricos de combustibles en Paraná


library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(vcd)
library(factoextra)

# 1. CARGA Y PREPARACIÓN DEL DATASET

combustibles <- read_delim(
  "seminario1/tp_final/data-set-precios-historicos-combustibles-parana.csv",
  delim = ";",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
)

combustibles <- combustibles %>%
  mutate(
    empresa = as.factor(empresa),
    producto = as.factor(producto),
    empresabandera = as.factor(empresabandera),
    fecha = dmy_hm(fecha_vigencia),
    anio = as.integer(anio),
    mes = as.integer(mes),
    mes_nombre = month(fecha, label = TRUE, abbr = FALSE),
    latitud_num = as.numeric(gsub("\\.", "", latitud)) / 100000,
    longitud_num = as.numeric(gsub("\\.", "", longitud)) / 100000
  )

# Revisión inicial
head(combustibles)
str(combustibles)
dim(combustibles)
summary(combustibles)


# 2. DATOS FALTANTES

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
sum(is.na(combustibles))


# 3. TABLAS Y GRÁFICOS UNIVARIADOS

# Tablas de frecuencia
tabla_producto <- combustibles %>%
  count(producto, sort = TRUE) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2))

tabla_bandera <- combustibles %>%
  count(empresabandera, sort = TRUE) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2))

tabla_empresa <- combustibles %>%
  count(empresa, sort = TRUE) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2))

tabla_anio <- combustibles %>%
  count(anio) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2))

tabla_producto
tabla_bandera
tabla_empresa
tabla_anio

# Resumen de precio
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

# Resumen de precio por producto
resumen_precio_producto <- combustibles %>%
  group_by(producto) %>%
  summarise(
    cantidad = n(),
    media = round(mean(precio, na.rm = TRUE), 2),
    mediana = round(median(precio, na.rm = TRUE), 2),
    desvio = round(sd(precio, na.rm = TRUE), 2),
    minimo = min(precio, na.rm = TRUE),
    maximo = max(precio, na.rm = TRUE),
    .groups = "drop"
  )

resumen_precio_producto

# Gráficos principales
ggplot(tabla_producto, aes(x = reorder(producto, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Cantidad de registros por producto", x = "Producto", y = "Cantidad")

ggplot(tabla_bandera, aes(x = reorder(empresabandera, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Cantidad de registros por bandera", x = "Bandera", y = "Cantidad")

ggplot(combustibles, aes(x = precio)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribución de precios", x = "Precio", y = "Frecuencia")

ggplot(combustibles, aes(x = producto, y = precio)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Precio según producto", x = "Producto", y = "Precio")

# Evolución anual
precio_anio <- combustibles %>%
  group_by(anio) %>%
  summarise(
    precio_promedio = mean(precio, na.rm = TRUE),
    precio_mediano = median(precio, na.rm = TRUE),
    cantidad = n(),
    .groups = "drop"
  )

precio_anio

ggplot(precio_anio, aes(x = anio, y = precio_promedio)) +
  geom_line() +
  geom_point() +
  labs(title = "Evolución del precio promedio por año", x = "Año", y = "Precio promedio")


# 4. INTERVALO DE CONFIANZA PARA VARIABLE CUALITATIVA
# Categoría elegida: SHELL C.A.P.S.A.

n_total <- nrow(combustibles)

n_shell <- combustibles %>%
  filter(empresabandera == "SHELL C.A.P.S.A.") %>%
  nrow()

ic_shell <- prop.test(
  x = n_shell,
  n = n_total,
  conf.level = 0.95,
  correct = FALSE
)

ic_shell

tabla_ic_shell <- tibble(
  variable = "empresabandera",
  categoria = "SHELL C.A.P.S.A.",
  total = n_total,
  cantidad_categoria = n_shell,
  porcentaje = round(n_shell / n_total * 100, 2),
  limite_inferior = round(ic_shell$conf.int[1] * 100, 2),
  limite_superior = round(ic_shell$conf.int[2] * 100, 2)
)

tabla_ic_shell


# 5. INTERVALO DE CONFIANZA PARA VARIABLE CUANTITATIVA
# Variable: precio

ic_precio <- t.test(combustibles$precio, conf.level = 0.95)

ic_precio

tabla_ic_precio <- tibble(
  variable = "precio",
  media = round(mean(combustibles$precio, na.rm = TRUE), 2),
  desvio = round(sd(combustibles$precio, na.rm = TRUE), 2),
  limite_inferior = round(ic_precio$conf.int[1], 2),
  limite_superior = round(ic_precio$conf.int[2], 2)
)

tabla_ic_precio

# IC de precio por producto
ic_precio_producto <- combustibles %>%
  group_by(producto) %>%
  summarise(
    n = n(),
    media = mean(precio, na.rm = TRUE),
    desvio = sd(precio, na.rm = TRUE),
    error = desvio / sqrt(n),
    li = media - qt(0.975, n - 1) * error,
    ls = media + qt(0.975, n - 1) * error,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

ic_precio_producto

# 6. ANÁLISIS DE ASOCIACIÓN

# Asociación entre producto y bandera comercial
tabla_producto_bandera <- table(combustibles$producto, combustibles$empresabandera)

tabla_producto_bandera

chisq.test(tabla_producto_bandera, simulate.p.value = TRUE)

assocstats(tabla_producto_bandera)

# Relación entre año y precio
cor.test(combustibles$anio, combustibles$precio, method = "pearson")
cor.test(combustibles$anio, combustibles$precio, method = "spearman")

ggplot(combustibles, aes(x = anio, y = precio)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  labs(title = "Relación entre año y precio", x = "Año", y = "Precio")



# 7. COMPARACIÓN DE GRUPOS / PRUEBA DE HIPÓTESIS


# Se comparan dos grupos independientes:
# Nafta súper entre 92 y 95 Ron vs Nafta premium de más de 95 Ron

datos_naftas <- combustibles %>%
  filter(producto %in% c(
    "Nafta súper entre 92 y 95 Ron",
    "Nafta premium de más de 95 Ron"
  ))

# Tabla descriptiva de los dos grupos
resumen_naftas <- datos_naftas %>%
  group_by(producto) %>%
  summarise(
    cantidad = n(),
    media = round(mean(precio, na.rm = TRUE), 2),
    mediana = round(median(precio, na.rm = TRUE), 2),
    desvio = round(sd(precio, na.rm = TRUE), 2),
    minimo = min(precio, na.rm = TRUE),
    maximo = max(precio, na.rm = TRUE),
    .groups = "drop"
  )

resumen_naftas

# Gráfico comparativo
ggplot(datos_naftas, aes(x = producto, y = precio)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Comparación de precios entre Nafta súper y Nafta premium",
    x = "Producto",
    y = "Precio"
  )

# Diagnóstico de normalidad por grupo
# Se utiliza Shapiro-Wilk, aunque con muestras grandes puede ser muy sensible.
by(datos_naftas$precio, datos_naftas$producto, shapiro.test)

# QQ plot para observar normalidad de forma gráfica
ggplot(datos_naftas, aes(sample = precio)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ producto) +
  labs(
    title = "QQ plot del precio por tipo de nafta"
  )

# Diagnóstico de igualdad de varianzas
# Como las varianzas pueden ser distintas o dudosas, se usa Welch.
var.test(precio ~ producto, data = datos_naftas)

# Prueba t de Welch
# H0: las medias de precio son iguales entre ambos productos.
# H1: las medias de precio son diferentes.

prueba_welch_naftas <- t.test(
  precio ~ producto,
  data = datos_naftas,
  var.equal = FALSE,
  conf.level = 0.95
)

prueba_welch_naftas


# 8. MODELOS LINEALES

modelo_1 <- lm(precio ~ anio, data = combustibles)
modelo_2 <- lm(precio ~ anio + producto, data = combustibles)

summary(modelo_1)
summary(modelo_2)

anova(modelo_1, modelo_2)

# Métricas simples
metricas <- tibble(
  modelo = c("precio ~ anio", "precio ~ anio + producto"),
  r2 = c(summary(modelo_1)$r.squared, summary(modelo_2)$r.squared),
  r2_ajustado = c(summary(modelo_1)$adj.r.squared, summary(modelo_2)$adj.r.squared),
  rmse = c(
    sqrt(mean(residuals(modelo_1)^2)),
    sqrt(mean(residuals(modelo_2)^2))
  )
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

metricas

combustibles_modelo <- combustibles %>%
  mutate(
    precio_estimado = predict(modelo_2),
    residuo = precio - precio_estimado
  )

ggplot(combustibles_modelo, aes(x = precio_estimado, y = precio)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Precio observado vs precio estimado", x = "Precio estimado", y = "Precio observado")


# 9. OUTLIERS

# Función simple para detectar outliers por IQR
es_outlier <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  x < q1 - 1.5 * iqr | x > q3 + 1.5 * iqr
}

# Outliers en precio
combustibles_outliers <- combustibles %>%
  mutate(outlier_precio = es_outlier(precio))

outliers_precio <- combustibles_outliers %>%
  filter(outlier_precio) %>%
  select(empresa, producto, precio, fecha_vigencia, empresabandera, anio, mes)

outliers_precio
nrow(outliers_precio)

# Outliers en varias variables numéricas
combustibles_outliers <- combustibles %>%
  mutate(
    outlier_precio = es_outlier(precio),
    outlier_anio = es_outlier(anio),
    outlier_mes = es_outlier(mes),
    outlier_latitud = es_outlier(latitud_num),
    outlier_longitud = es_outlier(longitud_num),
    total_outliers = outlier_precio + outlier_anio + outlier_mes +
      outlier_latitud + outlier_longitud
  )

tabla_outliers <- tibble(
  variable = c("precio", "anio", "mes", "latitud", "longitud"),
  cantidad = c(
    sum(combustibles_outliers$outlier_precio),
    sum(combustibles_outliers$outlier_anio),
    sum(combustibles_outliers$outlier_mes),
    sum(combustibles_outliers$outlier_latitud),
    sum(combustibles_outliers$outlier_longitud)
  )
)

tabla_outliers

filas_con_outliers <- combustibles_outliers %>%
  filter(total_outliers > 0) %>%
  select(
    empresa,
    producto,
    precio,
    fecha_vigencia,
    empresabandera,
    anio,
    mes,
    latitud_num,
    longitud_num,
    total_outliers
  )

filas_con_outliers
nrow(filas_con_outliers)

# 10. REDUCCIÓN DE DIMENSIÓN - PCA

datos_pca <- combustibles %>%
  select(precio, anio, mes, latitud_num, longitud_num) %>%
  drop_na()

pca <- prcomp(datos_pca, center = TRUE, scale. = TRUE)

summary(pca)

fviz_eig(pca, addlabels = TRUE)

fviz_pca_biplot(
  pca,
  repel = TRUE,
  title = "Biplot del PCA"
)

# 11. APORTE PERSONAL
# Evolución mensual y variación interanual

precio_mensual <- combustibles %>%
  group_by(anio, mes, producto) %>%
  summarise(
    precio_promedio = mean(precio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(fecha_mes = ymd(paste(anio, mes, "01", sep = "-")))

precio_mensual

ggplot(precio_mensual, aes(x = fecha_mes, y = precio_promedio)) +
  geom_line() +
  facet_wrap(~ producto, scales = "free_y") +
  labs(title = "Evolución mensual del precio promedio", x = "Fecha", y = "Precio promedio")

precio_anual_producto <- combustibles %>%
  group_by(anio, producto) %>%
  summarise(
    precio_promedio = mean(precio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(producto, anio) %>%
  group_by(producto) %>%
  mutate(
    variacion_interanual = (precio_promedio - lag(precio_promedio)) /
      lag(precio_promedio) * 100
  ) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

precio_anual_producto

ggplot(precio_anual_producto, aes(x = anio, y = variacion_interanual)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ producto) +
  labs(title = "Variación interanual del precio promedio", x = "Año", y = "Variación interanual (%)")