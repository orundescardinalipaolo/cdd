
#
# Contenido:
# 1) Diferencia de medias
#    - Z con varianzas conocidas
#    - t con varianzas desconocidas pero iguales
#    - Welch para varianzas desiguales
#    - muestras pareadas
# 2) Diferencia de proporciones
# 3) Razón de varianzas con F
# 4) Supuestos: normalidad y homocedasticidad
#


options(scipen = 999)

cat("\n==============================\n")
cat("INFERENCIA PARA DOS PARAMETROS\n")
cat("==============================\n")

###############################################################
# 0) FUNCIONES AUXILIARES
###############################################################

# Imprime una línea separadora para ordenar la salida.
separador <- function(titulo) {
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat(titulo, "\n")
  cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
}

# Función simple para redondear y mostrar resultados de forma consistente.
mostrar_resultado <- function(nombre, valor, dig = 4) {
  cat(sprintf("%-35s : %s\n", nombre, round(valor, dig)))
}

# Función para interpretar una prueba a partir del p-valor.
interpretar_p <- function(pvalor, alpha = 0.05) {
  if (pvalor < alpha) {
    return(sprintf("Se rechaza H0 al %.2f%% de significancia.", alpha * 100))
  } else {
    return(sprintf("No se rechaza H0 al %.2f%% de significancia.", alpha * 100))
  }
}

###############################################################
# 1) DIFERENCIA DE MEDIAS: VARIANZAS CONOCIDAS (CASO Z)
###############################################################

# Marco teórico:
# Si X1_bar ~ N(mu1, sigma1^2/n1) y X2_bar ~ N(mu2, sigma2^2/n2),
# entonces la diferencia X1_bar - X2_bar tiene media mu1 - mu2 y varianza
# sigma1^2/n1 + sigma2^2/n2.
# Si sigma1 y sigma2 son conocidas, usamos Z.

prueba_medias_z <- function(xbar1, xbar2, sigma1, sigma2, n1, n2,
                            delta0 = 0, alpha = 0.05,
                            alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  diff_hat <- xbar1 - xbar2
  se <- sqrt(sigma1^2 / n1 + sigma2^2 / n2)
  z <- (diff_hat - delta0) / se

  pvalor <- switch(
    alternative,
    "two.sided" = 2 * (1 - pnorm(abs(z))),
    "less"      = pnorm(z),
    "greater"   = 1 - pnorm(z)
  )

  zcrit <- qnorm(1 - alpha / 2)
  ic <- c(diff_hat - zcrit * se, diff_hat + zcrit * se)

  list(
    estimacion = diff_hat,
    error_estandar = se,
    estadistico = z,
    pvalor = pvalor,
    ic = ic,
    decision = interpretar_p(pvalor, alpha)
  )
}

separador("1) DIFERENCIA DE MEDIAS CON Z")

# Ejemplo docente:
# Dos líneas automáticas de envasado con desvíos poblacionales conocidos.
res_z <- prueba_medias_z(
  xbar1 = 502, xbar2 = 499,
  sigma1 = 6, sigma2 = 5,
  n1 = 40, n2 = 36,
  delta0 = 0,
  alternative = "two.sided"
)

mostrar_resultado("Estimacion de mu1 - mu2", res_z$estimacion)
mostrar_resultado("Error estandar", res_z$error_estandar)
mostrar_resultado("Estadistico Z", res_z$estadistico)
mostrar_resultado("p-valor", res_z$pvalor)
cat("IC 95%                            : [", round(res_z$ic[1], 4), ", ", round(res_z$ic[2], 4), "]\n", sep = "")
cat("Decision                          : ", res_z$decision, "\n", sep = "")

###############################################################
# 2) DIFERENCIA DE MEDIAS: VARIANZAS DESCONOCIDAS PERO IGUALES
###############################################################

# Marco teórico:
# Si se asume sigma1^2 = sigma2^2 = sigma^2, se estima una varianza común:
# Sp^2 = [ (n1 - 1) s1^2 + (n2 - 1) s2^2 ] / (n1 + n2 - 2)
# y el estadístico sigue una t con n1 + n2 - 2 grados de libertad.

prueba_medias_pooled <- function(xbar1, xbar2, s1, s2, n1, n2,
                                 delta0 = 0, alpha = 0.05,
                                 alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  diff_hat <- xbar1 - xbar2
  sp2 <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)
  sp <- sqrt(sp2)
  se <- sp * sqrt(1 / n1 + 1 / n2)
  tstat <- (diff_hat - delta0) / se
  gl <- n1 + n2 - 2

  pvalor <- switch(
    alternative,
    "two.sided" = 2 * (1 - pt(abs(tstat), df = gl)),
    "less"      = pt(tstat, df = gl),
    "greater"   = 1 - pt(tstat, df = gl)
  )

  tcrit <- qt(1 - alpha / 2, df = gl)
  ic <- c(diff_hat - tcrit * se, diff_hat + tcrit * se)

  list(
    estimacion = diff_hat,
    sp2 = sp2,
    sp = sp,
    error_estandar = se,
    estadistico = tstat,
    gl = gl,
    pvalor = pvalor,
    ic = ic,
    decision = interpretar_p(pvalor, alpha)
  )
}

separador("2) DIFERENCIA DE MEDIAS CON t Y VARIANZA COMBINADA")

res_pooled <- prueba_medias_pooled(
  xbar1 = 83, xbar2 = 76,
  s1 = 8, s2 = 9,
  n1 = 15, n2 = 14,
  delta0 = 0,
  alternative = "greater"
)

mostrar_resultado("Estimacion de mu1 - mu2", res_pooled$estimacion)
mostrar_resultado("Sp^2", res_pooled$sp2)
mostrar_resultado("Sp", res_pooled$sp)
mostrar_resultado("Error estandar", res_pooled$error_estandar)
mostrar_resultado("Estadistico t", res_pooled$estadistico)
mostrar_resultado("Grados de libertad", res_pooled$gl)
mostrar_resultado("p-valor", res_pooled$pvalor)
cat("IC 95%                            : [", round(res_pooled$ic[1], 4), ", ", round(res_pooled$ic[2], 4), "]\n", sep = "")
cat("Decision                          : ", res_pooled$decision, "\n", sep = "")

# Verificación con t.test() en R:
# var.equal = TRUE fuerza la versión pooled.
cat("\nVerificacion con t.test(var.equal = TRUE):\n")
print(t.test(
  x = rnorm(15, mean = 83, sd = 8),
  y = rnorm(14, mean = 76, sd = 9),
  var.equal = TRUE
))
cat("Nota: aqui usamos datos simulados solo para mostrar la sintaxis de la funcion.\n")

###############################################################
# 3) DIFERENCIA DE MEDIAS: WELCH (VARIANZAS DESIGUALES)
###############################################################

# Marco teórico:
# Cuando no es razonable suponer igualdad de varianzas, usamos
# el error estándar no combinado y los grados de libertad aproximados
# de Welch-Satterthwaite.

prueba_medias_welch <- function(xbar1, xbar2, s1, s2, n1, n2,
                                delta0 = 0, alpha = 0.05,
                                alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  diff_hat <- xbar1 - xbar2
  se <- sqrt(s1^2 / n1 + s2^2 / n2)
  tstat <- (diff_hat - delta0) / se

  numerador <- (s1^2 / n1 + s2^2 / n2)^2
  denominador <- ((s1^2 / n1)^2 / (n1 - 1)) + ((s2^2 / n2)^2 / (n2 - 1))
  gl <- numerador / denominador

  pvalor <- switch(
    alternative,
    "two.sided" = 2 * (1 - pt(abs(tstat), df = gl)),
    "less"      = pt(tstat, df = gl),
    "greater"   = 1 - pt(tstat, df = gl)
  )

  tcrit <- qt(1 - alpha / 2, df = gl)
  ic <- c(diff_hat - tcrit * se, diff_hat + tcrit * se)

  list(
    estimacion = diff_hat,
    error_estandar = se,
    estadistico = tstat,
    gl = gl,
    pvalor = pvalor,
    ic = ic,
    decision = interpretar_p(pvalor, alpha)
  )
}

separador("3) DIFERENCIA DE MEDIAS CON WELCH")

res_welch <- prueba_medias_welch(
  xbar1 = 44, xbar2 = 39,
  s1 = 6, s2 = 12,
  n1 = 12, n2 = 10,
  delta0 = 0,
  alternative = "two.sided"
)

mostrar_resultado("Estimacion de mu1 - mu2", res_welch$estimacion)
mostrar_resultado("Error estandar", res_welch$error_estandar)
mostrar_resultado("Estadistico t de Welch", res_welch$estadistico)
mostrar_resultado("Grados de libertad aprox.", res_welch$gl)
mostrar_resultado("p-valor", res_welch$pvalor)
cat("IC 95%                            : [", round(res_welch$ic[1], 4), ", ", round(res_welch$ic[2], 4), "]\n", sep = "")
cat("Decision                          : ", res_welch$decision, "\n", sep = "")

###############################################################
# 4) MUESTRAS PAREADAS
###############################################################

# Marco teórico:
# Si las mediciones vienen en pares naturales, no se comparan dos medias
# por separado: se trabaja con las diferencias D_i = X_{1i} - X_{2i}.
# Entonces el problema se reduce a una sola muestra.

prueba_pareada_manual <- function(dbar, sd, n,
                                  mu_d0 = 0, alpha = 0.05,
                                  alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  se <- sd / sqrt(n)
  tstat <- (dbar - mu_d0) / se
  gl <- n - 1

  pvalor <- switch(
    alternative,
    "two.sided" = 2 * (1 - pt(abs(tstat), df = gl)),
    "less"      = pt(tstat, df = gl),
    "greater"   = 1 - pt(tstat, df = gl)
  )

  tcrit <- qt(1 - alpha / 2, df = gl)
  ic <- c(dbar - tcrit * se, dbar + tcrit * se)

  list(
    estimacion = dbar,
    error_estandar = se,
    estadistico = tstat,
    gl = gl,
    pvalor = pvalor,
    ic = ic,
    decision = interpretar_p(pvalor, alpha)
  )
}

separador("4) MUESTRAS PAREADAS")

res_pareada <- prueba_pareada_manual(
  dbar = 4.8,
  sd = 5.5,
  n = 10,
  mu_d0 = 0,
  alternative = "greater"
)

mostrar_resultado("Diferencia media", res_pareada$estimacion)
mostrar_resultado("Error estandar", res_pareada$error_estandar)
mostrar_resultado("Estadistico t", res_pareada$estadistico)
mostrar_resultado("Grados de libertad", res_pareada$gl)
mostrar_resultado("p-valor", res_pareada$pvalor)
cat("IC 95%                            : [", round(res_pareada$ic[1], 4), ", ", round(res_pareada$ic[2], 4), "]\n", sep = "")
cat("Decision                          : ", res_pareada$decision, "\n", sep = "")

###############################################################
# 5) DIFERENCIA DE PROPORCIONES
###############################################################

# Marco teórico:
# p1_hat = x1/n1, p2_hat = x2/n2
# Estimador puntual: p1_hat - p2_hat
# Para el intervalo usamos error estándar NO combinado.
# Para probar H0: p1 = p2 usamos la proporción combinada.

prueba_proporciones <- function(x1, n1, x2, n2,
                                alpha = 0.05,
                                alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  p1 <- x1 / n1
  p2 <- x2 / n2
  diff_hat <- p1 - p2

  se_ic <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  zcrit <- qnorm(1 - alpha / 2)
  ic <- c(diff_hat - zcrit * se_ic, diff_hat + zcrit * se_ic)

  pc <- (x1 + x2) / (n1 + n2)
  se_h0 <- sqrt(pc * (1 - pc) * (1 / n1 + 1 / n2))
  z <- diff_hat / se_h0

  pvalor <- switch(
    alternative,
    "two.sided" = 2 * (1 - pnorm(abs(z))),
    "less"      = pnorm(z),
    "greater"   = 1 - pnorm(z)
  )

  # Chequeo simple de condiciones de aproximación normal.
  chequeo <- c(
    x1,
    n1 - x1,
    x2,
    n2 - x2
  )

  list(
    p1 = p1,
    p2 = p2,
    estimacion = diff_hat,
    se_ic = se_ic,
    pc = pc,
    se_h0 = se_h0,
    estadistico = z,
    pvalor = pvalor,
    ic = ic,
    min_frecuencia = min(chequeo),
    decision = interpretar_p(pvalor, alpha)
  )
}

separador("5) DIFERENCIA DE PROPORCIONES")

res_prop <- prueba_proporciones(
  x1 = 72, n1 = 300,
  x2 = 39, n2 = 280,
  alternative = "two.sided"
)

mostrar_resultado("p1_hat", res_prop$p1)
mostrar_resultado("p2_hat", res_prop$p2)
mostrar_resultado("Estimacion p1 - p2", res_prop$estimacion)
mostrar_resultado("SE para IC", res_prop$se_ic)
mostrar_resultado("Proporcion combinada", res_prop$pc)
mostrar_resultado("SE bajo H0", res_prop$se_h0)
mostrar_resultado("Estadistico Z", res_prop$estadistico)
mostrar_resultado("p-valor", res_prop$pvalor)
cat("IC 95%                            : [", round(res_prop$ic[1], 4), ", ", round(res_prop$ic[2], 4), "]\n", sep = "")
mostrar_resultado("Minima frecuencia observada", res_prop$min_frecuencia)
cat("Decision                          : ", res_prop$decision, "\n", sep = "")

###############################################################
# 6) RAZON DE VARIANZAS CON F
###############################################################

# Marco teórico:
# Si las poblaciones son normales e independientes, entonces
# (S1^2 / sigma1^2) / (S2^2 / sigma2^2) sigue una F.
# Bajo H0: sigma1^2 / sigma2^2 = 1, usamos Fobs = S1^2 / S2^2.
# OJO: este procedimiento es muy sensible a no normalidad.

prueba_f_varianzas <- function(s1_2, s2_2, n1, n2,
                               alpha = 0.05,
                               alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  fobs <- s1_2 / s2_2
  gl1 <- n1 - 1
  gl2 <- n2 - 1

  pvalor <- switch(
    alternative,
    "greater"   = 1 - pf(fobs, df1 = gl1, df2 = gl2),
    "less"      = pf(fobs, df1 = gl1, df2 = gl2),
    "two.sided" = 2 * min(pf(fobs, df1 = gl1, df2 = gl2),
                           1 - pf(fobs, df1 = gl1, df2 = gl2))
  )

  # Intervalo bilateral para sigma1^2 / sigma2^2.
  f_inf <- qf(alpha / 2, df1 = gl1, df2 = gl2)
  f_sup <- qf(1 - alpha / 2, df1 = gl1, df2 = gl2)
  ic <- c(fobs / f_sup, fobs / f_inf)

  list(
    razon_muestral = fobs,
    gl1 = gl1,
    gl2 = gl2,
    pvalor = pvalor,
    ic = ic,
    decision = interpretar_p(pvalor, alpha)
  )
}

separador("6) RAZON DE VARIANZAS CON F")

res_f <- prueba_f_varianzas(
  s1_2 = 49,
  s2_2 = 16,
  n1 = 16,
  n2 = 12,
  alternative = "greater"
)

mostrar_resultado("Razon muestral S1^2 / S2^2", res_f$razon_muestral)
mostrar_resultado("gl1", res_f$gl1)
mostrar_resultado("gl2", res_f$gl2)
mostrar_resultado("p-valor", res_f$pvalor)
cat("IC 95%                            : [", round(res_f$ic[1], 4), ", ", round(res_f$ic[2], 4), "]\n", sep = "")
cat("Decision                          : ", res_f$decision, "\n", sep = "")

###############################################################
# 7) SUPUESTOS: NORMALIDAD Y HOMOCEDASTICIDAD
###############################################################

separador("7) SUPUESTOS: NORMALIDAD Y HOMOCEDASTICIDAD")

# Generamos datos de ejemplo solo para mostrar herramientas diagnósticas.
set.seed(123)
grupo1 <- rnorm(25, mean = 50, sd = 8)
grupo2 <- rnorm(25, mean = 47, sd = 12)

cat("\nShapiro-Wilk para normalidad:\n")
print(shapiro.test(grupo1))
print(shapiro.test(grupo2))

cat("\nKolmogorov-Smirnov (uso docente, con cautela):\n")
cat("Atencion: si se estiman media y desvio con la misma muestra, la referencia exacta ya no es KS puro.\n")
print(ks.test(grupo1, "pnorm", mean(grupo1), sd(grupo1)))
print(ks.test(grupo2, "pnorm", mean(grupo2), sd(grupo2)))

cat("\nBartlett para igualdad de varianzas:\n")
print(bartlett.test(list(grupo1, grupo2)))
cat("Nota: Bartlett es potente bajo normalidad, pero sensible a no normalidad.\n")

# Levene no está en base R. Si el paquete 'car' está disponible, se usa.
if (requireNamespace("car", quietly = TRUE)) {
  cat("\nLevene (paquete car):\n")
  datos <- data.frame(
    valor = c(grupo1, grupo2),
    grupo = factor(rep(c("G1", "G2"), each = 25))
  )
  print(car::leveneTest(valor ~ grupo, data = datos))
} else {
  cat("\nLevene no se ejecuta porque el paquete 'car' no esta instalado.\n")
  cat("Instalacion sugerida: install.packages('car')\n")
}


install.packages("Sleuth3")
library(Sleuth3)
help(package = "Sleuth3")
data(case0102) 
View(case0102) 

install.packages("psych")
library(psych)
describeBy(Salary ~ Sex, data = case0102)
boxplot(case0102$Salary, case0102$Sex)

library(dplyr)
salario_mujeres <- case0102 %>%   filter(Sex == "Female")
salario_hombres <- case0102 %>%   filter(Sex == "Male")
View(salario_mujeres)
salario_hombres

var.test(Salary ~ Sex, data = case0102)
