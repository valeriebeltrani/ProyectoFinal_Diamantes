# =====================================================================
# PROYECTO FINAL - DETERMINANTES DEL PRECIO DE LOS DIAMANTES
# Integrantes: Valerie Beltrán, Juanita Heredia
# PARTE A: base, datos, metodología y modelos 1 a 3
#   M1: simple lineal-lineal   precio    ~ quilates
#   M2: simple log-lineal      ln_precio ~ quilates
#   M3: múltiple lineal-lineal precio    ~ quilates + controles
# Base: diamonds.csv (Kaggle), BASE COMPLETA, 219.703 diamantes, sin filtros
# Ecuación: Y = C + Controles + Q
# =====================================================================
 
# PAQUETES Y OPCIONES
paquetes <- c("tidyverse", "ggthemes", "jtools", "broom", "flextable",
             "officer", "huxtable", "car", "lmtest", "sandwich")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan) > 0) install.packages(faltan)
 
library(tidyverse)   # readr, dplyr, ggplot2, forcats, tidyr
library(ggthemes)    # theme_tufte()
library(jtools)      # summ() y export_summs()
library(broom)       # tidy()
library(flextable)   # tablas a Word


options(scipen = 999)
theme_set(theme_tufte(base_size = 12))
color_base <- "#e10f83"
dir.create("graficas", showWarnings = FALSE)
dir.create("tablas", showWarnings = FALSE)
 
guardar_grafica <- function(p, nombre, ancho = 9, alto = 5) {
  p <- p +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      text = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )


  ggsave(
    file.path("graficas", nombre),
    plot = p,
    width = ancho,
    height = alto,
    dpi = 300,
    bg = "white"
  )
}
 
# Guarda un data frame como tabla de Word
a_word <- function(df, titulo, ruta, fuente = "Elaboración propia.") {
 flextable(df) |>
   flextable::set_caption(titulo) |>
   colformat_double(digits = 3) |>
   autofit() |>
   theme_vanilla() |>
   add_footer_lines(fuente) |>
   save_as_docx(path = ruta)
}


# 1. CARGA Y CALIDAD DE LA BASE
archivo_csv <- "diamonds.csv"
crudo <- read_csv(archivo_csv, show_col_types = FALSE)
cat("Filas:", nrow(crudo), "| Columnas:", ncol(crudo), "\n")
stopifnot(nrow(crudo) == 219703)   # confirma que se usa la base completa
 
# 1.1 Tabla de calidad de la base
calidad <- tibble(
 indicador = c("Filas", "Columnas",
               "Valores NA en la base cruda",
               "Filas duplicadas (sin contar el índice)",
               "Precios menores o iguales a 0",
               "Quilates menores o iguales a 0",
               "Profundidad = 0 (dato no registrado)",
               "Tabla = 0 (dato no registrado)"),
 resultado = c(nrow(crudo), ncol(crudo),
               sum(is.na(crudo)),
               sum(duplicated(crudo[-1])),
               sum(crudo$total_sales_price <= 0, na.rm = TRUE),
               sum(crudo$carat_weight <= 0, na.rm = TRUE),
               sum(crudo$depth_percent == 0, na.rm = TRUE),
               sum(crudo$table_percent == 0, na.rm = TRUE))
)
print(calidad)
 
# 1.2 Selección y renombrado. La primera columna del csv es solo un índice.
diamantes <- crudo |>
 select(-1) |>
 transmute(
   precio           = total_sales_price,   # Y: precio de venta en USD
   quilates         = carat_weight,        # Q: peso en quilates
   forma            = cut,                 # forma de la piedra (Round, Oval, Pear...)
   color            = color,
   claridad         = clarity,
   calidad_corte    = cut_quality,
   laboratorio      = lab,
   simetria         = symmetry,
   pulido           = polish,
   ojo_limpio       = eye_clean,
   fluor_intensidad = fluor_intensity,
   culet_tamano     = culet_size,
   culet_condicion  = culet_condition,
   cintura_max      = girdle_max,
   profundidad      = depth_percent,
   tabla            = table_percent
 ) |>
 # si alguna categórica trae NA, se trata como "unknown" (no se pierden filas)
 mutate(across(where(is.character), ~ replace_na(.x, "unknown")),
        profundidad = coalesce(profundidad, 0),
        tabla = coalesce(tabla, 0))
 
cat("Valores faltantes por columna:\n")
print(colSums(is.na(diamantes)))


# 2. PREPARACIÓN DE VARIABLES
# 2.1 Logaritmo natural
stopifnot(all(diamantesprecio>0),all(diamantes$quilates > 0))
diamantes <- diamantes |>
 mutate(ln_precio = log(precio),
        ln_quilates = log(quilates))
 
# 2.2 Profundidad y tabla traen ceros que son datos no registrados. Para no perder
# filas (base completa) se crea una dummy "faltante" y el cero se reemplaza por el
# promedio de los valores válidos.
prof_media  <- mean(diamantes$profundidad[diamantes$profundidad > 0])
tabla_media <- mean(diamantes$tabla[diamantes$tabla > 0])
 
diamantes <- diamantes |>
 mutate(
   profundidad_faltante = as.integer(profundidad == 0),
   tabla_faltante       = as.integer(tabla == 0),
   profundidad = if_else(profundidad == 0, prof_media, profundidad),
   tabla       = if_else(tabla == 0, tabla_media, tabla)
 )
cat("Diamantes con profundidad no registrada:", sum(diamantes$profundidad_faltante), "\n")
cat("Diamantes con tabla no registrada:      ", sum(diamantes$tabla_faltante), "\n")
 
# 2.3 Categóricas -> factores. Referencia = categoría más frecuente.
niveles_color    <- c("D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "Fancy")
niveles_claridad <- c("I3", "I2", "I1", "SI3", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF")
niveles_corte    <- c("Excellent", "Ideal", "Very Good", "Good", "Fair", "unknown")
 
con_referencia <- function(x, niveles = NULL) {
 x <- as.character(x)
 if (is.null(niveles)) niveles <- sort(unique(x))
 ref <- names(which.max(table(x)))
 fct_relevel(factor(x, levels = niveles), ref)
}
 
diamantes <- diamantes |>
 mutate(
   color         = if_else(color == "unknown", "Fancy", color),
   color         = con_referencia(color, niveles_color),
   claridad      = con_referencia(claridad, niveles_claridad),
   calidad_corte = con_referencia(calidad_corte, niveles_corte),
   across(c(forma, laboratorio, simetria, pulido, ojo_limpio,
            fluor_intensidad, culet_tamano, culet_condicion, cintura_max),
          con_referencia)
 )
 
# Si esto falla, hay una categoría que no está en los niveles definidos arriba
stopifnot(!anyNA(diamantes))
cat("Categoría de referencia de cada factor:\n")
print(diamantes |> summarize(across(where(is.factor), ~ levels(.x)[1])), width = Inf)
 
# 2.4 Base limpia para la Parte B
saveRDS(diamantes, "diamantes_limpio.rds")


glosario <- tibble(
  variable = c(
    "Color", "Color", "Color", "Claridad",
    "Claridad", "Claridad", "Claridad",
    "Claridad", "Claridad", "Claridad"
  ),
  codigo = c(
    "D", "E", "F", "IF", "VVS1/VVS2",
    "VS1/VS2", "SI1/SI2", "I1", "I2", "I3"
  ),
  significado = c(
    "Excepcionalmente blanco",
    "Blanco",
    "Blanco, casi incoloro",
    "Internamente perfecto",
    "Inclusiones muy, muy pequeñas",
    "Inclusiones muy pequeñas",
    "Inclusiones pequeñas",
    "Inclusiones visibles",
    "Inclusiones evidentes",
    "Inclusiones muy evidentes"
  )
)


a_word(
  glosario,
  "Glosario de categorías y códigos de los diamantes",
  "tablas/glosario_categorias.docx"
)


# 3. DATOS
# 3.1 Mínimo, máximo y moda, media, mediana, desviación y asimetría
moda <- function(x) {
 valores <- unique(x)
 valores[which.max(tabulate(match(x, valores)))]
}
asimetria <- function(x) {
 s <- sqrt(mean((x - mean(x))^2))
 mean((x - mean(x))^3) / s^3
}
curtosis <- function(x) {
 s <- sqrt(mean((x - mean(x))^2))
 mean((x - mean(x))^4) / s^4
}
resumen_var <- function(x, nombre) {
 tibble(variable = nombre, n = length(x), minimo = min(x), maximo = max(x),
        media = mean(x), mediana = median(x), moda = moda(x),
        desv_est = sd(x), asimetria = asimetria(x))
}
 
tabla_desc <- bind_rows(
 resumen_var(diamantes$precio, "Precio (USD)"),
 resumen_var(diamantes$ln_precio, "ln(Precio)"),
 resumen_var(diamantes$quilates, "Quilates"),
 resumen_var(diamantes$ln_quilates, "ln(Quilates)"),
resumen_var(diamantes$profundidad[diamantes$profundidad_faltante == 0],
            "Profundidad (%), valores registrados"),
resumen_var(diamantes$tabla[diamantes$tabla_faltante == 0],
            "Tabla (%), valores registrados")
)
print(tabla_desc, width = Inf)
 
# La media del precio supera a la mediana por la cola derecha
cat(sprintf("Precio: media %s vs. mediana %s USD (asimetría %.2f) -> cola derecha larga.\n",
           scales::comma(mean(diamantes$precio), accuracy = 1),
           scales::comma(median(diamantes$precio), accuracy = 1),
           asimetria(diamantes$precio)))
 
# 3.2 Frecuencias de las categóricas
frecuencia <- function(df, var) {
 df |>
   count({{ var }}, sort = TRUE) |>
   mutate(porcentaje = round(100 * n / sum(n), 1))
}
tabla_claridad <- frecuencia(diamantes, claridad)
tabla_color    <- frecuencia(diamantes, color)
tabla_corte    <- frecuencia(diamantes, calidad_corte)
tabla_forma    <- frecuencia(diamantes, forma)
 
print(tabla_claridad)
cat("Claridad que más prevalece:", as.character(tabla_claridad$claridad[1]),
   "con", tabla_claridad$porcentaje[1], "% de los diamantes\n")
print(tabla_color)
print(tabla_corte)
print(tabla_forma)
 
# 3.3 Gráficas
g_hist_precio <- diamantes |>
 select(precio, ln_precio) |>
 pivot_longer(everything(), names_to = "escala", values_to = "valor") |>
 mutate(escala = factor(escala, levels = c("precio", "ln_precio"),
                        labels = c("Precio (USD)", "ln(Precio)"))) |>
 ggplot(aes(valor)) +
 geom_histogram(bins = 60, fill = color_base, alpha = 0.85) +
 facet_wrap(~ escala, scales = "free") +
 labs(title = "Distribución del precio: antes y después del logaritmo natural",
      x = NULL, y = "Número de diamantes")
print(g_hist_precio)
guardar_grafica(g_hist_precio, "g01_hist_precio_vs_ln.png")
 
g_hist_quilates <- diamantes |>
 select(quilates, ln_quilates) |>
 pivot_longer(everything(), names_to = "escala", values_to = "valor") |>
 mutate(escala = factor(escala, levels = c("quilates", "ln_quilates"),
                        labels = c("Quilates", "ln(Quilates)"))) |>
 ggplot(aes(valor)) +
 geom_histogram(bins = 60, fill = color_base, alpha = 0.85) +
 facet_wrap(~ escala, scales = "free") +
 labs(title = "Distribución de los quilates: antes y después del logaritmo natural",
      x = NULL, y = "Número de diamantes")
print(g_hist_quilates)
guardar_grafica(g_hist_quilates, "g02_hist_quilates_vs_ln.png")
 
g_claridad <- tabla_claridad |>
 mutate(claridad = factor(claridad, levels = niveles_claridad)) |>
 ggplot(aes(x = claridad, y = n)) +
 geom_col(fill = color_base) +
 geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.4, size = 3.5) +
 scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.1))) +
 labs(title = "Distribución de la claridad", x = "Claridad (de peor a mejor)",
      y = "Número de diamantes")
print(g_claridad)
guardar_grafica(g_claridad, "g03_barras_claridad.png")
 
g_disp_nivel <- ggplot(diamantes, aes(quilates, precio)) +
 geom_point(color = color_base, size = 0.6, alpha = 0.15) +
 geom_smooth(method = "lm", formula = y ~ x, color = "firebrick") +
 scale_y_continuous(labels = scales::label_dollar()) +
 labs(title = "Precio vs. quilates (nivel)", x = "Quilates", y = "Precio (USD)")
print(g_disp_nivel)
guardar_grafica(g_disp_nivel, "g04_dispersion_precio_quilates.png")
 
g_disp_log <- ggplot(diamantes, aes(quilates, ln_precio)) +
 geom_point(color = color_base, size = 0.6, alpha = 0.15) +
 geom_smooth(method = "lm", formula = y ~ x, color = "firebrick") +
 labs(title = "ln(Precio) vs. quilates", x = "Quilates", y = "ln(Precio)")
print(g_disp_log)
guardar_grafica(g_disp_log, "g05_dispersion_lnprecio_quilates.png")
 
g_caja_claridad <- diamantes |>
 mutate(claridad = factor(claridad, levels = niveles_claridad)) |>
 ggplot(aes(claridad, ln_precio)) +
 geom_boxplot(fill = "grey90", color = color_base, outlier.size = 0.3, outlier.alpha = 0.3) +
 labs(title = "ln(Precio) según claridad", x = "Claridad (de peor a mejor)", y = "ln(Precio)")
print(g_caja_claridad)
guardar_grafica(g_caja_claridad, "g06_caja_lnprecio_claridad.png")
 
# 3.4 Correlaciones
correlaciones <- diamantes |>
 summarize(r_precio_quilates   = cor(quilates, precio),
           r_lnprecio_quilates = cor(quilates, ln_precio),
           r_lnprecio_lnquilates = cor(ln_quilates, ln_precio))
print(correlaciones)
# La correlación describe asociación, NO causalidad.


# 4. METODOLOGÍA
# Corte transversal. Y = precio (USD) y ln(precio); Q = quilates.
#   M1: precio    = b0 + b1*quilates + e
#   M2: ln_precio = b0 + b1*quilates + e
#   M3: precio    = b0 + b1*quilates + controles + e
# (M4, múltiple log-lineal, lo corre la Parte B con la misma lista de controles.)
# Para cada pendiente: H0: b1 = 0 vs H1: b1 != 0. Prueba F global: H0: todas las
# pendientes son 0 vs H1: al menos una distinta de 0.
#
# Controles: profundidad y tabla (+ dummies de dato no registrado); dummies de color,
# claridad, calidad del corte, forma, laboratorio, simetría, pulido, ojo limpio,
# fluorescencia, culet (tamaño y condición) y grosor máximo de la cintura.
# Fuera: medidas físicas (largo, ancho, alto) porque determinan mecánicamente los
# quilates; fancy_color_* porque repiten "Fancy" de color; girdle_min y fluor_color
# porque repiten girdle_max y fluor_intensidad.
controles <- c("profundidad", "tabla", "profundidad_faltante", "tabla_faltante",
              "color", "claridad", "calidad_corte", "forma", "laboratorio",
              "simetria", "pulido", "ojo_limpio", "fluor_intensidad",
              "culet_tamano", "culet_condicion", "cintura_max")
 
f_m1 <- precio ~ quilates
f_m2 <- ln_precio ~ quilates
f_m3 <- reformulate(c("quilates", controles), response = "precio")
print(f_m1); print(f_m2); print(f_m3)
 
# 5. RESULTADOS
# 5.1 a 5.3 Estimación
m1 <- lm(f_m1, data = diamantes)
m2 <- lm(f_m2, data = diamantes)
m3 <- lm(f_m3, data = diamantes)
 
print(summ(m1)); print(summary(m1))
print(summ(m2)); print(summary(m2))
print(summ(m3)); print(summary(m3))
 
# Revisión: si algún coeficiente sale NA hay colinealidad exacta y hay que quitar ese control
cat("Coeficientes NA en M3 (debería ser 0):", sum(is.na(coef(m3))), "\n")
 
# 5.4 Inferencia sobre la pendiente de quilates
tabla_quilates <- bind_rows(
 tidy(m1, conf.int = TRUE) |> filter(term == "quilates") |> mutate(modelo = "M1 simple lin-lin"),
 tidy(m2, conf.int = TRUE) |> filter(term == "quilates") |> mutate(modelo = "M2 simple log-lin"),
 tidy(m3, conf.int = TRUE) |> filter(term == "quilates") |> mutate(modelo = "M3 múltiple lin-lin")
) |>
 select(modelo, coeficiente = estimate, error_est = std.error, t = statistic,
        valor_p = p.value, ic95_inf = conf.low, ic95_sup = conf.high)
print(tabla_quilates, width = Inf)
 
# 5.5 Interpretación
b1_m1 <- coef(m1)[["quilates"]]
b1_m2 <- coef(m2)[["quilates"]]
b1_m3 <- coef(m3)[["quilates"]]
cat("\n--- INTERPRETACIÓN ---\n")
cat(sprintf("M1: cada quilate adicional se asocia con %s USD más en el precio promedio (se rechaza H0 si p < 0,05).\n",
           scales::comma(b1_m1, accuracy = 1)))
cat(sprintf("M1 intercepto: %s USD. Un diamante de 0 quilates no existe: no se interpreta en términos de negocio.\n",
           scales::comma(coef(m1)[["(Intercept)"]], accuracy = 1)))
cat(sprintf("M2: cada quilate adicional se asocia con un aumento de aprox. %.1f%% en el precio (100*b1); exacto: %.1f%% (100*(exp(b1)-1)).\n",
           100 * b1_m2, 100 * (exp(b1_m2) - 1)))
cat(sprintf("M3: manteniendo constantes los controles, cada quilate adicional se asocia con %s USD más.\n",
           scales::comma(b1_m3, accuracy = 1)))
cat(sprintf("Cambio de M1 a M3: el coeficiente de quilates pasa de %s a %s (%.1f%%). Parte de la relación simple venía de que los quilates se mueven junto con la calidad.\n",
           scales::comma(b1_m1, accuracy = 1), scales::comma(b1_m3, accuracy = 1),
           100 * (b1_m3 / b1_m1 - 1)))
 
# Efecto de color, claridad y calidad de corte en M3 (frente a la categoría de referencia)
efectos_controles <- tidy(m3, conf.int = TRUE) |>
 filter(str_detect(term, "^(color|claridad|calidad_corte)")) |>
 select(term, estimate, std.error, p.value, conf.low, conf.high)
print(efectos_controles, n = Inf)
 
# 5.6 Predicción (solo dentro del rango observado de quilates)
cat("Rango observado de quilates:", min(diamantes$quilates), "a", max(diamantes$quilates), "\n")
nuevos <- tibble(quilates = c(0.5, 1, 2))
# M2 predice ln(precio): al volver a USD con exp() se subestima la media, por eso
# se aplica la corrección "smearing" (Duan), como en el caso.
smearing_m2 <- mean(exp(resid(m2)))
prediccion <- tibble(
 quilates = nuevos$quilates,
 precio_M1 = as.numeric(predict(m1, newdata = nuevos)),
 precio_M2 = smearing_m2 * exp(as.numeric(predict(m2, newdata = nuevos)))
)
print(prediccion)
 
# 5.7 Resumen de ajuste
resumen_modelos <- tibble(
 modelo = c("M1 simple lineal-lineal", "M2 simple log-lineal", "M3 múltiple lineal-lineal"),
 y = c("precio", "ln(precio)", "precio"),
 n = c(nobs(m1), nobs(m2), nobs(m3)),
 r2 = c(summary(m1)$r.squared, summary(m2)$r.squared, summary(m3)$r.squared),
r2_ajustado = c(summary(m1)$adj.r.squared, summary(m2)$adj.r.squared, summary(m3)$adj.r.squared)
)
print(resumen_modelos)
# M1/M3 y M2 tienen distinta variable dependiente: el R2 es solo referencia. La
# comparación en la misma escala (USD) está en la sección 8.
 
# 5.8: log-log simple (coeficiente = elasticidad)
m2_loglog <- lm(ln_precio ~ ln_quilates, data = diamantes)
cat(sprintf("Opcional log-log simple: elasticidad = %.3f | R2 = %.3f\n",
           coef(m2_loglog)[["ln_quilates"]], summary(m2_loglog)$r.squared))
 
g_disp_loglog <- ggplot(diamantes, aes(ln_quilates, ln_precio)) +
 geom_point(color = color_base, size = 0.6, alpha = 0.15) +
 geom_smooth(method = "lm", formula = y ~ x, color = "firebrick") +
 labs(title = "ln(Precio) vs. ln(Quilates)", x = "ln(Quilates)", y = "ln(Precio)")
print(g_disp_loglog)
guardar_grafica(g_disp_loglog, "g07_dispersion_loglog.png")
 
# 6. DIAGNÓSTICO: NORMALIDAD Y RESIDUOS
residuos <- bind_rows(
 tibble(modelo = "M1 simple lineal-lineal",    residuo = as.numeric(resid(m1)), ajustado = as.numeric(fitted(m1))),
 tibble(modelo = "M2 simple log-lineal",       residuo = as.numeric(resid(m2)), ajustado = as.numeric(fitted(m2))),
 tibble(modelo = "M3 múltiple lineal-lineal",  residuo = as.numeric(resid(m3)), ajustado = as.numeric(fitted(m3)))
)
 
tabla_normalidad <- residuos |>
 group_by(modelo) |>
 summarize(asimetria = asimetria(residuo), curtosis = curtosis(residuo))
print(tabla_normalidad)   # normal: asimetría 0 y curtosis 3
 
g_res_hist <- ggplot(residuos, aes(residuo)) +
 geom_histogram(bins = 80, fill = color_base, alpha = 0.85) +
 facet_wrap(~ modelo, scales = "free") +
 labs(title = "Histograma de los residuos", x = "Residuo", y = "Número de diamantes")
print(g_res_hist)
guardar_grafica(g_res_hist, "g08_hist_residuos.png", ancho = 11)
 
g_res_qq <- ggplot(residuos, aes(sample = residuo)) +
 stat_qq(size = 0.4, alpha = 0.3, color = color_base) +
 stat_qq_line(color = "firebrick") +
 facet_wrap(~ modelo, scales = "free") +
 labs(title = "QQ-plot de los residuos", x = "Cuantiles teóricos", y = "Cuantiles de los residuos")
print(g_res_qq)
guardar_grafica(g_res_qq, "g09_qq_residuos.png", ancho = 11)
 
# Residuos vs. ajustados (muestra de 20.000 puntos por modelo para que el gráfico sea ligero)
set.seed(123)
g_res_aj <- residuos |>
 group_by(modelo) |>
 slice_sample(n = 20000) |>
 ungroup() |>
 ggplot(aes(ajustado, residuo)) +
 geom_point(size = 0.4, alpha = 0.25, color = color_base) +
 geom_hline(yintercept = 0, color = "firebrick") +
 facet_wrap(~ modelo, scales = "free") +
 labs(title = "Residuos vs. valores ajustados", x = "Valor ajustado", y = "Residuo")
print(g_res_aj)
guardar_grafica(g_res_aj, "g10_residuos_vs_ajustados.png", ancho = 11)
 
# Shapiro-Wilk admite máximo 5.000 datos: se aplica a una muestra de los residuos
set.seed(123)
print(shapiro.test(sample(as.numeric(resid(m1)), 5000)))
print(shapiro.test(sample(as.numeric(resid(m2)), 5000)))
print(shapiro.test(sample(as.numeric(resid(m3)), 5000)))


# 7. DIAGNÓSTICO: MULTICOLINEALIDAD, HETEROCEDASTICIDAD, INFLUYENTES Y ERRORES ROBUSTOS
# 7.1 VIF del modelo múltiple. Con factores, car::vif entrega GVIF^(1/(2*Df)); se
# compara con sqrt(5) = 2,24 
vif_m3 <- tryCatch(car::vif(m3), error = function(e) NULL)
if (!is.null(vif_m3)) {
 print(round(vif_m3, 2))
 col_ajustada <- if (is.matrix(vif_m3)) vif_m3[, "GVIF^(1/(2*Df))"] else sqrt(vif_m3)
 cat("Variables por encima de sqrt(5):", paste(names(col_ajustada)[col_ajustada > sqrt(5)], collapse = ", "), "\n")
} else cat("No se pudo calcular el VIF (revisa colinealidad exacta en M3).\n")
 
# 7.2 Breusch-Pagan (H0: varianza de los errores constante)
bp <- tibble(
 modelo = c("M1", "M2", "M3"),
 estadistico = c(lmtest::bptest(m1)$statistic,
                 lmtest::bptest(m2)$statistic,
                 lmtest::bptest(m3)$statistic),
 valor_p     = c(lmtest::bptest(m1)$p.value,
                 lmtest::bptest(m2)$p.value,
                 lmtest::bptest(m3)$p.value)
)
print(bp)
 
# 7.3 Observaciones influyentes (distancia de Cook > 4/n). No se eliminan: se reportan.
cook <- tibble(
 modelo = c("M1", "M2", "M3"),
 influyentes = c(sum(cooks.distance(m1) > 4 / nobs(m1)),
                 sum(cooks.distance(m2) > 4 / nobs(m2)),
                 sum(cooks.distance(m3) > 4 / nobs(m3)))
) |> mutate(porcentaje = round(100 * influyentes / nrow(diamantes), 2))
print(cook)
 
# 7.4 Errores estándar robustos (HC1) para la pendiente de quilates
robusto <- function(m, nombre) {
 ct <- lmtest::coeftest(m, vcov. = sandwich::vcovHC(m, type = "HC1"))
 tibble(modelo = nombre,
        coeficiente = ct["quilates", "Estimate"],
        ee_robusto = ct["quilates", "Std. Error"],
        valor_p_robusto = ct["quilates", "Pr(>|t|)"])
}
tabla_robusta <- bind_rows(robusto(m1, "M1"), robusto(m2, "M2"), robusto(m3, "M3"))
print(tabla_robusta)
# Lectura: si la significancia de quilates se mantiene con errores robustos, la
# conclusión no depende del supuesto de varianza constante.

                   
# 8. COMPARACIÓN DE MODELOS EN LA MISMA ESCALA (USD)
# El R2 no sirve para comparar M2 con M1/M3 (distinta Y),
# así que se comparan los errores de predicción en USD. El corte es transversal: partición aleatoria 80/20.
set.seed(123)
idx <- sample(nrow(diamantes), size = round(0.8 * nrow(diamantes)))
train <- droplevels(diamantes[idx, ])
test  <- diamantes[-idx, ]
for (v in names(which(sapply(train, is.factor)))) {       # evita niveles nuevos en test
 test <- test[test[[v]] %in% levels(train[[v]]), ]
}
 
t1 <- lm(f_m1, data = train)
t2 <- lm(f_m2, data = train)
t3 <- lm(f_m3, data = train)
smearing_train <- mean(exp(resid(t2)))
 
metricas <- function(real, pred, nombre) {
 tibble(modelo = nombre, MAE = mean(abs(real - pred)), RMSE = sqrt(mean((real - pred)^2)))
}
tabla_pred <- bind_rows(
 metricas(test$precio, predict(t1, newdata = test), "M1 simple lin-lin"),
 metricas(test$precio, smearing_train * exp(predict(t2, newdata = test)), "M2 simple log-lin (smearing)"),
 metricas(test$precio, predict(t3, newdata = test), "M3 múltiple lin-lin")
)
print(tabla_pred)
cat("Observaciones de prueba:", nrow(test), "\n")

                   
# 9. TABLAS A WORD
# 9.1 Tabla de regresión completa
export_summs(m1, m2, m3,
            model.names = c("M1 simple lin-lin", "M2 simple log-lin", "M3 múltiple lin-lin"),
            statistics = c(N = "nobs", R2 = "r.squared", "R2 ajustado" = "adj.r.squared"),
            digits = 4, to.file = "docx",
            file.name = "tablas/tabla_regresion_parteA_completa.docx")
 
# 9.2 Versión compacta (los demás controles SÍ están en el modelo, solo se omiten de la tabla)
omitir <- grep("^(forma|laboratorio|simetria|pulido|ojo_limpio|fluor_intensidad|culet_tamano|culet_condicion|cintura_max)",
              names(coef(m3)), value = TRUE)
export_summs(m1, m2, m3,
            model.names = c("M1 simple lin-lin", "M2 simple log-lin", "M3 múltiple lin-lin"),
            statistics = c(N = "nobs", R2 = "r.squared", "R2 ajustado" = "adj.r.squared"),
            omit.coefs = omitir, digits = 4, to.file = "docx",
            file.name = "tablas/tabla_regresion_parteA_compacta.docx")
 
# 9.3 Otras tablas del informe
fuente <- "Fuente: base diamonds (Kaggle), 219.703 diamantes. Elaboración propia."
a_word(calidad,        "Calidad de la base",                              "tablas/tabla_calidad.docx", fuente)
a_word(tabla_desc,     "Estadísticas descriptivas de variables continuas", "tablas/tabla_descriptiva.docx", fuente)
a_word(tabla_quilates, "Coeficiente de quilates en los tres modelos",      "tablas/tabla_quilates.docx", fuente)
a_word(tabla_robusta,  "Pendiente de quilates con errores robustos (HC1)", "tablas/tabla_robusta.docx", fuente)
a_word(tabla_pred,     "Errores de predicción en USD (partición 80/20)",   "tablas/tabla_prediccion.docx", fuente)


# PARTE B: modelo 4, comparación de modelos, robustez y conclusiones
#     M4: múltiple log-lineal   ln_precio ~ quilates + controles
# Base: diamantes_limpio.rds (sale de la Parte A)
# Ecuación: Y = C + Controles + Q
# Pregunta: ¿cuánto aumenta el precio de un diamante por cada quilate
# adicional, teniendo en cuenta su color, claridad y corte?


if (!exists("m3")) stop("Primero hay que correr diamantes.R (Parte A)")


# error porcentual mediano de M1 a M3 (el promedio se dispara por las piedras
# grandes). Se calcula primero para liberar memoria de los modelos de prueba.
pct_mediano <- function(pred) 100 * median(abs(test$precio - pred) / test$precio)
if (!exists("pct_m1_m3")) {
  pct_m1_m3 <- c(pct_mediano(predict(t1, newdata = test)),
                 pct_mediano(smearing_train * exp(predict(t2, newdata = test))),
                 pct_mediano(predict(t3, newdata = test)))
  rm(t1, t2, t3)
  invisible(gc())
}


# 1. MODELO 4: MÚLTIPLE LOG-LINEAL (mismos controles de M3)
f_m4 <- reformulate(c("quilates", controles), response = "ln_precio")
m4 <- lm(f_m4, data = diamantes)
print(summ(m4))
print(summary(m4))
cat("Coeficientes NA en M4:", sum(is.na(coef(m4))), "\n")

# 1.1 Inferencia sobre quilates. H0: b1 = 0 vs H1: b1 != 0
inferencia_m4 <- tidy(m4, conf.int = TRUE) |>
  filter(term == "quilates") |>
  transmute(modelo = "M4 múltiple log-lin", coeficiente = estimate,
            error_est = std.error, t = statistic, valor_p = p.value,
            ic95_inf = conf.low, ic95_sup = conf.high)
print(inferencia_m4, width = Inf)


f_global <- summary(m4)$fstatistic
cat(sprintf("F global M4: F = %.1f, valor p = %.3g\n", f_global[1],
            pf(f_global[1], f_global[2], f_global[3], lower.tail = FALSE)))

# 1.2 Interpretación
b1_m4 <- coef(m4)[["quilates"]]
cat(sprintf("M4: un quilate más -> +%.1f%% en el precio (exp(b1)-1).\n",
            100 * (exp(b1_m4) - 1)))
cat(sprintf("M4: 0,1 quilates más -> +%.1f%% en el precio.\n",
            100 * (exp(0.1 * b1_m4) - 1)))

# 1.3 Efecto de color, claridad, corte y laboratorio en %
efectos_m4 <- tidy(m4, conf.int = TRUE) |>
  filter(str_detect(term, "^(color|claridad|calidad_corte|laboratorio)")) |>
  transmute(termino = term, coeficiente = estimate, valor_p = p.value,
            efecto_pct = 100 * (exp(estimate) - 1),
            ic95_inf_pct = 100 * (exp(conf.low) - 1),
            ic95_sup_pct = 100 * (exp(conf.high) - 1))
print(efectos_m4, n = Inf)


# 2. COMPARACIÓN DE LOS CUATRO MODELOS
nombres_mod <- c("M1 simple lin-lin", "M2 simple log-lin",
                 "M3 múltiple lin-lin", "M4 múltiple log-lin")

# 2.1 Errores en USD: se agrega M4 con la misma partición de la Parte A
t4 <- lm(f_m4, data = train)
pred_t4 <- mean(exp(resid(t4))) * exp(predict(t4, newdata = test))
rm(t4)
invisible(gc())

tabla_pred <- bind_rows(
  filter(tabla_pred, !str_detect(modelo, "^M4")),
  metricas(test$precio, pred_t4, "M4 múltiple log-lin (smearing)")
)
tabla_pred$error_pct_mediano <- c(pct_m1_m3, pct_mediano(pred_t4))
print(tabla_pred)

# 2.2 Tabla comparativa
modelos <- list(m1, m2, m3, m4)
coef_q  <- sapply(modelos, function(m) coef(m)[["quilates"]])

tabla_comparativa <- tibble(
  modelo    = nombres_mod,
  y         = c("precio", "ln(precio)", "precio", "ln(precio)"),
  controles = c("No", "No", "Sí", "Sí"),
  coef_quilates = coef_q,
  efecto_1_quilate = c(
    sprintf("+%s USD", scales::comma(coef_q[1], accuracy = 1)),
    sprintf("+%.1f%%", 100 * (exp(coef_q[2]) - 1)),
    sprintf("+%s USD", scales::comma(coef_q[3], accuracy = 1)),
    sprintf("+%.1f%%", 100 * (exp(coef_q[4]) - 1))),
  r2          = sapply(modelos, function(m) summary(m)$r.squared),
  r2_ajustado = sapply(modelos, function(m) summary(m)$adj.r.squared),
  MAE  = tabla_pred$MAE,
  RMSE = tabla_pred$RMSE,
  error_pct_mediano = tabla_pred$error_pct_mediano
)
print(tabla_comparativa, width = Inf)
# el R2 de M2 y M4 está en ln(precio), por eso se comparan los errores en USD


export_summs(m1, m2, m3, m4, model.names = nombres_mod,
             statistics = c(N = "nobs", R2 = "r.squared",
                            "R2 ajustado" = "adj.r.squared"),
             omit.coefs = omitir, digits = 4, to.file = "docx",
             file.name = "tablas/tabla_regresion_4modelos_compacta.docx")


ajustado_m3 <- as.numeric(fitted(m3))
rm(modelos)
invisible(gc())


# 3. ROBUSTEZ DEL MODELO 4
res_m4 <- tibble(
  quilates  = diamantes$quilates,
  precio    = diamantes$precio,
  ln_precio = diamantes$ln_precio,
  ajustado  = as.numeric(fitted(m4)),
  residuo   = as.numeric(resid(m4))
) |>
  mutate(precio_ajustado = mean(exp(residuo)) * exp(ajustado),
         residuo2 = residuo^2)


tabla_normalidad <- bind_rows(
  filter(tabla_normalidad, !str_detect(modelo, "^M4")),
  tibble(modelo = "M4 múltiple log-lineal",
         asimetria = asimetria(res_m4$residuo),
         curtosis  = curtosis(res_m4$residuo))
)
print(tabla_normalidad)


set.seed(123)
muestra <- slice_sample(res_m4, n = 20000)

# 3.1 Linealidad
g_lin_quilates <- ggplot(muestra, aes(quilates, residuo)) +
  geom_point(size = 0.4, alpha = 0.25, color = color_base) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "M4: residuos frente a quilates", x = "Quilates",
       y = "Residuo (escala ln)")
print(g_lin_quilates)
guardar_grafica(g_lin_quilates, "g11_m4_linealidad_quilates.png")

# RESET de Ramsey. H0: la forma funcional es correcta
reset_m4 <- lmtest::resettest(m4, power = 2:3, type = "fitted")
print(reset_m4)

# quilates al cuadrado
m4_cuad <- update(m4, . ~ . + I(quilates^2))
print(tidy(m4_cuad) |> filter(term %in% c("quilates", "I(quilates^2)")))
r2_cuad <- summary(m4_cuad)$r.squared
rm(m4_cuad)
invisible(gc())
cat(sprintf("R2 M4 = %.3f | R2 con quilates al cuadrado = %.3f\n",
            summary(m4)$r.squared, r2_cuad))

# 3.2 Varianza constante
bp_m4 <- lmtest::bptest(m4)
bp <- bind_rows(filter(bp, modelo != "M4"),
                tibble(modelo = "M4", estadistico = unname(bp_m4$statistic),
                       valor_p = bp_m4$p.value))
print(bp)


g_residuo2 <- ggplot(muestra, aes(ajustado, residuo2)) +
  geom_point(size = 0.4, alpha = 0.25, color = color_base) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "firebrick") +
  labs(title = "M4: residuos al cuadrado frente a ajustados",
       x = "ln(precio) ajustado", y = "Residuo al cuadrado")
print(g_residuo2)
guardar_grafica(g_residuo2, "g12_m4_residuos_cuadrado.png")


tabla_robusta <- bind_rows(filter(tabla_robusta, modelo != "M4"), robusto(m4, "M4"))
print(tabla_robusta)

# 3.3 Multicolinealidad. Se compara GVIF^(1/(2*Df)) con sqrt(5) = 2,24
# si car::vif falla (por ejemplo, por coeficientes NA) se hace el mismo
# cálculo a mano con la matriz de correlaciones
gvif_manual <- function(m) {
  ok  <- !is.na(coef(m))
  mm  <- model.matrix(m)
  asg <- attr(mm, "assign")[ok][-1]
  R   <- cor(mm[, ok, drop = FALSE][, -1, drop = FALSE])
  rm(mm)
  ld  <- function(A) as.numeric(determinant(A, logarithm = TRUE)$modulus)
  nombres <- attr(terms(m), "term.labels")
  map_dfr(unique(asg), function(j) {
    i <- which(asg == j)
    g <- exp(ld(R[i, i, drop = FALSE]) + ld(R[-i, -i, drop = FALSE]) - ld(R))
    tibble(variable = nombres[j], GVIF = g, gl = length(i),
           GVIF_ajustado = g^(1 / (2 * length(i))))
  })
}
tabla_vif <- tryCatch(
  as_tibble(car::vif(m4), rownames = "variable") |>
    rename(gl = Df, GVIF_ajustado = GVIF^(1/(2*Df))),
  error = function(e) gvif_manual(m4)
) |>
  mutate(supera_umbral = if_else(GVIF_ajustado > sqrt(5), "Sí", "No")) |>
  arrange(desc(GVIF_ajustado))
print(tabla_vif, n = Inf)

# 3.4 Ajustados contra reales
g_aj_real_ln <- ggplot(muestra, aes(ajustado, ln_precio)) +
  geom_point(size = 0.4, alpha = 0.25, color = color_base) +
  geom_abline(intercept = 0, slope = 1, color = "firebrick") +
  labs(title = "M4: ln(precio) real frente a ajustado",
       x = "ln(precio) ajustado", y = "ln(precio) real")
print(g_aj_real_ln)
guardar_grafica(g_aj_real_ln, "g13_m4_ajustado_vs_real_ln.png")


g_aj_real_usd <- ggplot(muestra, aes(precio_ajustado, precio)) +
  geom_point(size = 0.4, alpha = 0.25, color = color_base) +
  geom_abline(intercept = 0, slope = 1, color = "firebrick") +
  scale_x_log10(labels = scales::label_dollar()) +
  scale_y_log10(labels = scales::label_dollar()) +
  labs(title = "M4: precio real frente a ajustado (USD, ejes en log)",
       x = "Precio ajustado (USD)", y = "Precio real (USD)")
print(g_aj_real_usd)
guardar_grafica(g_aj_real_usd, "g14_m4_ajustado_vs_real_usd.png")

# 3.5 Errores al cuadrado por rango de quilates (M3 vs M4, en USD)
n_obs_total <- nrow(diamantes)
errores_rango <- tibble(
  quilates = diamantes$quilates,
  precio   = diamantes$precio,
  e2_m3    = (diamantes$precio - ajustado_m3)^2,
  e2_m4    = (diamantes$precio - res_m4$precio_ajustado)^2
) |>
  mutate(rango = cut(quilates, breaks = c(0, 0.5, 1, 2, 3, 5, Inf),
                     labels = c("hasta 0,5", "0,5 a 1", "1 a 2", "2 a 3",
                                "3 a 5", "más de 5"))) |>
  group_by(rango) |>
  summarize(diamantes = n(),
            porcentaje = round(100 * n() / n_obs_total, 1),
            precio_mediano = median(precio),
            RMSE_M3 = sqrt(mean(e2_m3)),
            RMSE_M4 = sqrt(mean(e2_m4)),
            .groups = "drop")
print(errores_rango, width = Inf)


g_error2 <- errores_rango |>
  select(rango, RMSE_M3, RMSE_M4) |>
  pivot_longer(-rango, names_to = "modelo", values_to = "rmse") |>
  mutate(modelo = dplyr::recode(modelo, RMSE_M3 = "M3 lin-lin", RMSE_M4 = "M4 log-lin")) |>
  ggplot(aes(rango, rmse, fill = modelo)) +
  geom_col(position = "dodge") +
  scale_y_log10(labels = scales::label_dollar()) +
  scale_fill_manual(values = c("grey60", color_base)) +
  labs(title = "Error en USD por rango de quilates (eje en log)",
       x = "Quilates", y = "RMSE (USD)", fill = NULL)
print(g_error2)
guardar_grafica(g_error2, "g15_errores_cuadrado_por_rango.png")

# 3.6 Datos raros: residuo estudentizado > 3, hat > 3p/n, Cook > 4/n
n_obs <- nobs(m4)
p_par <- length(coef(m4))
limpiar <- function(x) if_else(is.finite(x), x, NA_real_)
rstud  <- limpiar(rstudent(m4))
cook_4 <- limpiar(cooks.distance(m4))


tabla_raros <- tibble(
  criterio = c("Residuo estudentizado > 3", "Apalancamiento > 3p/n",
               "Distancia de Cook > 4/n"),
  diamantes = c(sum(abs(rstud) > 3, na.rm = TRUE),
                sum(limpiar(hatvalues(m4)) > 3 * p_par / n_obs, na.rm = TRUE),
                sum(cook_4 > 4 / n_obs, na.rm = TRUE))
) |> mutate(porcentaje = round(100 * diamantes / n_obs, 2))
print(tabla_raros)


top_raros <- diamantes |>
  mutate(rstud = rstud, precio_ajustado_M4 = res_m4$precio_ajustado) |>
  arrange(desc(abs(rstud))) |>
  slice_head(n = 10) |>
  select(precio, precio_ajustado_M4, quilates, color, claridad, forma, rstud)
print(top_raros, width = Inf)


# M4 sin los influyentes (no se borran de la base, solo se compara)
quedan <- which(!(cook_4 > 4 / n_obs) | is.na(cook_4))
m4_sin <- lm(f_m4, data = droplevels(diamantes[quedan, ]))
sensibilidad <- tibble(
  version = c("M4 con todos los datos", "M4 sin influyentes"),
  n = c(n_obs, nobs(m4_sin)),
  coef_quilates = c(b1_m4, coef(m4_sin)[["quilates"]]),
  efecto_pct = 100 * (exp(coef_quilates) - 1),
  r2 = c(summary(m4)$r.squared, summary(m4_sin)$r.squared)
)
rm(m4_sin, quedan, rstud, cook_4)
invisible(gc())
print(sensibilidad)

# 3.7 Extensión: ln(quilates) en vez de quilates
f_m5 <- reformulate(c("ln_quilates", controles), response = "ln_precio")
m5 <- lm(f_m5, data = diamantes)
elasticidad <- coef(m5)[["ln_quilates"]]
r2_m5 <- summary(m5)$r.squared
asim_m5 <- asimetria(resid(m5))
curt_m5 <- curtosis(resid(m5))
rm(m5)
invisible(gc())


t5 <- lm(f_m5, data = train)
pred_t5 <- mean(exp(resid(t5))) * exp(predict(t5, newdata = test))
rm(t5)
invisible(gc())


cat(sprintf("M5 log-log: elasticidad = %.3f | R2 = %.3f\n", elasticidad, r2_m5))


extension <- tibble(
  modelo = c("M4 múltiple log-lin", "M5 múltiple log-log"),
  r2 = c(summary(m4)$r.squared, r2_m5),
  asimetria_res = c(asimetria(res_m4$residuo), asim_m5),
  curtosis_res  = c(curtosis(res_m4$residuo), curt_m5),
  error_pct_mediano = c(pct_mediano(pred_t4), pct_mediano(pred_t5))
)
print(extension, width = Inf)

                       
# 4. CIFRAS PARA LAS CONCLUSIONES
cat(sprintf("Efecto de 0,1 quilates en M4: +%.1f%%\n", 100 * (exp(0.1 * b1_m4) - 1)))
cat(sprintf("R2: M2 = %.3f | M4 = %.3f | M5 = %.3f\n",
            summary(m2)$r.squared, summary(m4)$r.squared, r2_m5))
cat(sprintf("GVIF ajustado máximo = %.2f\n", max(tabla_vif$GVIF_ajustado)))
cat(sprintf("Efecto de 1 quilate: %.1f%% con todos | %.1f%% sin influyentes\n",
            sensibilidad$efecto_pct[1], sensibilidad$efecto_pct[2]))
cat(sprintf("Elasticidad M5: %.2f (10%% más de quilates -> +%.1f%%)\n",
            elasticidad, 100 * (1.1^elasticidad - 1)))

                       
# 5. TABLAS A WORD
a_word(inferencia_m4, "M4: inferencia sobre la pendiente de quilates",
       "tablas/tabla_m4_inferencia.docx", fuente)
a_word(efectos_m4, "M4: efecto de color, claridad, corte y laboratorio (%)",
       "tablas/tabla_m4_efectos.docx", fuente)
a_word(tabla_comparativa, "Comparación de los cuatro modelos",
       "tablas/tabla_comparativa_4modelos.docx", fuente)
a_word(tabla_normalidad, "Asimetría y curtosis de los residuos (M1 a M4)",
       "tablas/tabla_normalidad_4modelos.docx", fuente)
a_word(tibble(prueba = "RESET", estadistico = unname(reset_m4$statistic),
              valor_p = reset_m4$p.value),
       "M4: prueba de forma funcional", "tablas/tabla_m4_reset.docx", fuente)
a_word(bp, "Breusch-Pagan (M1 a M4)", "tablas/tabla_bp_4modelos.docx", fuente)
a_word(tabla_robusta, "Quilates con errores robustos HC1 (M1 a M4)",
       "tablas/tabla_robusta_4modelos.docx", fuente)
a_word(tabla_vif, "M4: VIF", "tablas/tabla_m4_vif.docx", fuente)
a_word(errores_rango, "Error en USD por rango de quilates (M3 y M4)",
       "tablas/tabla_errores_por_rango.docx", fuente)
a_word(tabla_raros, "M4: datos raros", "tablas/tabla_m4_datos_raros.docx", fuente)
a_word(top_raros, "M4: los 10 diamantes con mayor residuo",
       "tablas/tabla_m4_top_raros.docx", fuente)
a_word(sensibilidad, "M4 con y sin datos influyentes",
       "tablas/tabla_m4_sensibilidad.docx", fuente)
a_word(extension, "M4 log-lineal vs. M5 log-log",
       "tablas/tabla_extension_loglog.docx", fuente)
