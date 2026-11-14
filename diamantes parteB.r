# =====================================================================
# PROYECTO FINAL - DETERMINANTES DEL PRECIO DE LOS DIAMANTES
# Integrantes: Valerie Beltrán, Juanita Heredia
# PARTE B: modelo 4, comparación de modelos, robustez y conclusiones
#     M4: múltiple log-lineal   ln_precio ~ quilates + controles
# Base: diamantes_limpio.rds (sale de la Parte A)
# Ecuación: Y = C + Controles + Q
# =====================================================================

# Pregunta: ¿cuánto aumenta el precio de un diamante por cada quilate
# adicional, teniendo en cuenta su color, claridad y corte?


# PAQUETES Y OPCIONES
paquetes <- c("tidyverse", "ggthemes", "jtools", "broom", "flextable",
              "officer", "huxtable", "car", "lmtest", "sandwich")
faltan <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltan) > 0) install.packages(faltan)

library(tidyverse)
library(ggthemes)
library(jtools)
library(broom)
library(flextable)

options(scipen = 999)
theme_set(theme_tufte(base_size = 12))
color_base <- "#e10f83"
dir.create("graficas", showWarnings = FALSE)
dir.create("tablas", showWarnings = FALSE)

guardar_grafica <- function(p, nombre, ancho = 9, alto = 5) {
  p <- p +
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      text       = element_text(color = "black"),
      axis.text  = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
  ggsave(file.path("graficas", nombre), plot = p, width = ancho,
         height = alto, dpi = 300, bg = "white")
}

a_word <- function(df, titulo, ruta, fuente = "Elaboración propia.") {
  flextable(df) |>
    flextable::set_caption(titulo) |>
    colformat_double(digits = 3) |>
    autofit() |>
    theme_vanilla() |>
    add_footer_lines(fuente) |>
    save_as_docx(path = ruta)
}

asimetria <- function(x) {
  s <- sqrt(mean((x - mean(x))^2))
  mean((x - mean(x))^3) / s^3
}
curtosis <- function(x) {
  s <- sqrt(mean((x - mean(x))^2))
  mean((x - mean(x))^4) / s^4
}

# guarda solo lo necesario de cada modelo para no llenar la memoria
resumen_modelo <- function(m) {
  s <- summary(m)
  list(coefs = tidy(m, conf.int = TRUE),
       r2 = s$r.squared,
       r2_aj = s$adj.r.squared,
       n = nobs(m))
}

fuente <- "Fuente: base diamonds (Kaggle), 219.703 diamantes. Elaboración propia."


# 1. BASE
diamantes <- readRDS("diamantes_limpio.rds")
stopifnot(nrow(diamantes) == 219703, !anyNA(diamantes))


# 2. FÓRMULAS (las mismas de la Parte A)
controles <- c("profundidad", "tabla", "profundidad_faltante", "tabla_faltante",
               "color", "claridad", "calidad_corte", "forma", "laboratorio",
               "simetria", "pulido", "ojo_limpio", "fluor_intensidad",
               "culet_tamano", "culet_condicion", "cintura_max")

f_m1 <- precio ~ quilates
f_m2 <- ln_precio ~ quilates
f_m3 <- reformulate(c("quilates", controles), response = "precio")
f_m4 <- reformulate(c("quilates", controles), response = "ln_precio")
formulas    <- list(f_m1, f_m2, f_m3, f_m4)
nombres_mod <- c("M1 simple lin-lin", "M2 simple log-lin",
                 "M3 múltiple lin-lin", "M4 múltiple log-lin")


# 3. MODELOS 1 A 3 (para la comparación)
m1 <- lm(f_m1, data = diamantes)
r_m1 <- resumen_modelo(m1)
rm(m1)

m2 <- lm(f_m2, data = diamantes)
r_m2 <- resumen_modelo(m2)
rm(m2)

m3 <- lm(f_m3, data = diamantes)
r_m3 <- resumen_modelo(m3)
ajustado_m3 <- as.numeric(fitted(m3))
rm(m3)
invisible(gc())


# 4. MODELO 4: MÚLTIPLE LOG-LINEAL
m4 <- lm(f_m4, data = diamantes)
print(summary(m4))
r_m4 <- resumen_modelo(m4)
cat("Coeficientes NA en M4:", sum(is.na(coef(m4))), "\n")

# 4.1 Inferencia sobre quilates. H0: b1 = 0 vs H1: b1 != 0
inferencia_m4 <- r_m4$coefs |>
  filter(term == "quilates") |>
  transmute(modelo = "M4 múltiple log-lin", coeficiente = estimate,
            error_est = std.error, t = statistic, valor_p = p.value,
            ic95_inf = conf.low, ic95_sup = conf.high)
print(inferencia_m4, width = Inf)

# Prueba F global
f_global <- summary(m4)$fstatistic
cat(sprintf("F global M4: F = %.1f, valor p = %.3g\n", f_global[1],
            pf(f_global[1], f_global[2], f_global[3], lower.tail = FALSE)))

# 4.2 Interpretación
b1_m4 <- coef(m4)[["quilates"]]
cat(sprintf("M4: un quilate más -> +%.1f%% en el precio (exacto, exp(b1)-1).\n",
            100 * (exp(b1_m4) - 1)))
cat(sprintf("M4: 0,1 quilates más -> +%.1f%% en el precio.\n",
            100 * (exp(0.1 * b1_m4) - 1)))

# 4.3 Efecto de color, claridad, corte y laboratorio en %
efectos_m4 <- r_m4$coefs |>
  filter(str_detect(term, "^(color|claridad|calidad_corte|laboratorio)")) |>
  transmute(termino = term, coeficiente = estimate, valor_p = p.value,
            efecto_pct = 100 * (exp(estimate) - 1),
            ic95_inf_pct = 100 * (exp(conf.low) - 1),
            ic95_sup_pct = 100 * (exp(conf.high) - 1))
print(efectos_m4, n = Inf)


# 5. COMPARACIÓN DE LOS CUATRO MODELOS
lista_mod <- list(r_m1, r_m2, r_m3, r_m4)

# 5.1 Tabla de regresión de los cuatro modelos
estrellas <- function(p) {
  case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ "")
}
columna <- function(r, nombre) {
  r$coefs |>
    transmute(term,
              !!nombre := sprintf("%.4f%s (%.4f)", estimate,
                                  estrellas(p.value), std.error))
}
tabla_regresion <- map2(lista_mod, nombres_mod, columna) |>
  reduce(full_join, by = "term") |>
  mutate(across(-term, ~ replace_na(.x, "")))

estadisticos <- tibble(term = c("N", "R2", "R2 ajustado"))
for (i in seq_along(lista_mod)) {
  estadisticos[[nombres_mod[i]]] <- c(
    format(lista_mod[[i]]$n, big.mark = ".", decimal.mark = ","),
    sprintf("%.4f", lista_mod[[i]]$r2),
    sprintf("%.4f", lista_mod[[i]]$r2_aj))
}
tabla_regresion <- bind_rows(tabla_regresion, estadisticos) |>
  rename(variable = term)

omitir <- "^(forma|laboratorio|simetria|pulido|ojo_limpio|fluor_intensidad|culet_tamano|culet_condicion|cintura_max)"
tabla_regresion_compacta <- tabla_regresion |>
  filter(!str_detect(variable, omitir))
print(tabla_regresion_compacta, n = Inf, width = Inf)

# 5.2 Errores en USD con la misma partición 80/20 de la Parte A
set.seed(123)
idx   <- sample(nrow(diamantes), size = round(0.8 * nrow(diamantes)))
train <- droplevels(diamantes[idx, ])
test  <- diamantes[-idx, ]
for (v in names(which(sapply(train, is.factor)))) {
  test <- test[test[[v]] %in% levels(train[[v]]), ]
}

# en los modelos log se vuelve a USD con smearing
pred_usd <- function(m, nuevos, log = FALSE) {
  p <- as.numeric(predict(m, newdata = nuevos))
  if (log) p <- mean(exp(resid(m))) * exp(p)
  p
}

metricas <- function(real, pred) {
  err <- real - pred
  tibble(MAE = mean(abs(err)),
         RMSE = sqrt(mean(err^2)),
         error_abs_mediano = median(abs(err)),
         error_pct_mediano = 100 * median(abs(err) / real))
}

pred_test <- list()
for (i in 1:4) {
  m_train <- lm(formulas[[i]], data = train)
  pred_test[[i]] <- pred_usd(m_train, test, log = i %in% c(2, 4))
  rm(m_train)
  invisible(gc())
}

coef_q <- map_dbl(lista_mod, ~ .x$coefs$estimate[.x$coefs$term == "quilates"])

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
  r2          = map_dbl(lista_mod, "r2"),
  r2_ajustado = map_dbl(lista_mod, "r2_aj")
) |>
  bind_cols(map_dfr(pred_test, ~ metricas(test$precio, .x)))
print(tabla_comparativa, width = Inf)
# el R2 de M2 y M4 está en ln(precio), por eso se comparan los errores en USD


# 6. ROBUSTEZ DEL MODELO 4
res_m4 <- tibble(
  quilates  = diamantes$quilates,
  precio    = diamantes$precio,
  ln_precio = diamantes$ln_precio,
  ajustado  = as.numeric(fitted(m4)),
  residuo   = as.numeric(resid(m4))
)
smearing_m4 <- mean(exp(res_m4$residuo))
res_m4 <- res_m4 |>
  mutate(precio_ajustado = smearing_m4 * exp(ajustado),
         error2_usd = (precio - precio_ajustado)^2,
         residuo2   = residuo^2)

tabla_normalidad_m4 <- tibble(modelo = "M4 múltiple log-lineal",
                              asimetria = asimetria(res_m4$residuo),
                              curtosis  = curtosis(res_m4$residuo))
print(tabla_normalidad_m4)

# muestra de 20.000 puntos para las gráficas
set.seed(123)
muestra <- slice_sample(res_m4, n = 20000)

# 6.1 Linealidad
g_lin_quilates <- ggplot(muestra, aes(quilates, residuo)) +
  geom_point(size = 0.4, alpha = 0.25, color = color_base) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "M4: residuos frente a quilates", x = "Quilates",
       y = "Residuo (escala ln)")
print(g_lin_quilates)
guardar_grafica(g_lin_quilates, "g11_m4_linealidad_quilates.png")

g_lin_ajustados <- ggplot(muestra, aes(ajustado, residuo)) +
  geom_point(size = 0.4, alpha = 0.25, color = color_base) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "M4: residuos frente a valores ajustados",
       x = "ln(precio) ajustado", y = "Residuo")
print(g_lin_ajustados)
guardar_grafica(g_lin_ajustados, "g12_m4_residuos_ajustados.png")

# RESET de Ramsey. H0: la forma funcional es correcta
reset_m4 <- lmtest::resettest(m4, power = 2:3, type = "fitted")
print(reset_m4)

# quilates al cuadrado
m4_cuad <- update(m4, . ~ . + I(quilates^2))
cuadratico <- tidy(m4_cuad) |> filter(term %in% c("quilates", "I(quilates^2)"))
r2_cuad <- summary(m4_cuad)$r.squared
rm(m4_cuad)
invisible(gc())
print(cuadratico)
cat(sprintf("R2 M4 = %.3f | R2 con quilates al cuadrado = %.3f\n",
            r_m4$r2, r2_cuad))

# 6.2 Varianza constante
# Breusch-Pagan. H0: varianza de los errores constante
bp_m4 <- lmtest::bptest(m4)
print(bp_m4)

g_residuo2 <- ggplot(muestra, aes(ajustado, residuo2)) +
  geom_point(size = 0.4, alpha = 0.25, color = color_base) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "firebrick") +
  labs(title = "M4: residuos al cuadrado frente a ajustados",
       x = "ln(precio) ajustado", y = "Residuo al cuadrado")
print(g_residuo2)
guardar_grafica(g_residuo2, "g13_m4_residuos_cuadrado.png")

# errores estándar robustos (HC1)
ct_rob <- lmtest::coeftest(m4, vcov. = sandwich::vcovHC(m4, type = "HC1"))
robusto_m4 <- tibble(modelo = "M4",
                     coeficiente     = ct_rob["quilates", "Estimate"],
                     ee_clasico      = inferencia_m4$error_est,
                     ee_robusto      = ct_rob["quilates", "Std. Error"],
                     valor_p_robusto = ct_rob["quilates", "Pr(>|t|)"])
rm(ct_rob)
invisible(gc())
print(robusto_m4)

# 6.3 Multicolinealidad (VIF). Con factores se usa GVIF^(1/(2*Df)) y se
# compara con sqrt(5) = 2,24. M3 y M4 tienen el mismo VIF.
vif_m4 <- car::vif(m4)
tabla_vif <- as_tibble(vif_m4, rownames = "variable") |>
  rename(gl = Df, GVIF_ajustado = `GVIF^(1/(2*Df))`) |>
  mutate(supera_umbral = if_else(GVIF_ajustado > sqrt(5), "Sí", "No")) |>
  arrange(desc(GVIF_ajustado))
print(tabla_vif, n = Inf)

# 6.4 Ajustados contra reales
g_aj_real_ln <- ggplot(muestra, aes(ajustado, ln_precio)) +
  geom_point(size = 0.4, alpha = 0.25, color = color_base) +
  geom_abline(intercept = 0, slope = 1, color = "firebrick") +
  labs(title = "M4: ln(precio) real frente a ajustado",
       x = "ln(precio) ajustado", y = "ln(precio) real")
print(g_aj_real_ln)
guardar_grafica(g_aj_real_ln, "g14_m4_ajustado_vs_real_ln.png")

g_aj_real_usd <- ggplot(muestra, aes(precio_ajustado, precio)) +
  geom_point(size = 0.4, alpha = 0.25, color = color_base) +
  geom_abline(intercept = 0, slope = 1, color = "firebrick") +
  scale_x_log10(labels = scales::label_dollar()) +
  scale_y_log10(labels = scales::label_dollar()) +
  labs(title = "M4: precio real frente a ajustado (USD, ejes en log)",
       x = "Precio ajustado (USD)", y = "Precio real (USD)")
print(g_aj_real_usd)
guardar_grafica(g_aj_real_usd, "g15_m4_ajustado_vs_real_usd.png")

# 6.5 Errores al cuadrado por rango de quilates (M3 vs M4, en USD)
errores_rango <- tibble(
  quilates = diamantes$quilates,
  precio   = diamantes$precio,
  e2_m3    = (diamantes$precio - ajustado_m3)^2,
  e2_m4    = res_m4$error2_usd
) |>
  mutate(rango = cut(quilates, breaks = c(0, 0.5, 1, 2, 3, 5, Inf),
                     labels = c("hasta 0,5", "0,5 a 1", "1 a 2", "2 a 3",
                                "3 a 5", "más de 5"))) |>
  group_by(rango) |>
  summarize(diamantes = n(),
            porcentaje = round(100 * n() / nrow(diamantes), 1),
            precio_mediano = median(precio),
            RMSE_M3 = sqrt(mean(e2_m3)),
            RMSE_M4 = sqrt(mean(e2_m4)),
            .groups = "drop")
print(errores_rango, width = Inf)

g_error2 <- errores_rango |>
  select(rango, RMSE_M3, RMSE_M4) |>
  pivot_longer(-rango, names_to = "modelo", values_to = "rmse") |>
  mutate(modelo = recode(modelo, RMSE_M3 = "M3 lin-lin", RMSE_M4 = "M4 log-lin")) |>
  ggplot(aes(rango, rmse, fill = modelo)) +
  geom_col(position = "dodge") +
  scale_y_log10(labels = scales::label_dollar()) +
  scale_fill_manual(values = c("grey60", color_base)) +
  labs(title = "Error en USD por rango de quilates (eje en log)",
       x = "Quilates", y = "RMSE (USD)", fill = NULL)
print(g_error2)
guardar_grafica(g_error2, "g16_errores_cuadrado_por_rango.png")

# 6.6 Datos raros: residuo estudentizado > 3, hat > 3p/n, Cook > 4/n
n_obs <- nobs(m4)
p_par <- length(coef(m4))
limpiar <- function(x) if_else(is.finite(x), x, NA_real_)
rstud <- limpiar(rstudent(m4))
hat   <- limpiar(hatvalues(m4))
cook  <- limpiar(cooks.distance(m4))

tabla_raros <- tibble(
  criterio = c("Residuo estudentizado > 3", "Apalancamiento > 3p/n",
               "Distancia de Cook > 4/n"),
  diamantes = c(sum(abs(rstud) > 3, na.rm = TRUE),
                sum(hat > 3 * p_par / n_obs, na.rm = TRUE),
                sum(cook > 4 / n_obs, na.rm = TRUE))
) |> mutate(porcentaje = round(100 * diamantes / n_obs, 2))
print(tabla_raros)

top_raros <- diamantes |>
  mutate(rstud = rstud, precio_ajustado_M4 = res_m4$precio_ajustado) |>
  arrange(desc(abs(rstud))) |>
  slice_head(n = 10) |>
  select(precio, precio_ajustado_M4, quilates, color, claridad, forma, rstud)
print(top_raros, width = Inf)

# M4 sin los influyentes (no se borran de la base, solo se compara)
quedan <- which(!(cook > 4 / n_obs) | is.na(cook))
rm(rstud, hat)
m4_sin <- lm(f_m4, data = droplevels(diamantes[quedan, ]))
sensibilidad <- tibble(
  version = c("M4 con todos los datos", "M4 sin influyentes"),
  n = c(n_obs, nobs(m4_sin)),
  coef_quilates = c(b1_m4, coef(m4_sin)[["quilates"]]),
  efecto_pct = 100 * (exp(coef_quilates) - 1),
  r2 = c(r_m4$r2, summary(m4_sin)$r.squared)
)
rm(m4_sin, quedan, cook)
invisible(gc())
print(sensibilidad)

# ya no se necesita M4 completo
rm(m4)
invisible(gc())

# 6.7 Extensión: ln(quilates) en vez de quilates
f_m5 <- reformulate(c("ln_quilates", controles), response = "ln_precio")
m5 <- lm(f_m5, data = diamantes)
elasticidad <- coef(m5)[["ln_quilates"]]
r2_m5 <- summary(m5)$r.squared
asim_m5 <- asimetria(resid(m5))
curt_m5 <- curtosis(resid(m5))
rm(m5)
invisible(gc())

m5_train <- lm(f_m5, data = train)
pred_m5 <- pred_usd(m5_train, test, log = TRUE)
rm(m5_train)
invisible(gc())

cat(sprintf("M5 log-log: elasticidad = %.3f | R2 = %.3f\n", elasticidad, r2_m5))

extension <- bind_rows(
  tibble(modelo = "M4 múltiple log-lin", r2 = r_m4$r2,
         asimetria_res = tabla_normalidad_m4$asimetria,
         curtosis_res = tabla_normalidad_m4$curtosis) |>
    bind_cols(metricas(test$precio, pred_test[[4]])),
  tibble(modelo = "M5 múltiple log-log", r2 = r2_m5,
         asimetria_res = asim_m5, curtosis_res = curt_m5) |>
    bind_cols(metricas(test$precio, pred_m5))
)
print(extension, width = Inf)


# 7. CIFRAS PARA LAS CONCLUSIONES
cat(sprintf("Efecto de 0,1 quilates en M4: +%.1f%%\n", 100 * (exp(0.1 * b1_m4) - 1)))
cat(sprintf("R2: M2 = %.3f | M4 = %.3f | M5 = %.3f\n", r_m2$r2, r_m4$r2, r2_m5))
cat(sprintf("Breusch-Pagan p = %.3g | valor p robusto quilates = %.3g\n",
            bp_m4$p.value, robusto_m4$valor_p_robusto))
cat(sprintf("GVIF ajustado máximo = %.2f\n", max(tabla_vif$GVIF_ajustado)))
cat(sprintf("Efecto de 1 quilate: %.1f%% con todos | %.1f%% sin influyentes\n",
            sensibilidad$efecto_pct[1], sensibilidad$efecto_pct[2]))
cat(sprintf("Elasticidad M5: %.2f (10%% más de quilates -> +%.1f%%)\n",
            elasticidad, 100 * (1.1^elasticidad - 1)))


# 8. TABLAS A WORD
nota_est <- paste(fuente, "Error estándar entre paréntesis. *** p<0,01; ** p<0,05; * p<0,1.")
a_word(tabla_regresion, "Regresión de los cuatro modelos (completa)",
       "tablas/tabla_regresion_4modelos_completa.docx", nota_est)
a_word(tabla_regresion_compacta, "Regresión de los cuatro modelos",
       "tablas/tabla_regresion_4modelos_compacta.docx",
       paste(nota_est, "Forma, laboratorio, simetría, pulido, ojo limpio, fluorescencia, culet y cintura están en M3 y M4 pero no se muestran."))
a_word(inferencia_m4, "M4: inferencia sobre la pendiente de quilates",
       "tablas/tabla_m4_inferencia.docx", fuente)
a_word(efectos_m4, "M4: efecto de color, claridad, corte y laboratorio (%)",
       "tablas/tabla_m4_efectos.docx", fuente)
a_word(tabla_comparativa, "Comparación de los cuatro modelos",
       "tablas/tabla_comparativa_4modelos.docx", fuente)
a_word(tabla_normalidad_m4, "M4: asimetría y curtosis de los residuos",
       "tablas/tabla_m4_normalidad.docx", fuente)
a_word(tibble(prueba = c("RESET", "Breusch-Pagan"),
              estadistico = c(unname(reset_m4$statistic), unname(bp_m4$statistic)),
              valor_p = c(reset_m4$p.value, bp_m4$p.value)),
       "M4: pruebas de linealidad y varianza constante",
       "tablas/tabla_m4_pruebas.docx", fuente)
a_word(robusto_m4, "M4: quilates con errores robustos (HC1)",
       "tablas/tabla_m4_robusta.docx", fuente)
a_word(tabla_vif, "M4: VIF",
       "tablas/tabla_m4_vif.docx", fuente)
a_word(errores_rango, "Error en USD por rango de quilates (M3 y M4)",
       "tablas/tabla_errores_por_rango.docx", fuente)
a_word(tabla_raros, "M4: datos raros",
       "tablas/tabla_m4_datos_raros.docx", fuente)
a_word(top_raros, "M4: los 10 diamantes con mayor residuo",
       "tablas/tabla_m4_top_raros.docx", fuente)
a_word(sensibilidad, "M4 con y sin datos influyentes",
       "tablas/tabla_m4_sensibilidad.docx", fuente)
a_word(extension, "M4 log-lineal vs. M5 log-log",
       "tablas/tabla_extension_loglog.docx", fuente)

cat("Listo: Parte B terminada.\n")
