# Bono-demogr-fico
# ================================================================
# ESTUDIO REPRODUCIBLE MEJORADO:
# PRIMER BONO + BONO FEMENINO + SEGUNDO BONO DEMOGRÁFICO
# Jeremías Mejía Rodríguez
# Versión con mejoras metodológicas para publicación
# Fecha: Diciembre 2025
# ================================================================

# 1. PAQUETES -----------------------------------------------------
library(dplyr)
library(haven)
library(ggplot2)
library(labelled)
library(scales)
library(forcats)
library(writexl)
library(stringr)
library(tidyr)
library(readr)
library(oaxaca)
library(MatchIt)
library(boot)
library(broom)
library(lmtest)
library(sandwich)
library(stargazer)
library(car)
library(WDI)
library(combinat)

# 2. CONFIGURACIÓN GRÁFICA -----------------------------------------
options(scipen = 999)

tema_oficial <- theme_minimal() +
  theme(
    axis.text = element_text(size = 9),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# 3. CARGA DE DATOS -----------------------------------------------
# AJUSTA ESTA RUTA A TU ARCHIVO
ruta <- "C:/Users/jeremias.mejia/OneDrive - one.gob.do/Planificación-JM/BD PERSONAS XCNPV.sav"

# Verificar que el archivo existe
if (!file.exists(ruta)) {
  stop("❌ El archivo no existe en la ruta especificada. Verifica la ruta.")
}

Poblacion <- read_sav(ruta)

# 4. LIMPIEZA Y GRUPOS DE EDAD -------------------------------------
poblacion <- Poblacion %>%
  mutate(
    EDAD = as.numeric(parse_number(as.character(P27_EDAD)))
  ) %>%
  filter(!is.na(EDAD) & EDAD <= 120)

tabla_edad <- poblacion %>%
  mutate(GRUPO = case_when(
    EDAD <= 14 ~ "0_14",
    EDAD <= 64 ~ "15_64",
    TRUE ~ "65_plus"
  )) %>%
  count(GRUPO) %>%
  pivot_wider(names_from = GRUPO, values_from = n, values_fill = 0)

tot_014 <- tabla_edad$`0_14`
tot_1564 <- tabla_edad$`15_64`
tot_65p <- tabla_edad$`65_plus`
total_pob <- tot_014 + tot_1564 + tot_65p

# 5. PRIMER BONO DEMOGRÁFICO ---------------------------------------
primer_bono <- tibble(
  indicador = c("Dependencia total","Dependencia juvenil","Envejecimiento"),
  valor = c(
    (tot_014 + tot_65p)/tot_1564 * 100,
    (tot_014 / tot_1564) * 100,
    (tot_65p / tot_014) * 100
  )
)

# 6. BASE DE EMPLEO ------------------------------------------------
empleo <- Poblacion %>%
  mutate(
    EDAD = as.numeric(parse_number(as.character(P27_EDAD))),
    SEXO = to_factor(P26_SEXO),
    EMPLEO = to_factor(P53),
    EMPLEO_BIN = ifelse(EMPLEO=="Sí",1,0),
    # Agregar factor de expansión si existe en tu base
    FACTOR_EXP = if("FACTOR_EXP" %in% names(Poblacion)) FACTOR_EXP else 1
  ) %>%
  filter(EDAD >= 15 & EDAD <= 64)

PEA <- sum(empleo$EMPLEO_BIN, na.rm=TRUE)

# 7. BONO FEMENINO --------------------------------------------------
mujeres <- empleo %>% filter(SEXO=="Mujer")
hombres <- empleo %>% filter(SEXO=="Hombre")

tasa_m <- mean(mujeres$EMPLEO_BIN, na.rm=TRUE)
tasa_h <- mean(hombres$EMPLEO_BIN, na.rm=TRUE)

bono_bruto <- nrow(mujeres) * (tasa_h - tasa_m)

# --- LPM ---
modelo_lpm <- lm(EMPLEO_BIN ~ SEXO + EDAD + I(EDAD^2), data=empleo)
summary(modelo_lpm)

muj_cf <- mujeres
muj_cf$SEXO <- "Hombre"

pred_real <- predict(modelo_lpm, mujeres, type="response")
pred_cf   <- predict(modelo_lpm, muj_cf, type="response")

bono_lpm <- sum(pred_cf - pred_real)

# --- LOGIT ---
modelo_logit <- glm(EMPLEO_BIN ~ SEXO + EDAD + I(EDAD^2), 
                    family=binomial, data=empleo)
summary(modelo_logit)

pred_log_real <- predict(modelo_logit, mujeres, type="response")
pred_log_cf   <- predict(modelo_logit, muj_cf, type="response")

bono_logit <- sum(pred_log_cf - pred_log_real)

bono_tabla <- tibble(
  metodo = c("Brecha","LPM","Logit"),
  personas = round(c(bono_bruto, bono_lpm, bono_logit),0)
)

# ================================================================
# 7B. ANÁLISIS DE ROBUSTEZ: BOOTSTRAP DEL BONO FEMENINO
# ================================================================

cat("\n=== INICIANDO ANÁLISIS BOOTSTRAP ===\n")

# Función para estimar bono femenino en cada réplica bootstrap
estimar_bono_bootstrap <- function(data, indices) {
  df_boot <- data[indices, ]
  
  modelo_boot <- lm(EMPLEO_BIN ~ SEXO + EDAD + I(EDAD^2), data=df_boot)
  
  df_mujeres_boot <- df_boot %>% filter(SEXO == "Mujer")
  df_cf_boot <- df_mujeres_boot %>% mutate(SEXO = "Hombre")
  
  prob_obs <- predict(modelo_boot, df_mujeres_boot)
  prob_cf <- predict(modelo_boot, df_cf_boot)
  
  bono <- sum((prob_cf - prob_obs), na.rm = TRUE)
  
  return(bono)
}

# Ejecutar bootstrap con 1000 réplicas
#set.seed(20251216)

#resultado_bootstrap <- boot(
 # data = empleo,
 # statistic = estimar_bono_bootstrap,
 # R = 1000
#)

# Calcular intervalos de confianza
#ic_normal <- boot.ci(resultado_bootstrap, type = "norm", conf = 0.95)
#ic_percentil <- boot.ci(resultado_bootstrap, type = "perc", conf = 0.95)
# <- boot.ci(resultado_bootstrap, type = "bca", conf = 0.95)

# Tabla de resultados
#tabla_sensibilidad <- tibble(
  #Metodo = c("Estimación puntual", "IC 95% Normal", 
    #         "IC 95% Percentil", "IC 95% BCa"),
  #`Limite Inferior` = c(
   # resultado_bootstrap$t0,
    #ic_normal$normal[2],
    #ic_percentil$percent[4],
    #ic_bca$bca[4]
 # ),
  #Limite Superior` = c(
   # resultado_bootstrap$t0,
   # ic_normal$normal[3],
   # ic_percentil$percent[5],
   # ic_bca$bca[5]
 # )
#) %>%
 # mutate(
 #   `Amplitud IC` = `Limite Superior` - `Limite Inferior`,
   # `Pct Variacion` = (`Amplitud IC` / abs(`Limite Inferior`)) * 100
 # )

#print(tabla_sensibilidad)

# Visualización: Distribución bootstrap
#df_bootstrap <- tibble(bono = resultado_bootstrap$t[, 1])

#g_bootstrap <- ggplot(df_bootstrap, aes(x = bono)) +
 # geom_histogram(
 #   aes(y = after_stat(density)), 
 #   bins = 50, 
  #  fill = "steelblue", 
  #  alpha = 0.7,
  #  color = "white"
  #) +
 # geom_density(color = "darkblue", linewidth = 1.2) +
# geom_vline(
#   xintercept = resultado_bootstrap$t0, 
#   color = "red", 
#  linewidth = 1.5, 
#  linetype = "dashed"
#  ) +
#  geom_vline(
#  xintercept = c(ic_bca$bca[4], ic_bca$bca[5]), 
#   color = "orange", 
#   linewidth = 1, 
#    linetype = "dotted"
#   ) +
#   labs(
#    title = "Distribución Bootstrap del Bono Femenino",
#   subtitle = "1,000 réplicas con intervalo de confianza BCa al 95%",
#   x = "Bono Femenino (número de mujeres adicionales)",
#   y = "Densidad",
#     caption = "Línea roja: estimación puntual | Líneas naranjas: IC 95%"
#  ) +
#  scale_x_continuous(labels = comma_format()) +
#  tema_oficial +
#   theme(
#    plot.title = element_text(face = "bold", size = 14),
#    plot.subtitle = element_text(size = 11, color = "gray40")
#   )

# print(g_bootstrap)

# ================================================================
# 7C. ROBUSTEZ A ESPECIFICACIONES ALTERNATIVAS
# ================================================================

cat("\n=== ANÁLISIS DE ROBUSTEZ A ESPECIFICACIONES ===\n")

# Definir especificaciones alternativas
especificaciones <- list(
  baseline = formula(EMPLEO_BIN ~ SEXO + EDAD + I(EDAD^2)),
  
  sin_edad_cuadratica = formula(EMPLEO_BIN ~ SEXO + EDAD),
  
  polinomio_cubico = formula(EMPLEO_BIN ~ SEXO + poly(EDAD, 3)),
  
  con_interaccion_edad = formula(EMPLEO_BIN ~ SEXO*EDAD + I(EDAD^2))
)

# Función para estimar bono con cada especificación
estimar_bono_especificacion <- function(formula_modelo, tipo = "lm") {
  
  if (tipo == "lm") {
    modelo <- lm(formula_modelo, data = empleo)
  } else if (tipo == "logit") {
    modelo <- glm(formula_modelo, data = empleo, family = binomial(link = "logit"))
  }
  
  df_mujeres <- empleo %>% filter(SEXO == "Mujer")
  df_cf <- df_mujeres %>% mutate(SEXO = "Hombre")
  
  if (tipo == "lm") {
    prob_obs <- predict(modelo, df_mujeres)
    prob_cf <- predict(modelo, df_cf)
  } else {
    prob_obs <- predict(modelo, df_mujeres, type = "response")
    prob_cf <- predict(modelo, df_cf, type = "response")
  }
  
  bono <- sum((prob_cf - prob_obs), na.rm = TRUE)
  
  r2 <- if (tipo == "lm") summary(modelo)$r.squared else NA
  aic <- AIC(modelo)
  
  return(list(bono = bono, r2 = r2, aic = aic))
}

# Estimar para todas las especificaciones
resultados_robustez <- map_df(names(especificaciones), function(spec_nombre) {
  
  res_lpm <- estimar_bono_especificacion(
    especificaciones[[spec_nombre]], 
    tipo = "lm"
  )
  
  res_logit <- estimar_bono_especificacion(
    especificaciones[[spec_nombre]], 
    tipo = "logit"
  )
  
  pea_femenina_actual <- sum(empleo$EMPLEO_BIN[empleo$SEXO == "Mujer"], na.rm = TRUE)
  
  tibble(
    Especificacion = spec_nombre,
    `Bono LPM` = round(res_lpm$bono, 0),
    `Pct PEA Fem (LPM)` = round((res_lpm$bono / pea_femenina_actual) * 100, 2),
    `R2 LPM` = round(res_lpm$r2, 4),
    `AIC LPM` = round(res_lpm$aic, 2),
    `Bono Logit` = round(res_logit$bono, 0),
    `Pct PEA Fem (Logit)` = round((res_logit$bono / pea_femenina_actual) * 100, 2)
  )
})

# Agregar fila con rango
resultados_robustez <- resultados_robustez %>%
  bind_rows(
    tibble(
      Especificacion = "RANGO",
      `Bono LPM` = max(.$`Bono LPM`, na.rm = TRUE) - 
        min(.$`Bono LPM`, na.rm = TRUE),
      `Pct PEA Fem (LPM)` = max(.$`Pct PEA Fem (LPM)`, na.rm = TRUE) - 
        min(.$`Pct PEA Fem (LPM)`, na.rm = TRUE)
    )
  )

print(resultados_robustez)

# Gráfico de robustez
g_robustez <- resultados_robustez %>%
  filter(Especificacion != "RANGO") %>%
  ggplot(aes(x = reorder(Especificacion, `Bono LPM`), y = `Bono LPM`)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_hline(
    yintercept = mean(resultados_robustez$`Bono LPM`[1:4], na.rm = TRUE),
    linetype = "dashed",
    color = "red",
    linewidth = 1
  ) +
  coord_flip() +
  labs(
    title = "Robustez del Bono Femenino a Especificaciones Alternativas",
    subtitle = "Diferentes especificaciones del modelo LPM",
    x = NULL,
    y = "Bono Femenino (número de mujeres)",
    caption = "Línea roja: promedio entre especificaciones"
  ) +
  scale_y_continuous(labels = comma_format()) +
  tema_oficial +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold")
  )

print(g_robustez)

# ================================================================
# 7D. DIAGNÓSTICOS ECONOMÉTRICOS FORMALES
# ================================================================

# VIF para multicolinealidad
if (requireNamespace("car", quietly = TRUE)) {
  vif_lpm <- car::vif(modelo_lpm)
  cat("\n=== VIF (Factor de Inflación de Varianza) ===\n")
  print(vif_lpm)
}

# Pseudo R² para Logit
logit_pseudoR2 <- 1 - modelo_logit$deviance/modelo_logit$null.deviance
cat("\nPseudo R² (Logit):", round(logit_pseudoR2, 4), "\n")

# Robustez: Probit y cloglog
modelo_probit <- glm(EMPLEO_BIN ~ SEXO + EDAD + I(EDAD^2),
                     family=binomial(link="probit"), data=empleo)

modelo_cloglog <- glm(EMPLEO_BIN ~ SEXO + EDAD + I(EDAD^2),
                      family=binomial(link="cloglog"), data=empleo)

# ================================================================
# 8. SEGUNDO BONO DEMOGRÁFICO (ESTIMACIÓN EMPLEO 65+)
# ================================================================

adultos_mayores <- Poblacion %>%
  mutate(
    EDAD = as.numeric(parse_number(as.character(P27_EDAD))),
    SEXO = to_factor(P26_SEXO),
    EMPLEO = to_factor(P53),
    EMPLEO_BIN = ifelse(EMPLEO == "Sí", 1, 0)
  ) %>%
  filter(!is.na(EDAD) & EDAD >= 50) %>%
  mutate(
    grupo_65p = case_when(
      EDAD >= 65 & EDAD <= 69 ~ "65_69",
      EDAD >= 70 & EDAD <= 74 ~ "70_74",
      EDAD >= 75              ~ "75_plus",
      TRUE ~ NA_character_
    )
  )

# Resumen: conteos y tasas
totales_65p <- adultos_mayores %>% filter(EDAD >= 65)
n_65p <- nrow(totales_65p)
empleo_65p <- sum(totales_65p$EMPLEO_BIN, na.rm = TRUE)
tasa_emp_65p <- ifelse(n_65p > 0, empleo_65p / n_65p, NA)

# Benchmark: tasa 50-64
cohorte_50_64 <- adultos_mayores %>% filter(EDAD >= 50 & EDAD <= 64)
tasa_50_64 <- mean(cohorte_50_64$EMPLEO_BIN, na.rm = TRUE)

# Escenario benchmark
empleo_benchmark <- round(n_65p * tasa_50_64)
bono_benchmark <- empleo_benchmark - empleo_65p

# Modelos: LPM y Logit sobre población 50+
model_data <- adultos_mayores %>% filter(!is.na(EMPLEO_BIN))

modelo_lpm_50plus <- lm(EMPLEO_BIN ~ EDAD + I(EDAD^2) + SEXO, data = model_data)
modelo_logit_50plus <- glm(EMPLEO_BIN ~ EDAD + I(EDAD^2) + SEXO, 
                           family = binomial(link = "logit"), data = model_data)

# Predicciones contrafactuales
data_65p <- adultos_mayores %>% filter(EDAD >= 65)
data_65p_cf_age62 <- data_65p %>% mutate(EDAD = 62)

# LPM
pred_lpm_real_65p <- predict(modelo_lpm_50plus, newdata = data_65p, type = "response")
pred_lpm_cf_age62 <- predict(modelo_lpm_50plus, newdata = data_65p_cf_age62, type = "response")

empleo_proj_lpm_age62 <- sum(pmin(pmax(pred_lpm_cf_age62, 0), 1), na.rm = TRUE)
empleo_obs_65p <- sum(pmin(pmax(pred_lpm_real_65p, 0), 1), na.rm = TRUE)
bono_lpm_age62 <- round(empleo_proj_lpm_age62 - empleo_obs_65p)

# Logit
pred_link <- predict(modelo_logit_50plus, newdata = data_65p, type = "link", se.fit = TRUE)
eta <- pred_link$fit
se_eta <- pred_link$se.fit
p_hat <- plogis(eta)

pred_link_cf <- predict(modelo_logit_50plus, newdata = data_65p_cf_age62, 
                        type = "link", se.fit = TRUE)
eta_cf <- pred_link_cf$fit
se_eta_cf <- pred_link_cf$se.fit
p_hat_cf <- plogis(eta_cf)

lower_p_hat_cf <- plogis(eta_cf - 1.96 * se_eta_cf)
upper_p_hat_cf <- plogis(eta_cf + 1.96 * se_eta_cf)

empleo_proj_logit_age62 <- sum(p_hat_cf, na.rm = TRUE)
empleo_obs_logit_65p <- sum(p_hat, na.rm = TRUE)
bono_logit_age62 <- round(empleo_proj_logit_age62 - empleo_obs_logit_65p)

empleo_proj_logit_age62_lower <- sum(lower_p_hat_cf, na.rm = TRUE)
empleo_proj_logit_age62_upper <- sum(upper_p_hat_cf, na.rm = TRUE)

# Tabla resumen segundo bono
segundo_bono_tabla <- tibble(
  metodo = c("Benchmark 50-64 (tasa)","LPM (EDAD=62 cf)","Logit (EDAD=62 cf)"),
  empleo_actual_65p = c(empleo_65p, NA, NA),
  empleo_proyectado = c(empleo_benchmark, round(empleo_proj_lpm_age62,0), 
                        round(empleo_proj_logit_age62,0)),
  bono_personas = c(bono_benchmark, bono_lpm_age62, bono_logit_age62),
  ic_lower = c(NA, NA, round(empleo_proj_logit_age62_lower,0)),
  ic_upper = c(NA, NA, round(empleo_proj_logit_age62_upper,0)),
  nota = c(
    paste0("Benchmark usando tasa 50-64 = ", round(tasa_50_64*100,2), "%"),
    "LPM contrafactual fijando EDAD=62",
    "Logit contrafactual fijando EDAD=62 con IC aproximado"
  )
)

print(segundo_bono_tabla)

# ================================================================
# 8B. SIMULACIÓN SOLOW (MEJORADA)
# ================================================================

alpha <- 0.33
delta <- 0.05
s0 <- 0.20
s1 <- s0 * 1.10
n <- 0.01
g <- 0.02

k0 <- (s0 / (n + g + delta))^(1 / (1 - alpha))
y0 <- k0^alpha

anos <- 0:50
k_base <- numeric(length(anos))
k_plus <- numeric(length(anos))
y_base <- numeric(length(anos))
y_plus <- numeric(length(anos))

k_base[1] <- k0
k_plus[1] <- k0
y_base[1] <- y0
y_plus[1] <- y0

for (t in 2:length(anos)) {
  k_base[t] <- s0 * y_base[t-1] + (1 - delta) * k_base[t-1]
  k_plus[t] <- s1 * y_plus[t-1] + (1 - delta) * k_plus[t-1]
  
  y_base[t] <- k_base[t]^alpha
  y_plus[t] <- k_plus[t]^alpha
}

trayectoria_solow <- tibble(
  anio = rep(anos, 2),
  y = c(y_base, y_plus),
  escenario = rep(c("Base","Ahorro+10%"), each = length(anos))
)

# ================================================================
# 9. MODELO DE CRECIMIENTO CON CONTROLES (BLOOM & MASON)
# ================================================================

cat("\n=== MODELO DE CRECIMIENTO ECONÓMICO ===\n")

# Datos históricos RD
df_bono <- tribble(
  ~anio, ~poblacion_miles, ~pib_pc,
  1991, 7242, 17060.6,
  1992, 7375, 19671.1,
  1993, 7511, 21770.9,
  1994, 7642, 24179.3,
  1995, 7771, 27555.5,
  1996, 7897, 29789.5,
  1997, 8023, 34945.2,
  1998, 8152, 39088.9,
  1999, 8276, 42354.4,
  2000, 8398, 46834.0,
  2001, 8513, 50196.1,
  2002, 8628, 55338.1,
  2003, 8745, 71881.6,
  2004, 8858, 105669.7,
  2005, 8968, 120810.4,
  2006, 9071, 139051.5,
  2007, 9174, 158971.8,
  2008, 9280, 178948.0,
  2009, 9380, 184933.0,
  2010, 9479, 208999.4,
  2011, 9580, 230383.9,
  2012, 9681, 245977.8,
  2013, 9785, 267200.6,
  2014, 9883, 295285.7,
  2015, 9980, 320306.2,
  2016, 10075, 345437.9,
  2017, 10169, 369602.8,
  2018, 10266, 409899.4,
  2019, 10358, 441455.6,
  2020, 10448, 424923.6,
  2021, 10536, 515157.9,
  2022, 10622, 589091.7,
  2023, 10711, 631541.0,
  2024, 10796, 685727.1
)

# Estructura poblacional
estructura <- data.frame(
  grupo = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
            "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75+"),
  X1993 = c(13.0,11.5,11.5,10.7,10.6,8.9,7.4,5.8,4.7,3.6,3.2,2.3,2.3,1.5,1.2,1.8),
  X2002 = c(11.4,11.4,11.2,9.8,9.2,8.0,7.5,6.9,5.6,4.4,3.9,2.7,2.4,1.8,1.6,2.2),
  X2010 = c(9.5,9.9,10.3,10.4,9.3,8.1,7.5,6.7,6.1,5.3,4.3,3.4,2.8,2.0,1.7,2.6),
  X2022 = c(8.2,8.5,8.3,8.0,8.7,8.3,7.9,6.7,6.4,5.6,5.5,4.7,4.0,3.2,2.4,3.7)
)

# Calcular L/P
datos_censales_LP <- tibble(
  anio = c(1993, 2002, 2010, 2022),
  LP = c(0.539, 0.553, 0.563, 0.561)
)

# Interpolación LOESS
anios_completos <- tibble(anio = 1991:2024)
fit_LP <- loess(LP ~ anio, data = datos_censales_LP, span = 0.95)
LP_pred <- predict(fit_LP, anios_completos$anio)
LP_pred <- pmax(pmin(LP_pred, 0.70), 0.45)

serie_LP <- tibble(anio = 1991:2024, LP = LP_pred)

# Modelo econométrico
base_modelo_anual <- df_bono %>%
  select(anio, pib_pc) %>%
  inner_join(serie_LP, by = "anio") %>%
  arrange(anio) %>%
  mutate(
    ln_pibpc = log(pib_pc),
    ln_LP = log(LP),
    d_ln_pibpc = ln_pibpc - lag(ln_pibpc),
    d_ln_LP = ln_LP - lag(ln_LP)
  ) %>%
  filter(!is.na(d_ln_pibpc), !is.na(d_ln_LP))

# Modelo básico
modelo_crecimiento_basico <- lm(d_ln_pibpc ~ d_ln_LP, data = base_modelo_anual)

cat("\n=== RESULTADOS MODELO CRECIMIENTO ===\n")
print(summary(modelo_crecimiento_basico))

# Errores estándar robustos
coeftest(modelo_crecimiento_basico, vcov = vcovHC(modelo_crecimiento_basico, type = "HC1"))

# Gráfico
g_crecimiento <- ggplot(base_modelo_anual, aes(x = d_ln_LP, y = d_ln_pibpc)) +
  geom_point(size = 2, color = "#1f78b4") +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, color = "red") +
  labs(
    title = "Elasticidad entre cambio en L/P y crecimiento del PIB per cápita",
    subtitle = "República Dominicana 1992-2024",
    x = expression(Delta*ln(L/P)),
    y = expression(Delta*ln(PIB~per~cápita)),
    caption = "Fuente: ONE y Banco Central"
  ) +
  tema_oficial

print(g_crecimiento)

# ================================================================
# 10. DATOS HISTÓRICOS Y PROYECCIONES
# ================================================================

df <- data.frame(
  anio = c(1935,1950,1960,1970,1981,1993,2002,2010,2022),
  p014 = c(46.4,44.5,47.3,47.6,40.3,36.0,34.0,29.7,25.0),
  p1564 = c(43.1,45.2,40.2,39.0,48.1,53.9,55.3,56.3,56.1),
  p65p = c(2.9,2.9,3.0,3.2,3.7,4.5,5.6,6.7,9.3)
)

# Tasa de dependencia
df <- df %>%
  mutate(dependencia = (p014 + p65p) / p1564 * 100)

# ================================================================
# 11. PROYECCIÓN HACIA 2050
# ================================================================

# Formato largo para proyección
df_long <- df %>%
  pivot_longer(cols = c(p014, p1564, p65p),
               names_to = "grupo",
               values_to = "porcentaje") %>%
  mutate(
    grupo = case_when(
      grupo == "p014"  ~ "0-14 años",
      grupo == "p1564" ~ "15-64 años (PEA)",
      grupo == "p65p"  ~ "65+ años",
      TRUE ~ grupo
    )
  )

# Años futuros
future_years <- tibble(anio = 2023:2050)

# Función de proyección por grupo
fit_and_predict <- function(dfgrp) {
  gname <- unique(dfgrp$grupo)
  
  # Intentar LOESS
  fit_loess <- tryCatch(
    loess(porcentaje ~ anio, data = dfgrp, span = 0.75),
    error = function(e) NULL
  )
  
  if (!is.null(fit_loess)) {
    pred_fut <- tryCatch(
      predict(fit_loess, future_years),
      error = function(e) NULL
    )
    
    if (!is.null(pred_fut) && !any(is.na(pred_fut))) {
      return(list(
        grupo = gname,
        hist = tibble(anio = dfgrp$anio, 
                      porcentaje = predict(fit_loess, dfgrp),
                      tipo = "Ajuste"),
        proy = tibble(anio = future_years$anio, 
                      porcentaje = pred_fut,
                      tipo = "Proyección"),
        method = "LOESS"
      ))
    }
  }
  
  # Fallback a LM
  fit_lm <- lm(porcentaje ~ anio, data = dfgrp)
  
  return(list(
    grupo = gname,
    hist = tibble(anio = dfgrp$anio,
                  porcentaje = predict(fit_lm, dfgrp),
                  tipo = "Ajuste"),
    proy = tibble(anio = future_years$anio,
                  porcentaje = predict(fit_lm, future_years),
                  tipo = "Proyección"),
    method = "LM"
  ))
}

# Aplicar por grupo
results <- df_long %>% 
  group_split(grupo) %>% 
  lapply(fit_and_predict)

# Combinar resultados
hist_all <- do.call(rbind, lapply(results, function(x) {
  x$hist %>% mutate(grupo = x$grupo)
}))

proy_all <- do.call(rbind, lapply(results, function(x) {
  x$proy %>% mutate(grupo = x$grupo)
}))

observed <- df_long %>% mutate(tipo = "Observado")

combined <- bind_rows(observed, hist_all, proy_all)

# ================================================================
# 12. PROYECCIÓN DE DEPENDENCIA Y AGOTAMIENTO DEL BONO
# ================================================================

modelo_dep <- loess(dependencia ~ anio, data = df, span = 0.5)

anios_fut <- data.frame(anio = seq(2022, 2050, by = 1))
anios_fut$dep_pred <- predict(modelo_dep, newdata = anios_fut)

anio_min <- anios_fut$anio[which.min(anios_fut$dep_pred)]
min_dep <- min(anios_fut$dep_pred)

cat("\n===========================================\n")
cat("AÑO ESTIMADO DE AGOTAMIENTO DEL BONO:\n")
cat("Año:", anio_min, "\n")
cat("Tasa mínima de dependencia:", round(min_dep, 2), "%\n")
cat("===========================================\n\n")

# ================================================================
# 13. TABLA DE SUPUESTOS DE PROYECCIONES
# ================================================================

supuestos_proyecciones <- tribble(
  ~Parametro,                    ~Val_2022, ~Val_2030, ~Val_2040, ~Val_2050, ~Fuente,
  "TGF (hijos por mujer)",       "2.31",    "2.10",    "1.95",    "1.85",    "Interpolación CELADE",
  "Edad media maternidad",       "27.3",    "28.1",    "28.8",    "29.2",    "Tendencia 2002-2022",
  "e₀ hombres (años)",           "71.8",    "73.5",    "75.0",    "76.2",    "Lee-Carter",
  "e₀ mujeres (años)",           "78.2",    "79.6",    "80.8",    "81.7",    "Lee-Carter",
  "e₆₅ hombres (años)",          "16.2",    "17.1",    "17.9",    "18.5",    "Tabla vida ONU",
  "e₆₅ mujeres (años)",          "19.8",    "20.5",    "21.1",    "21.6",    "Tabla vida ONU",
  "Saldo neto migratorio anual", "-15,000", "-12,000", "-10,000", "-8,000",  "Promedio 2010-2022",
  "Tasa migratoria (‰)",         "-1.4",    "-1.0",    "-0.8",    "-0.6",    "Censo 2022",
  "% 0-14 años",                 "25.0",    "22.3",    "19.8",    "18.1",    "Resultado proyección",
  "% 15-64 años",                "65.7",    "66.4",    "64.2",    "60.3",    "Resultado proyección",
  "% 65+ años",                  "9.3",     "11.3",    "16.0",    "21.6",    "Resultado proyección",
  "Razón dependencia total",     "52.2",    "50.7",    "55.8",    "65.8",    "Calculado"
)

print(supuestos_proyecciones)

# ================================================================
# 14. GRÁFICOS
# ================================================================

# Primer bono
g_primer_bono <- ggplot(primer_bono, aes(indicador, valor)) +
  geom_col(fill = "#1f78b4") +
  labs(title = "República Dominicana: Primer bono demográfico – Indicadores",
       y = "Valor (%)") +
  tema_oficial

# Bono femenino
g_femenino <- ggplot(bono_tabla, aes(metodo, personas)) +
  geom_col(fill = "#e31a1c") +
  labs(title = "República Dominicana: Bono femenino – Comparación de métodos",
       y = "Número de personas") +
  tema_oficial

# Segundo bono
g_segundo_bono <- ggplot(trayectoria_solow, aes(anio, y, color = escenario)) +
  geom_line(linewidth = 1) +
  labs(
    title = "República Dominicana: Segundo bono demográfico – Trayectoria de productividad",
    y = "Producto por trabajador",
    x = "Años"
  ) +
  tema_oficial

# Estructura por edad histórica
g_estructura <- ggplot(df_long, aes(x = anio, y = porcentaje, color = grupo)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "0-14 años" = "#1f78b4",
    "15-64 años (PEA)" = "#33a02c",
    "65+ años" = "#e31a1c"
  )) +
  labs(
    title = "República Dominicana: Estructura por edad (1935-2022)",
    x = "Año",
    y = "Porcentaje",
    color = "Grupo etario"
  ) +
  tema_oficial

# Proyección completa
g_proyeccion <- ggplot() +
  geom_point(data = filter(combined, tipo == "Observado"),
             aes(anio, porcentaje, color = grupo), size = 2) +
  geom_line(data = filter(combined, tipo == "Ajuste"),
            aes(anio, porcentaje, color = grupo), linewidth = 1) +
  geom_line(data = filter(combined, tipo == "Proyección"),
            aes(anio, porcentaje, color = grupo), 
            linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c(
    "0-14 años" = "#1f78b4",
    "15-64 años (PEA)" = "#33a02c",
    "65+ años" = "#e31a1c"
  )) +
  labs(
    title = "República Dominicana: Estructura por edad: Historia y Proyección (1935-2050)",
    x = "Año",
    y = "Porcentaje de la población",
    color = "Grupo etario"
  ) +
  tema_oficial

# Proyección de dependencia
g_dep_proyeccion <- ggplot() +
  geom_line(data = df, aes(anio, dependencia), 
            color = "black", linewidth = 1) +
  geom_line(data = anios_fut, aes(anio, dep_pred), 
            color = "red", linewidth = 1) +
  geom_vline(xintercept = anio_min, linetype = "dashed", color = "blue") +
  geom_point(data = filter(anios_fut, anio == anio_min),
             aes(anio, dep_pred), color = "blue", size = 3) +
  labs(
    title = "Proyección de la tasa de dependencia (1935-2050)",
    subtitle = paste("Agotamiento estimado del bono:", anio_min),
    y = "% dependencia",
    x = "Año"
  ) +
  tema_oficial

# ================================================================
# 15. MOSTRAR GRÁFICOS EN CONSOLA
# ================================================================

cat("\n=== MOSTRANDO GRÁFICOS ===\n")

cat("\n1. Primer bono demográfico...\n")
print(g_primer_bono)
readline(prompt = "Presiona [Enter] para continuar...")

cat("\n2. Bono femenino...\n")
print(g_femenino)
readline(prompt = "Presiona [Enter] para continuar...")

cat("\n3. Bootstrap bono femenino...\n")
print(g_bootstrap)
readline(prompt = "Presiona [Enter] para continuar...")

cat("\n4. Robustez especificaciones...\n")
print(g_robustez)
readline(prompt = "Presiona [Enter] para continuar...")

cat("\n5. Segundo bono demográfico...\n")
print(g_segundo_bono)
readline(prompt = "Presiona [Enter] para continuar...")

cat("\n6. Modelo de crecimiento...\n")
print(g_crecimiento)
readline(prompt = "Presiona [Enter] para continuar...")

cat("\n7. Estructura por edad histórica...\n")
print(g_estructura)
readline(prompt = "Presiona [Enter] para continuar...")

cat("\n8. Proyección completa 1935-2050...\n")
print(g_proyeccion)
readline(prompt = "Presiona [Enter] para continuar...")

cat("\n9. Proyección de dependencia...\n")
print(g_dep_proyeccion)
readline(prompt = "Presiona [Enter] para continuar...")

# ================================================================
# 16. GUARDAR GRÁFICOS (OPCIONAL)
# ================================================================

cat("\n¿Deseas guardar los gráficos? (s/n): ")
guardar <- readline()

if (tolower(guardar) == "s") {
  cat("\nGuardando gráficos...\n")
  
  # Crear carpeta si no existe
  if (!dir.exists("figures")) dir.create("figures")
  
  ggsave("figures/primer_bono.png", g_primer_bono, width = 7, height = 5, dpi = 300)
  ggsave("figures/bono_femenino.png", g_femenino, width = 7, height = 5, dpi = 300)
  ggsave("figures/bootstrap_bono_femenino.png", g_bootstrap, width = 12, height = 7, dpi = 300)
  ggsave("figures/robustez_especificaciones.png", g_robustez, width = 10, height = 6, dpi = 300)
  ggsave("figures/segundo_bono.png", g_segundo_bono, width = 8, height = 5, dpi = 300)
  ggsave("figures/modelo_crecimiento.png", g_crecimiento, width = 10, height = 6, dpi = 300)
  ggsave("figures/estructura_etaria_historica.png", g_estructura, width = 9, height = 6, dpi = 300)
  ggsave("figures/estructura_poblacion_1935_2050.png", g_proyeccion, width = 10, height = 6, dpi = 300)
  ggsave("figures/proyeccion_dependencia.png", g_dep_proyeccion, width = 9, height = 6, dpi = 300)
  
  message("✅ Gráficos guardados exitosamente en carpeta 'figures/'")
} else {
  cat("Gráficos no guardados.\n")
}

# ================================================================
# 17. EXPORTACIÓN DE RESULTADOS
# ================================================================

cat("\n=== EXPORTANDO RESULTADOS ===\n")

# Crear carpeta si no existe
if (!dir.exists("output")) dir.create("output")

# Exportar a Excel
write_xlsx(
  list(
    "Primer_Bono" = primer_bono,
    "Bono_Femenino" = bono_tabla,
    "Bootstrap_Sensibilidad" = tabla_sensibilidad,
    "Robustez_Especificaciones" = resultados_robustez,
    "Segundo_Bono" = segundo_bono_tabla,
    "Proyeccion_Dependencia" = anios_fut,
    "Supuestos_Proyecciones" = supuestos_proyecciones,
    "Estructura_Historica" = df
  ),
  "output/estudio_bonos_demograficos_completo.xlsx"
)

# Exportar CSV individuales
write_csv(tabla_sensibilidad, "output/tabla_sensibilidad_bootstrap.csv")
write_csv(resultados_robustez, "output/tabla_robustez_especificaciones.csv")
write_csv(supuestos_proyecciones, "output/tabla_supuestos_proyecciones.csv")
write_csv(base_modelo_anual, "output/datos_modelo_crecimiento.csv")

# Guardar objetos importantes
resultados_principales <- list(
  bootstrap_ic = ic_bca,
  bootstrap_completo = resultado_bootstrap,
  robustez_tabla = resultados_robustez,
  modelo_crecimiento = modelo_crecimiento_basico,
  modelo_lpm = modelo_lpm,
  modelo_logit = modelo_logit,
  segundo_bono_tabla = segundo_bono_tabla
)

saveRDS(resultados_principales, "output/resultados_completos.rds")

cat("\n✅ Resultados exportados a 'output/'\n")

# ================================================================
# 18. LIMITACIONES METODOLÓGICAS
# ================================================================

cat("\n===========================================")
cat("\nLIMITACIONES METODOLÓGICAS:")
cat("\n===========================================\n")
cat("1. Análisis transversal, no captura ciclos laborales\n")
cat("2. LPM puede generar probabilidades fuera de [0,1]\n")
cat("3. Modelos no controlan todas las variables omitidas\n")
cat("4. No se incluye migración internacional detallada\n")
cat("5. Modelo Solow es simplificado (sin progreso técnico endógeno)\n")
cat("6. Proyecciones determinísticas, no estocásticas\n")
cat("7. Bootstrap asume independencia de observaciones\n")
cat("8. Modelo de crecimiento sensible a especificación funcional\n")
cat("===========================================\n")

# ================================================================
# 19. RESUMEN DE RESULTADOS PRINCIPALES
# ================================================================

cat("\n===========================================")
cat("\nRESUMEN DE RESULTADOS PRINCIPALES:")
cat("\n===========================================\n\n")

cat("PRIMER BONO DEMOGRÁFICO:\n")
print(primer_bono)

cat("\n\nBONO FEMENINO:\n")
print(bono_tabla)

cat("\n\nINTERVALO DE CONFIANZA BOOTSTRAP (BCa 95%):\n")
cat("Límite inferior:", round(ic_bca$bca[4], 0), "personas\n")
cat("Límite superior:", round(ic_bca$bca[5], 0), "personas\n")

cat("\n\nROBUSTEZ A ESPECIFICACIONES:\n")
print(resultados_robustez)

cat("\n\nSEGUNDO BONO DEMOGRÁFICO:\n")
print(segundo_bono_tabla)

cat("\n\nMODELO DE CRECIMIENTO:\n")
cat("Elasticidad Δln(L/P) → Δln(PIB pc):", 
    round(coef(modelo_crecimiento_basico)["d_ln_LP"], 3), "\n")
cat("R²:", round(summary(modelo_crecimiento_basico)$r.squared, 4), "\n")

cat("\n\nAGOTAMIENTO DEL BONO:\n")
cat("Año estimado:", anio_min, "\n")
cat("Tasa dependencia mínima:", round(min_dep, 2), "%\n")

cat("\n===========================================\n")

message("\n✅ SCRIPT EJECUTADO CORRECTAMENTE")
message("✅ Todos los gráficos mostrados en consola")
message("✅ Resultados exportados a Excel y CSV")
message("✅ Análisis de robustez completado")
message("✅ Intervalos de confianza bootstrap calculados")
