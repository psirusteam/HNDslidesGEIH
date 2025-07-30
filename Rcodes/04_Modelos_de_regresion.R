# ==============================================================================
# Análisis de encuestas de hogares con R - Módulo 4: Modelos de regresión
# CEPAL - Unidad de Estadísticas Sociales
# Autor: Andrés Gutiérrez y Stalyn Guerrero 
# Email: andres.gutierrez@cepal.org
# ==============================================================================

#-------------------------------------------------------------------------------
# Cargar librerías necesarias
#-------------------------------------------------------------------------------
library (survey)
library(srvyr)
library(convey)
library(TeachingSampling)
library(printr)
library(stargazer)
library(jtools)
library(broom)
library(ggpmisc)
library(modelsummary)
library(nortest)  #REALIZA 10 PRUEBAS 
library(moments)  #REALIZA 1 PRUEBA
library(svydiags)
library(magrittr)
library(purrr)
library(haven)

theme_cepal <- function(...) theme_light(10) + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="bottom", 
        legend.justification = "left", 
        legend.direction="horizontal",
        plot.title = element_text(size = 20, hjust = 0.5),
        ...) 


select <- dplyr::select

#-------------------------------------------------------------------------------
# 1. Lectura de la base de datos y diseño de encuesta
#-------------------------------------------------------------------------------
# Leer la base de datos (ajustar la ruta según sea necesario)
encuesta <- readRDS("Slides/Imagenes/02_variable_continua/ENIGH_HND_Hogar.rds")

# Preparación de variables derivadas en la base 'encuesta'


encuesta <- encuesta %>%
  mutate(
    # Conversión de la variable de estrato a factor etiquetado (según haven)
    estrato = haven::as_factor(F1_A0_ESTRATO),
    
    # Conversión de la variable de área (urbano/rural) a factor
    Area = haven::as_factor(F1_A0_AREA),
    
    # Corrección de ingresos negativos: se reemplazan por cero
    ingreso_per = ifelse(YDISPONIBLE_PER < 0, 0, YDISPONIBLE_PER),
    
    # Cálculo del ingreso total del hogar como ingreso per cápita * número de personas
    ingreso_hog = ingreso_per * CANTIDAD_PERSONAS
  )

# Exploración inicial de variables monetarias

# Selección de variables clave y visualización de las primeras 20 observaciones
encuesta %>%
  dplyr::select(
    CANTIDAD_PERSONAS,           # Tamaño del hogar
    GASTO_CORRIENTE_HOGAR,       # Gasto total corriente del hogar
    CONSUMO_FINAL_HOGAR,         # Consumo final estimado
    YDISPONIBLE_PER,             # Ingreso per cápita sin corrección
    ingreso_per,                 # Ingreso per cápita corregido
    ingreso_hog                  # Ingreso total del hogar estimado
  ) %>%
  head(20)


# Configuración global para manejar PSU solitarios en dominios con pocos elementos.
# La opción "adjust" aplica un ajuste de varianza recomendado en estos casos.
options(survey.lonely.psu = "adjust")  

# Definición del diseño muestral complejo
diseno <- encuesta %>%
  as_survey_design(
    strata = estrato,     # Estrato de muestreo (factor)
    ids = F1_A0_UPM,      # Identificador de la Unidad Primaria de Muestreo (UPM)
    weights = Factor,     # Factor de expansión para cada observación
    nest = TRUE           # Indica que las UPM están anidadas dentro de los estratos
  )


# Crear subgrupos basados en área
sub_Urbano <- diseno %>% filter(Area == "1. Urbana")
sub_Rural <- diseno %>% filter(Area == "2. Rural")

#-------------------------------------------------------------------------------
# 2. Visualización de datos
#-------------------------------------------------------------------------------

# Scatterplot sin ponderar
# Este gráfico muestra la relación entre el gasto corriente del hogar y el 
# ingreso del hogar, sin considerar los factores de expansión muestral.

plot_sin <-
  ggplot(data = encuesta,
         aes(x = GASTO_CORRIENTE_HOGAR, y = ingreso_hog)) +
  geom_point() +  # puntos individuales para cada observación
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +  # línea de regresión lineal
  theme_cepal()  # tema gráfico personalizado (CEPAL)

# Añade la ecuación de la recta y el R² al gráfico sin ponderar
plot_sin <- plot_sin + stat_poly_eq(
  formula = y ~ x,
  aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"), size = 5),
  parse = TRUE
)

plot_sin  # mostrar gráfico


# Scatterplot ponderado
# Este gráfico es similar al anterior, pero considera los pesos muestrales (Factor)

plot_Ponde <-
  ggplot(data = encuesta,
         aes(x = GASTO_CORRIENTE_HOGAR, y = ingreso_hog)) +
  geom_point(aes(size = Factor)) +  # tamaño del punto proporcional al peso muestral
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x,
              mapping = aes(weight = Factor)) +  # línea de regresión ponderada
  theme_cepal()

# Añade la ecuación de la recta y el R² al gráfico ponderado
plot_Ponde <- plot_Ponde + stat_poly_eq(
  formula = y ~ x,
  aes(weight = Factor, label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
  parse = TRUE,
  size = 5
)

plot_Ponde  # mostrar gráfico

#-------------------------------------------------------------------------------
# 3. Modelos de regresión
#-------------------------------------------------------------------------------

# Regresión lineal simple sin ponderar
fit_sinP <- lm(ingreso_hog ~ GASTO_CORRIENTE_HOGAR, data = encuesta)
summary(fit_sinP)  # resumen del modelo

# Regresión ponderada por el factor de expansión muestral
fit_Ponde <- lm(ingreso_hog ~ GASTO_CORRIENTE_HOGAR,
                data = encuesta,
                weights = Factor)
summary(fit_Ponde)  # resumen del modelo ponderado

# Regresión considerando el diseño muestral completo con 'survey'
fit_svy <- svyglm(ingreso_hog ~ GASTO_CORRIENTE_HOGAR, 
                  design = diseno)
summary(fit_svy)  # resumen del modelo con diseño complejo


# Comparación de modelos
df_model <- data.frame(
  intercept = c(coefficients(fit_sinP)[1],
                coefficients(fit_Ponde)[1],
                coefficients(fit_svy)[1]),
  slope = c(coefficients(fit_sinP)[2],
            coefficients(fit_Ponde)[2],
            coefficients(fit_svy)[2]),
  Modelo = c("Sin ponderar", "Ponderado(lm)", "Ponderado(svyglm)")
)

# Gráfico comparativo
plot_Ponde2 <- plot_Ponde +
  geom_abline(
    data = df_model,
    mapping = aes(
      slope = slope,
      intercept = intercept,
      linetype = Modelo,
      color = Modelo
    ),
    size = 1
  )
# Configura la opción para que los números en tablas LaTeX sean planos (sin formato científico)
options("modelsummary_format_numeric_latex" = "plain")

# Comparación de tres modelos de regresión usando modelsummary:
# 1. Modelo sin ponderación
# 2. Modelo ponderado con lm
# 3. Modelo ponderado con svyglm (diseño muestral)
modelsummary(
  list(
    "Sin Pond" = fit_sinP,
    "Ponde(lm)" = fit_Ponde,
    "Ponde(svyglm)" = fit_svy
  ),
  statistic = c("p = ({p.value})"),   # Muestra solo los valores p
  gof_omit = 'BIC|Log',               # Omite BIC y log-likelihood del resumen
  output = "html",                    # Salida en formato HTML
  latex_engine = "kable"             # Motor LaTeX (irrelevante para HTML pero útil si se cambia)
)

# -------------------------------------------------------------------------
# Estimación del R2 ponderado a partir del modelo svyglm
# -------------------------------------------------------------------------

# Ajuste del modelo de regresión ponderado con diseño muestral
fit_svy <- svyglm(ingreso_hog ~ GASTO_CORRIENTE_HOGAR, 
                  design = diseno, family = stats::gaussian())

# Cálculo de la media ponderada del ingreso del hogar
medY <- diseno %>% summarise(medY = survey_mean(ingreso_hog))

# Cálculo de sumas de cuadrados totales (SST) y explicadas (SSE)
diseno %<>% mutate(
  ypred = fitted(fit_svy, type = "response"),     # Valores predichos por el modelo
  medY = medY,                                    # Media ponderada del ingreso
  sst = (ingreso_hog - medY$medY)^2,              # Suma de cuadrados total
  sse = (ypred - medY$medY)^2                     # Suma de cuadrados explicada
)

# Estimación total de SST y SSE usando diseño complejo
diseno %>% summarise(
  WSST = survey_total(sst),
  WSSE = survey_total(sse)
) %>% 
  transmute(
    WSST, WSSE,
    R2 = WSSE / WSST                  # Coeficiente de determinación ponderado
  )

# -------------------------------------------------------------------------
# Estimación alternativa del R2 usando la dispersión residual
# -------------------------------------------------------------------------

# Modelo nulo (solo intercepto)
modNul <- svyglm(ingreso_hog ~ 1, design = diseno)

# Resúmenes de ambos modelos
s1 <- summary(fit_svy)
s0 <- summary(modNul)

# Dispersión residual de cada modelo
WSST <- s0$dispersion   # Varianza del modelo nulo
WSSE <- s1$dispersion   # Varianza del modelo completo

# R² ponderado alternativo basado en la reducción de la dispersión
R2 <- 1 - WSSE / WSST
R2

# -------------------------------------------------------------------------
# Estimación del R2 ajustado ponderado
# -------------------------------------------------------------------------

n <- nrow(encuesta)  # Tamaño total de la muestra
p <- 2                # Número de parámetros en el modelo (intercepto + 1 predictor)

# R² ajustado para tener en cuenta el número de predictores
R2Adj <- 1 - ((n - 1) / (n - p)) * (1 - R2)
R2Adj

## -----------------------------------------------------------------------------
# 4. Método de Q-Weighting (Pfeffermann, 2011)
## -----------------------------------------------------------------------------

# Paso 1: Ajustar modelo para q-weights
fit_wgt <- lm(1/Factor ~ GASTO_CORRIENTE_HOGAR, data = encuesta)

# Paso 2: Obtener predicciones de q-weights
qw <- predict(fit_wgt)
summary(qw)


# Paso 3: Crear nuevos q-weights
encuesta <- encuesta %>% mutate(wk1 = Factor/qw)


# Paso 4: Definir nuevo diseño muestral con q-weights
diseno_qwgt <- encuesta %>%
  as_survey_design(
    strata = estrato,
    ids = F1_A0_UPM,
    weights = wk1,
    nest = TRUE
  )

# Modelo con q-weights
fit_svy_qwgt <- svyglm(ingreso_hog ~ GASTO_CORRIENTE_HOGAR,
                       design = diseno_qwgt)
s1_qwgt <- summary(fit_svy_qwgt)
tidy(fit_svy_qwgt)



modelsummary(list("svyglm(wgt)" = fit_svy,
                  "svyglm(qwgt)" = fit_svy_qwgt),
             output = "html",
              statistic = c("p = ({p.value})"),
             title = "Comprando Modelos con Q Weighting",
             gof_omit = 'R2 Adj.|BIC|Log')

## -----------------------------------------------------------------------------
# 5. Modelo propuesto final
## -----------------------------------------------------------------------------
# Preparar variables adicionales

diseno_qwgt <-
  diseno_qwgt %>% mutate(
    TIPOVIVIENDA = as_factor(F1_A1_P1_TIPOVIVIENDA),
    TIENEVEHICULOS = as_factor(F2_A2_P1_TIENEVEHICULOS))

# Modelo final con transformación logarítmica

mod_svy <- svyglm(
  log(ingreso_hog + 500) ~ log(GASTO_CORRIENTE_HOGAR +500) +
    TIENEVEHICULOS + Area  ,
  design = diseno_qwgt
)

s1_final <- summary(mod_svy)

modelsummary(
  list(
    mod_svy
  ),
  statistic = c("p = ({p.value})"),   # Muestra solo los valores p
  gof_omit = 'BIC|Log',               # Omite BIC y log-likelihood del resumen
  output = "html",                    # Salida en formato HTML
  latex_engine = "kable"             # Motor LaTeX (irrelevante para HTML pero útil si se cambia)
)

par(mfrow = c(2, 2))
plot(mod_svy)

## -----------------------------------------------------------------------------
# 6. Diagnósticos del modelo
## -----------------------------------------------------------------------------

# Residuales estandarizados

stdresids = as.numeric(svystdres(mod_svy)$stdresids)
diseno_qwgt$variables %<>% 
  mutate(stdresids = stdresids)

# Histograma de residuales
p1_hist <- ggplot(data = diseno_qwgt$variables,
                  aes(x = stdresids)) +
  geom_histogram(
    aes(y = ..density..),
    colour = "black",
    bins = 30,
    fill = "blue",
    alpha = 0.3
  ) + geom_density(linewidth = 2, colour = "blue") +
  geom_function(fun = dnorm, colour = "red",
                linewidth = 2) +
  theme_cepal() + labs(y = "")

p1_hist

# Gráficos de varianza constante

library(patchwork)
diseno_qwgt$variables %<>% 
  mutate(pred = predict(mod_svy))
g2 <- ggplot(data = diseno_qwgt$variables,
             aes(x = log(GASTO_CORRIENTE_HOGAR + 
                           500), y = stdresids))+
  geom_point() +
  geom_hline(yintercept = 0) + theme_cepal()
g3 <- ggplot(data = diseno_qwgt$variables,
             aes(x = TIENEVEHICULOS , y = stdresids))+
  geom_point() +
  geom_hline(yintercept = 0) + theme_cepal()


g4 <- ggplot(data = diseno_qwgt$variables,
             aes(x = Area, y = stdresids)) +
  geom_point() +
  geom_hline(yintercept = 0) + theme_minimal()

g6 <- (g4|g3)/(g2)
g6

## -----------------------------------------------------------------------------
# 7. Detección de observaciones influyentes
## -----------------------------------------------------------------------------

# Distancia de Cook

d_cook = data.frame(cook = svyCooksD(mod_svy),
                    id = 1:length(svyCooksD(mod_svy)))

g_dcook <- ggplot(d_cook, aes(y = cook, x = id)) +
  geom_point() +
  theme_bw(20)

g_dcook

# DfBetas
d_dfbetas = data.frame(t(svydfbetas(mod_svy)$Dfbetas))
colnames(d_dfbetas) <- paste0("Beta_", 1:4)
d_dfbetas %>% slice(1:5L)

d_dfbetas$id <- 1:nrow(d_dfbetas)
d_dfbetas <- reshape2::melt(d_dfbetas, 
                            id.vars = "id")
cutoff <- svydfbetas(mod_svy)$cutoff

d_dfbetas <- d_dfbetas %>%
  mutate(Criterio = ifelse(abs(value) > cutoff, "Si", "No"))

tex_label <- d_dfbetas %>% 
  filter(Criterio == "Si") %>%
  arrange(desc(abs(value))) %>%
  slice(1:10L)

# Gráfico DfBetas

Fig_DFbetas <- ggplot(d_dfbetas, aes(y = abs(value), x = id)) +
  geom_point(aes(col = Criterio)) +
  geom_text(data = tex_label,
            angle = 45,
            vjust = -1,
            aes(label = id)) +
  geom_hline(aes(yintercept = cutoff)) +
  facet_wrap(. ~ variable, nrow = 2) +
  scale_color_manual(values = c("Si" = "red", "No" = "black")) +
  theme_cepal()

Fig_DFbetas

# DfFits

d_dffits = data.frame(
  dffits = svydffits(mod_svy)$Dffits,
  id = 1:length(svydffits(mod_svy)$Dffits))

cutoff <- svydffits(mod_svy)$cutoff

d_dffits <- d_dffits %>%
  mutate(C_cutoff = ifelse(abs(dffits) > cutoff, "Si", "No"))

g_dffits <- ggplot(d_dffits, aes(y = abs(dffits), x = id)) +
  geom_point(aes(col = C_cutoff)) +
  geom_hline(yintercept = cutoff) +
   scale_color_manual(
    values = c("Si" = "red", "No" = "black"))+
  theme_cepal()

g_dffits


# Matriz Hat

vec_hat <- svyhat(mod_svy, doplot = FALSE)

d_hat = data.frame(hat = vec_hat,
                    id = 1:length(vec_hat))
d_hat <- d_hat %>%
  mutate(C_cutoff = ifelse(hat > (3 * mean(hat)), "Si", "No"))

g_hat <- ggplot(d_hat, aes(y = hat, x = id)) +
  geom_point(aes(col = C_cutoff)) +
  geom_hline(yintercept = (3 * mean(d_hat$hat))) +
  scale_color_manual(
    values = c("Si" = "red", "No" = "black"))+
  theme_cepal()

## -----------------------------------------------------------------------------
# 8. Inferencia sobre los parámetros
## -----------------------------------------------------------------------------

# Resumen de coeficientes
mod_svy %>% broom::tidy()

# Intervalos de confianza
survey:::confint.svyglm(mod_svy)


## -----------------------------------------------------------------------------
# 9. Predicciones
## -----------------------------------------------------------------------------

model.matrix(mod_svy) %>% as.data.frame()%>% slice(1:7)

vcov(mod_svy)


xobs <- model.matrix(mod_svy) %>%
  data.frame() %>% slice(1) %>% as.matrix()

cov_beta <- vcov(mod_svy) %>% as.matrix()

as.numeric(xobs %*% cov_beta %*% t(xobs))

# Predicciones para observaciones en la muestra
pred <- data.frame(predict(mod_svy, type = "response"))
pred_IC <- data.frame(confint(predict(mod_svy, type = "response")))
colnames(pred_IC) <- c("Lim_Inf", "Lim_Sup")
pred <- bind_cols(pred, pred_IC)

## ------------------------------------------------------------------------
pred %>% slice(1:10)


# Gráfico de predicciones
pred$log_ingreso_hog <- log(encuesta$ingreso_hog + 500)
pd <- position_dodge(width = 0.2)
pred_mod <-  ggplot(pred %>% slice(1:100L),
                    aes(
                      x = exp(log_ingreso_hog) - 500 ,
                      y = exp(response) - 500
                    )) +
  geom_errorbar(aes(ymin = exp(Lim_Inf) - 500,
                    ymax = exp(Lim_Sup) - 500),
                width = .1,
                linetype = 1) +
  geom_point(size = 1, position = pd) +
  theme_bw()

pred_mod


# Predicción para nuevos datos

datos_nuevos <- data.frame(GASTO_CORRIENTE_HOGAR  = 500, 
                           TIENEVEHICULOS = "2. No",
                           Area = "2. Rural" )



x_noObs = matrix(c(1,log(500+500) ,1,1),nrow = 1)
as.numeric(sqrt(x_noObs%*%cov_beta%*%t(x_noObs)))


# Realizar predicción 
predict(mod_svy, newdata = datos_nuevos, type =  "link") %>% 
  data.frame()



confint(predict(mod_svy, newdata = datos_nuevos))

#-------------------------------------------------------------------------------
# FIN DEL SCRIPT
#-------------------------------------------------------------------------------
