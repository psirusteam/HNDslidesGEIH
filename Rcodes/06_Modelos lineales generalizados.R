# ==============================================================================
# Análisis de encuestas de hogares con R - Módulo 6: Modelos lineales generalizados
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
library(broom)
library(jtools)
library(modelsummary)
library(patchwork)
# remotes::install_github("BS1125/CMAverse")
library(dotwhisker)
library(CMAverse)
library(kableExtra)
rm(list = ls())

#-------------------------------------------------------------------------------
# 1. Lectura de la base de datos y diseño de encuesta
#-------------------------------------------------------------------------------

# Lectura de las bases de datos de hogares
encuesta_hog <- readRDS("Slides/Imagenes/06_MLG1/ENIGH_HND_Hogar.rds")

# Transformar y seleccionar variables

encuesta_hog <- encuesta_hog %>% # Base de datos.
  transmute(
    LLAVE_HOGAR,
    estrato = haven::as_factor(F1_A0_ESTRATO),
    Area = haven::as_factor(F1_A0_AREA),
    ingreso_per  = ifelse(YDISPONIBLE_PER < 0 , 0 , YDISPONIBLE_PER) ,
    gasto_per = GASTO_CORRIENTE_HOGAR / CANTIDAD_PERSONAS,
    pobreza_LP = case_when(
      ingreso_per < 3046 & Area == "1. Urbana" ~ "1",
      ingreso_per < 1688  &
        Area == "2. Rural" ~ "1",
      TRUE ~ "0"
    ), 
     TIPOVIVIENDA = haven::as_factor(F1_A1_P1_TIPOVIVIENDA),
     TIENEVEHICULOS = haven::as_factor(F2_A2_P1_TIENEVEHICULOS)
  )

# Lectura de las bases de datos de personas
encuesta_per <- readRDS("Slides/Imagenes/06_MLG1/ENIGH_HND_Pers.rds")

# Unir las bases de datos
encuesta <- inner_join(encuesta_hog, encuesta_per,
                       by = join_by(LLAVE_HOGAR))

# Limpiar objetos temporales
rm(encuesta_per, encuesta_hog)


#-------------------------------------------------------------------------------
# 2. Definición del diseño muestral
#-------------------------------------------------------------------------------

# Definir diseño muestral con srvyr
diseno <- encuesta %>%
  as_survey_design(
    strata = estrato,    # Id de los estratos
    ids = F1_A0_UPM,     # Id para las observaciones
    weights = Factor,    # Factores de expansión
    nest = TRUE          # Valida el anidado dentro del estrato
  )

# Tranformación de variables 

diseno <- diseno %>% mutate(
   Sexo = haven::as_factor(F2_A6_P3_SEXO), 
   etnia = haven::as_factor(F2_A6_P5_ETNIA ),
  log_ingreso_per = log(ingreso_per  + 500),
  log_gasto_per = log(gasto_per  + 500)
)

## -------------------------------------------------------------------------------
# 3. Análisis de tablas de contingencia
## -------------------------------------------------------------------------------

# Tabla de doble entrada para pobreza por sexo (totales)
(tab_pobreza_sexo <- svyby(~pobreza_LP, ~Sexo,
      FUN = svytotal, design = as.svrepdesign(diseno), 
      se=F, na.rm=T, ci=T, keep.var=TRUE))


tab <- svytable(~pobreza_LP + Sexo, design = diseno)
data.frame(tab)


# Tabla de doble entrada para pobreza por sexo (proporciones)
(tab_pobreza_sexo <- svyby(~pobreza_LP, ~Sexo,
      FUN = svymean, design = as.svrepdesign(diseno), 
      se=F, na.rm=T, ci=T, keep.var=TRUE))


prop.table(tab, margin = 2) %>% data.frame()


# Prueba Chi-cuadrado ajustada por diseño
summary(tab, statistic = "Chisq")

# Prueba F ajustada por diseño
summary(tab, statistic = "F")

# Prueba de Wald
summary(tab, statistic = "Wald")

# Prueba de Wald ajustada
summary(tab, statistic = "adjWald")


## -----------------------------------------------------------------------------
# 4. Modelos log-lineales
## -----------------------------------------------------------------------------

# Modelo log-lineal con interacción

mod1 <- svyloglin(~pobreza_LP + Sexo + pobreza_LP:Sexo , diseno)

(s1 <- summary(mod1))

# Modelo log-lineal sin interacción

mod2 <- svyloglin(~pobreza_LP + Sexo, diseno)
(s2 <- summary(mod2))


# Comparación de modelos
anova(mod1, mod2)


#-------------------------------------------------------------------------------
# 5. Modelos de regresión logística
#-------------------------------------------------------------------------------

# Ajustar modelo logístico considerando diseño muestral

mod_loglin <- svyglm(
  pobreza_LP ~ Area + TIPOVIVIENDA + TIENEVEHICULOS +
    etnia,
  family = quasibinomial,
  design = diseno %>%
    mutate(pobreza_LP = as.numeric(pobreza_LP))
)

# Resumen del modelo
tidy(mod_loglin) 

# Intervalos de confianza para los coeficientes

bind_cols(
  data.frame(OR = exp(coef(mod_loglin))), # estimado (odds ratio)
        setNames(as.data.frame(confint(mod_loglin)), c("2.5", "97.5")),   # IC en logit
          setNames(as.data.frame(exp(
            confint(mod_loglin)
          )), c("exp(2.5)", "exp(97.5)"))    # IC en OR
          )


g1 <- plot_summs(mod_loglin,
                 scale = TRUE,
                 plot.distributions = TRUE)

g1

# Pruebas de Wald para términos del modelo

regTermTest(model = mod_loglin, ~TIPOVIVIENDA)

regTermTest(model = mod_loglin, ~etnia)

regTermTest(model = mod_loglin, ~Area)


# Gráfico de efectos
effe_Area <- effect_plot(mod_loglin, pred = Area,
                         interval = TRUE)
effe_vehiculo <- effect_plot(mod_loglin, pred = TIENEVEHICULOS,
                          interval = TRUE)
effe_etnia <- effect_plot(mod_loglin, pred = etnia,
                          interval = TRUE)
effe_mod1 <-   (effe_Area | effe_vehiculo)/(effe_etnia)


#-------------------------------------------------------------------------------
# 6. Modelo con interacciones
#-------------------------------------------------------------------------------

# Modelo logístico con interacciones
mod_loglin_int <- svyglm(
  pobreza_LP ~ Area + etnia + TIPOVIVIENDA + 
    TIPOVIVIENDA:TIENEVEHICULOS + TIPOVIVIENDA:etnia ,
  family = quasibinomial,
  design = diseno %>% mutate(pobreza_LP = as.numeric(pobreza_LP))
)


# Resumen del modelo
 tidy(mod_loglin_int) 

# Comparación de los modelos 

 g2 <- plot_summs(
  mod_loglin,
  mod_loglin_int,
  scale = TRUE,
  plot.distributions = FALSE,
  model.names = c("Modelo sin interacción",
                  "Modelo con interacción"),
  colors = c("dodgerblue4", "firebrick"),
  legend.title = "Modelo",
  alpha = 0.8,
  # transparencia
  dodge = 0.6,
  # separación entre modelos
  facet.modx = FALSE
) +
  theme_minimal(base_size = 12) +
  labs(title = "Comparación de efectos estandarizados",
       x = "Efecto estandarizado (beta)",
       y = "") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))

 g2

# Intervalos de confianza para los coeficientes
bind_cols(
  data.frame(OR = exp(coef(mod_loglin_int))),                                 # estimado (odds ratio)
  setNames(as.data.frame(confint(mod_loglin_int)), c("2.5", "97.5")),  # IC en logit
  setNames(as.data.frame(exp(confint(mod_loglin_int))), c("exp(2.5)", "exp(97.5)"))    # IC en OR
)

# Pruebas de Wald para interacciones

regTermTest(model = mod_loglin_int, ~TIPOVIVIENDA )

regTermTest(model = mod_loglin_int, ~Area)

regTermTest(model = mod_loglin_int, ~etnia )

regTermTest(model = mod_loglin_int, ~TIPOVIVIENDA:etnia)

regTermTest(model = mod_loglin_int, ~TIPOVIVIENDA:TIENEVEHICULOS)

#-------------------------------------------------------------------------------
# 7. Modelo con Q-Weighting
#-------------------------------------------------------------------------------
# Ajustar modelo para estimar pesos
fit_wgt <- lm(Factor ~  Area + TIPOVIVIENDA + TIENEVEHICULOS,
              data = diseno$variables)

wgt_hat <- predict(fit_wgt)

summary(wgt_hat)

# Crear nuevos pesos
encuesta <- diseno$variables %>%  mutate(Factor2 = Factor / wgt_hat)

# Definir nuevo diseño muestral con pesos ajustados
diseno_qwgt <- encuesta %>%
  as_survey_design(
    strata = estrato,
    ids = F1_A0_UPM ,
    weights = Factor2,
    nest = TRUE
  ) %>%
  mutate(pobreza_LP = as.numeric(pobreza_LP))

# Ajustar modelo logístico con pesos ajustados
mod_loglin_qwgt <-
  svyglm(
    pobreza_LP ~ Area + TIPOVIVIENDA + TIENEVEHICULOS + 
    etnia,
    family = quasibinomial,
    design = diseno_qwgt
  )

# Resumen del modelo

(  tab_mod <- tidy(mod_loglin_qwgt) )

# Comparación de los modelos 
g3 <- plot_summs(
  mod_loglin,
  mod_loglin_qwgt,
  scale = TRUE,
  plot.distributions = FALSE,
  model.names = c("Modelo sin Q_Weighting",
                  "Modelo con Q_Weighting"),
  colors = c("dodgerblue4", "firebrick"),
  legend.title = "Modelo",
  alpha = 0.8,
  # transparencia
  dodge = 0.6,
  # separación entre modelos
  facet.modx = FALSE
) +
  theme_minimal(base_size = 12) +
  labs(title = "Comparación de efectos estandarizados",
       x = "Efecto estandarizado (beta)",
       y = "") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))

g3

# Intervalos de confianza para los coeficientes

bind_cols(
  data.frame(OR = exp(coef(mod_loglin_qwgt))),                                 # estimado (odds ratio)
  setNames(as.data.frame(confint(mod_loglin_qwgt)), c("2.5", "97.5")),  # IC en logit
  setNames(as.data.frame(exp(confint(mod_loglin_qwgt))), c("exp(2.5)", "exp(97.5)"))    # IC en OR
)

# Pruebas de Wald para interacciones

regTermTest(model = mod_loglin_qwgt, ~TIENEVEHICULOS)

regTermTest(model = mod_loglin_qwgt, ~Area)

regTermTest(model = mod_loglin_qwgt, ~etnia)

# Gráfico de efectos
effe_Area <- effect_plot(mod_loglin_qwgt, pred = Area,
                         interval = TRUE)
effe_vehiculo <- effect_plot(mod_loglin_qwgt, pred = TIENEVEHICULOS,
                             interval = TRUE)
effe_etnia <- effect_plot(mod_loglin_qwgt, pred = etnia,
                          interval = TRUE)
effe_mod3 <-   (effe_Area | effe_vehiculo) / (effe_etnia)

#-------------------------------------------------------------------------------
# 8. Modelos Gamma para variables continuas
#-------------------------------------------------------------------------------

encuesta_temp <- filter(encuesta, ingreso_per < 75000)
# Conteo de los registros eliminados 
nrow(encuesta) -nrow(encuesta_temp)

# Estimación de parámetros Gamma por método de momentos
x <- encuesta_temp$ingreso_per

n = length(x)

shape1 = (n*mean(x)^2)/sum((x-mean(x))^2)

rate1 = (n*mean(x))/sum((x-mean(x))^2)

c(shape1 = shape1, rate1 = rate1)

# Gráfico de densidad comparativa

plot_gamma1 <- ggplot(data = encuesta_temp, aes(x = ingreso_per)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "white", fill = "skyblue", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1.5) +
  stat_function(fun = dgamma, args = list(shape = shape1, rate = rate1), col = "red", size = 1.5) +
  labs(
    title = "Distribución de Ingresos",
    x = "Ingresos",
    y = "Densidad"
  ) + theme_minimal()



# Crear nuevas variables para el modelo Gamma

diseno_qwgt2 <- diseno_qwgt %>% 
  mutate(ingreso_per2  = ingreso_per + 1, 
         edad = case_when(F2_A6_P4_EDAD < 16 ~ "0 - 15", 
                          F2_A6_P4_EDAD < 21 ~ "16 - 20",
                          F2_A6_P4_EDAD < 26 ~ "21 - 25",
                          F2_A6_P4_EDAD < 31 ~ "26 - 30",
                          F2_A6_P4_EDAD < 36 ~ "31 - 35",
                          F2_A6_P4_EDAD < 41 ~ "36 - 40",
                          F2_A6_P4_EDAD < 46 ~ "41 - 45",
                          F2_A6_P4_EDAD < 51 ~ "46 - 50",
                          TRUE ~ "51 o más"))

# Ajustar modelo Gamma

modelo <- svyglm(formula = ingreso_per2  ~ edad + Area+   Area*Sexo + 
                   TIPOVIVIENDA + TIENEVEHICULOS  ,
                   design = diseno_qwgt2, 
                  family = Gamma(link = "inverse"))

# Resumen del modelo

broom::tidy(modelo) %>% mutate(across(
  c("estimate", "std.error"),
  ~ format(., scientific = FALSE, digits = 10)
))

# Estimación de dispersión
(alpha = MASS::gamma.dispersion(modelo))
mod_s <- summary(modelo, dispersion = alpha)
mod_s$dispersion


# Predicciones e intervalos de confianza
pred <- predict(modelo, type = "response", se = T)

pred_IC <- data.frame(confint(pred))

colnames(pred_IC) <- c("Lim_Inf", "Lim_Sup")

pred <- bind_cols(data.frame(pred), pred_IC)

pred$ingreso_per2 <- encuesta$ingreso_per + 1

pred %>% slice(1:15L)


# Gráfico de predicciones
pd <- position_dodge(width = 0.2)
plot_pred <- ggplot(pred %>% slice(1:1000L),
       aes(x = ingreso_per2 , y = response)) +
  geom_errorbar(aes(ymin = Lim_Inf,
                    ymax = Lim_Sup),
                width = .1,
                linetype = 1) +
  geom_point(size = 2, position = pd) +
  theme_bw()

plot_pred


