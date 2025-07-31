# ==============================================================================
# Análisis de encuestas de hogares con R - Modulo 7: Modelos multinivel
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
library(ggplot2)
library(stringi)


theme_cepal <- function(...){
  theme_light(10) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.justification = "left",
    legend.direction = "horizontal",
    plot.title = element_text(size = 20, hjust = 0.5),
    ...
  ) }

# ------------------------------------------------------------------------------
# 1. Preparando los datos
# ------------------------------------------------------------------------------

# Cargar la base de datos de hogares
encuesta_hog <- readRDS("Slides/Imagenes/06_MLG1/ENIGH_HND_Hogar.rds")

# Filtrar y transformar los datos
encuesta_hog <- encuesta_hog %>% # Base de datos.
  filter(YDISPONIBLE_PER > 0 ) %>% 
  transmute(
    LLAVE_HOGAR,
    estrato = haven::as_factor(F1_A0_ESTRATO),
    dam = haven::as_factor(F1_A0_DEPARTAMENTO),
    Area = haven::as_factor(F1_A0_AREA),
    ingreso_per  = ifelse(YDISPONIBLE_PER < 0 , 0 , YDISPONIBLE_PER) ,
    gasto_per = GASTO_CORRIENTE_HOGAR / CANTIDAD_PERSONAS,
    pobreza_LP = case_when(
      ingreso_per < 3046 & Area == "1. Urbana" ~ 1,
      ingreso_per < 1688  &
        Area == "2. Rural" ~ 1,
      TRUE ~ 0
    ), 
    log_gasto = log(gasto_per + 500),
    log_ingreso = log(ingreso_per + 500),
     TIPOVIVIENDA = haven::as_factor(F1_A1_P1_TIPOVIVIENDA),
     TIENEVEHICULOS = haven::as_factor(F2_A2_P1_TIENEVEHICULOS), 
    Factor
  )

# Definir departamentos de interés
dam_pba <- c("1. Atlántida", "2. Colon",
             "3. Comayagua", "4. Copan", "5. Cortes")

# Muestra de los datos iniciales 

encuesta_hog  %>% slice(1:10L) %>% dplyr::select(dam:pobreza_LP)

# Filtrando los departamentos de interés
encuesta_plot <- encuesta_hog %>% filter(dam %in% dam_pba)

#-------------------------------------------------------------------------------
# 2. Gráfico de dispersión con ajuste lineal
#-------------------------------------------------------------------------------

# Se grafica la relación entre el logaritmo del ingreso y el logaritmo del gasto
# utilizando la base 'encuesta_plot'

plot_mult1 <- ggplot(data = encuesta_plot,
                     aes(y = log_ingreso, x = log_gasto)) +
  geom_jitter() +  # Se agrega ruido a los puntos para evitar sobreposición
  theme(legend.position = "none",  # Se elimina la leyenda
        plot.title = element_text(hjust = 0.5)) +  # Centrado del título
  geom_smooth(formula = y ~ x, 
              method = "lm",  # Ajuste lineal
              se = FALSE) +   # No mostrar bandas de error
  ggtitle(
    latex2exp::TeX(
      "$Ingreso_{i}\\sim\\hat{\\beta}_{0}+\\hat{\\beta}_{1}Gasto_{i}+\\epsilon_{i}$"
    )) +  # Título en notación matemática con LaTeX
  theme_cepal()  # Tema gráfico personalizado de CEPAL

# Mostrar el gráfico
plot_mult1

#-------------------------------------------------------------------------------
# Ajuste de un modelo de regresión lineal sin intercepto global
# permitiendo interceptos específicos por dominio (dam)
#-------------------------------------------------------------------------------

# Se utiliza la base 'encuesta_hog'

modelo <- lm(log_ingreso ~ 0 + dam + log_gasto, data = encuesta_hog)

# Extraer el coeficiente de la variable log_gasto (pendiente común para todos los dominios)
B1 <- as.numeric(coef(modelo)["log_gasto"])

# Extraer los coeficientes asociados a los interceptos por dominio (dam)
B0 <- coef(modelo)[1:18]  # Se asume que hay 18 dominios

# Limpiar los nombres de las variables eliminando el prefijo "dam"
names(B0) <- stri_replace_all_fixed(str = names(B0), 
                                    pattern = "dam",  
                                    replacement = "")

# Crear un data.frame con los coeficientes por dominio
coef_Mod <- data.frame(
  dam = names(B0),   # Código del dominio
  B0 = as.numeric(B0),  # Intercepto específico
  B1 = B1  # Pendiente común
) %>% 
  filter(dam %in% dam_pba)  # Filtrar solo dominios de prueba

# Mostrar la tabla de coeficientes
coef_Mod

# Gráfico de dispersión con líneas de regresión específicas por grupo (dam)

plot_mult2  <- ggplot(data = encuesta_plot,
       aes(y = log_ingreso , x = log_gasto,
           colour = dam)) +
  geom_jitter() + theme(legend.position="none",
    plot.title = element_text(hjust = 0.5)) +
  geom_abline(data = coef_Mod,
              mapping=aes(slope=B1,
                          intercept=B0,
                          colour = dam)) +
  ggtitle(
    latex2exp::TeX("$Ingreso_{ij}\\sim\\hat{\\beta}_{0j}+\\hat{\\beta}_{1}Gasto_{ij}+\\epsilon_{ij}$"))+
  theme_cepal()



#-------------------------------------------------------------------------------
# Ajuste del modelo lineal pendiente aleatoria 
#-------------------------------------------------------------------------------

modelo <- lm(log_ingreso ~ log_gasto:dam + 1, data = encuesta_hog)

# Extracción del intercepto
B0 <- as.numeric(coef(modelo)["(Intercept)"])

# Extracción y renombramiento de los coeficientes de interacción
B1 <- coef(modelo)[2:19]
names(B1) <- stri_replace_all_fixed(
  str = names(B1),
  pattern = "log_gasto:dam",
  replacement = ""
)

# Creación del data.frame con los coeficientes para graficar
coef_Mod <- data.frame(
  dam = names(B1),
  B0 = B0,
  B1 = as.numeric(B1)
) %>%
  filter(dam %in% dam_pba)

# Visualización de los coeficientes
coef_Mod


plot_mult3 <- ggplot(data = encuesta_plot,
       aes(y = log_ingreso , x = log_gasto,
           colour = dam)) +
  geom_jitter() + theme(legend.position="none",
    plot.title = element_text(hjust = 0.5)) +
  geom_abline(data = coef_Mod,
              mapping=aes(slope=B1,
                          intercept=B0, colour = dam)) +
  ggtitle(
    latex2exp::TeX("$Ingreso_{ij}\\sim\\hat{\\beta}_{0}+\\hat{\\beta}_{1j}Gasto_{ij}+\\epsilon_{ij}$"))+
  theme_cepal()

plot_mult3


## Modelo con pendiente e intercepto aleatoria  

plot_mult4 <- ggplot(data = encuesta_plot,
       aes(y = log_ingreso , x = log_gasto,
           colour = dam)) +
    geom_smooth( formula = y ~ x, method = "lm", se = F) +
  geom_jitter() + theme(legend.position="none",
    plot.title = element_text(hjust = 0.5)) +
  ggtitle(
    latex2exp::TeX("$Ingreso_{ij}\\sim\\hat{\\beta}_{0j}+\\hat{\\beta}_{1j}Gasto_{ij}+\\epsilon_{ij}$"))+
  theme_cepal()


# ------------------------------------------------------------------------------
# 3. Cálculo de pesos 
# ------------------------------------------------------------------------------

# Calcular pesos Qweighted
mod_qw <- lm( Factor ~  Area + TIPOVIVIENDA +  TIENEVEHICULOS ,
             data = encuesta_hog)

summary(predict(mod_qw))

encuesta_hog$Factor2 <-   encuesta_hog$Factor/predict(mod_qw)

# Calcular pesos senate-weight
n = nrow(encuesta_hog)

encuesta_hog <- encuesta_hog %>%
  mutate(Factor3 = n * Factor / sum(Factor))

# Comparar los pesos
encuesta_hog %>% summarise(
  fep = sum(Factor),
  q_wei = sum(Factor2),
  fep2 = sum(Factor3)
)

qw0 <- ggplot(encuesta_hog, aes(x = Factor2, y = Factor3)) +
  geom_point() +   theme_bw() +
  labs(x = "q-weighted", y = "senate-weight")
qw0

#-------------------------------------------------------------------------------
#4. Modelos lineales multinivel 
# ------------------------------------------------------------------------------

library(lme4) 

# 4.1 Modelo nulo (solo intercepto aleatorio)
mod_null  <- lmer(log_ingreso   ~ (1  |  dam),
                  data  =  encuesta_hog,
                  weights  =  Factor2)

mod_null2  <- lmer(log_ingreso   ~ (1  |  dam),
                   data  =  encuesta_hog,
                   weights  =  Factor3)


## Comparando los intercepto aleatorios 

coef_mod_null <- bind_cols(coef(mod_null)$dam,
                           coef(mod_null2)$dam)
colnames(coef_mod_null) <- c("Intercept Mod 1",
                             "Intercept Mod 2")
coef_mod_null 

mod_null

# Correlación intraclasica de los coeficientes 
performance::icc(mod_null)

# Predicción del modelo 

(tab_pred <- data.frame(Pred = predict(mod_null), 
           log_ingreso  = encuesta_hog$log_ingreso , 
           dam = encuesta_hog$dam)) %>% distinct() %>% 
  slice(1:6L) # Son las pendientes aleatorias

## Predicción del modelo nulo 
mod_null <-
  ggplot(data = tab_pred, aes(x = Pred, y = log_ingreso , colour = dam)) +
  geom_point() + geom_abline(intercept = 0,
                             slope = 1,
                             colour = "red") +
  theme_bw() + theme(legend.position = "none")


# 4.2 Modelo con intercepto aleatorio y efecto fijo de gasto

mod_Int_Aleatorio <- lmer(
  log_ingreso  ~ log_gasto   + (1 | dam),
  data = encuesta_hog, weights  =  Factor2)

performance::icc(mod_Int_Aleatorio)

## Efecto aleatorio por departamento 

coef(mod_Int_Aleatorio)$dam 


Coef_Estimado <- inner_join(
  coef(mod_Int_Aleatorio)$dam %>%
    tibble::rownames_to_column(var = "dam"),
  encuesta_plot %>% dplyr::select(dam) %>% distinct()
)

# Gráfica del modelo

plot_mod_Int_Aleatorio <-
  ggplot(data = encuesta_plot,
         aes(y = log_ingreso , x = log_gasto, colour = dam)) +
  geom_jitter() +
  theme(legend.position = "none",
       plot.title = element_text(hjust = 0.5)) +
  geom_abline(
    data = Coef_Estimado,
    mapping = aes(
      slope = log_gasto,
      intercept = `(Intercept)`,
      colour = dam
    ) ) +   theme_cepal()

plot_mod_Int_Aleatorio

## Predicción del modelo 

(tab_pred <- data.frame(
  Pred = predict(mod_Int_Aleatorio), 
           log_ingreso  = encuesta_hog$log_ingreso , 
           dam = encuesta_hog$dam)) %>% distinct() %>% 
  slice(1:6L) # Son las pendientes aleatorias



plot_Int_Aleatorio <- ggplot(data = tab_pred, aes(x = Pred, y = log_ingreso , colour = dam)) +
  geom_point() + geom_abline(intercept = 0, slope = 1, colour = "red") +
  theme_bw() + theme(legend.position = "none")

plot_Int_Aleatorio


# 4.3 Modelo con intercepto y pendiente aleatoria

mod_Pen_Aleatorio <- lmer(
  log_ingreso  ~ log_gasto  + (1 + log_gasto| dam),
  data = encuesta_hog, weights  =  Factor2)

performance::icc(mod_Pen_Aleatorio)



coef(mod_Pen_Aleatorio)$dam %>% slice(1:14L)

# Coeficiente del modelo 

Coef_Estimado <- inner_join(
  coef(mod_Pen_Aleatorio)$dam %>%
       add_rownames(var = "dam"),
encuesta_plot %>% dplyr::select(dam) %>% distinct())


# Scaterplot del modelo 

plot_mod_Pen_Aleatorio <-ggplot(data = encuesta_plot,
       aes(y = log_ingreso , x = log_gasto,
           colour = dam)) +
  geom_jitter() + theme(legend.position="none",
    plot.title = element_text(hjust = 0.5)) +
  geom_abline(data = Coef_Estimado,
              mapping=aes(slope=log_gasto,
                          intercept=`(Intercept)`,
                          colour = dam))+
  theme_cepal()

plot_mod_Pen_Aleatorio

# Predicción del modelo 

(tab_pred <- data.frame(Pred = predict(mod_Pen_Aleatorio), 
           log_ingreso  = encuesta_hog$log_ingreso , 
           dam = encuesta_hog$dam)) %>% distinct() %>% 
  slice(1:8L) # Son las pendientes aleatorias


plot_mod_Pen_Aleatorio_pred <- ggplot(data = tab_pred, aes(x = Pred, y = log_ingreso , colour = dam)) +
  geom_point() + geom_abline(intercept = 0, slope = 1, colour = "red") +
  theme_bw() + theme(legend.position = "none")

plot_mod_Pen_Aleatorio_pred



# 4.4 Modelo más complejo con área y gasto medio por dam

media_dam <- encuesta_hog %>% group_by(dam) %>% 
  summarise(mu = mean(log_gasto))
encuesta_hog <- inner_join(encuesta_hog, 
                       media_dam, by = "dam")  

mod_Pen_Aleatorio2 <- lmer(
  log_ingreso  ~ 1 + log_gasto + Area + mu + 
    (1 + log_gasto + Area + mu | dam ),
    data = encuesta_hog, weights  =  Factor2)

performance::icc(mod_Pen_Aleatorio2)


# Predicción del modelo 

(tab_pred <- data.frame(Pred = predict(mod_Pen_Aleatorio2), 
           log_ingreso  = encuesta_hog$log_ingreso , 
           dam = encuesta_hog$dam)) %>% distinct() %>% 
  slice(1:10L) # Son las pendientes aleatorias


mod_Pen_Aleatorio2_pred <-
ggplot(data = tab_pred, aes(x = Pred, y = log_ingreso , colour = dam)) +
  geom_point() + geom_abline(intercept = 0, slope = 1, colour = "red") +
  theme_bw() + theme(legend.position = "none")

mod_Pen_Aleatorio2_pred

## Coeficeintes del modelo 

(Coef_Estimado <- inner_join(
  coef(mod_Pen_Aleatorio2)$dam %>%
    tibble::rownames_to_column(var = "dam"),
  encuesta_plot %>% dplyr::select(dam, Area) %>% distinct()
))



(Coef_Estimado <- Coef_Estimado %>%  
   inner_join(media_dam, by = "dam")) %>% slice(1:6L)


(Coef_Estimado %<>%  mutate(B0 = ifelse(
Area == "1. Urbana", `(Intercept)` + mu.y * mu.x + `Area2. Rural`,
              `(Intercept)` + mu.y * mu.x)) %>%
  dplyr::select(dam, Area, B0, log_gasto))



plot_mod_Pen_Aleatorio212 <-ggplot(data = encuesta_plot,
       aes(y = log_ingreso , x = log_gasto,
           colour = dam)) +
  geom_jitter() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  facet_grid( ~ Area) +
  geom_abline(
    data = Coef_Estimado,
    mapping = aes(
      slope = log_gasto,
      intercept = B0,
      colour = dam
    )
  ) +
  theme_cepal()


#-------------------------------------------------------------------------------
# 5. Modelos logísticos multinivel 
#-------------------------------------------------------------------------------

# Gráfico base: relación entre pobreza y log del gasto per cápita
modl_logit1 <- ggplot(data = encuesta_hog,
                      aes(y = pobreza_LP, x = log_gasto)) +
  geom_point() +  # puntos observados
  geom_smooth(
    formula = y ~ x,
    method = "glm",  # ajuste con regresión logística
    se = FALSE,
    method.args = list(family = binomial(link = "logit"))
  ) +
  theme_bw()

# Función auxiliar para calcular probabilidades a partir del modelo logístico
auxLogit <- function(x, b0, b1) {
  1 / (1 + exp(-(b0 + b1 * x)))
}

# Estimación del intercepto (modelo nulo, sin predictores)
B0 <- coef(glm(pobreza_LP ~ 1, 
               data = encuesta_plot, 
               family = binomial(link = "logit")))

# Estimación por dominio del coeficiente de log_gasto (modelo sin intercepto)
coef_Mod <- encuesta_plot %>%
  group_by(dam) %>%
  summarise(
    B1 = coef(glm(pobreza_LP ~ -1 + log_gasto, 
                  family = binomial(link = "logit")))
  ) %>%
  mutate(B0 = B0)

# Vista preliminar de los primeros seis dominios
coef_Mod %>% slice(1:6L)

# Generación de valores de log_gasto para predicción
# y cálculo de las probabilidades estimadas por dominio
pred_logit <- coef_Mod %>%
  mutate(log_gasto = list(seq(0, 20, length = 100))) %>%
  tidyr::unnest_legacy()

# Aplicación de la función logística para cada dominio
pred_logit %<>% 
  mutate(Prob = auxLogit(log_gasto, B0, B1))

# Gráfico de las curvas de probabilidad por dominio
modl_logit2 <- ggplot(data = pred_logit,
                      aes(y = Prob, x = log_gasto, colour = dam)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "none")

# Mostrar gráfico
modl_logit2


# 5.1 Modelo logístico nulo
mod_logist_null  <- glmer( pobreza_LP  ~ ( 1  |  dam ),
                   data  =  encuesta_hog,
             weights  =  Factor2,
             family = binomial(link = "logit") )


coef( mod_logist_null )$dam


mod_logist_null

performance::icc(mod_logist_null)


## Predición del modelo 

(tab_pred <- data.frame(
  Pred = predict(mod_logist_null, type = "response"), 
  pobreza = encuesta_hog$pobreza_LP, 
  dam = encuesta_hog$dam)) %>% distinct() %>% 
  slice(1:6L) 

# Comparación de la predicción y el observado 
weighted.mean(encuesta_hog$pobreza_LP, encuesta_hog$Factor2)
weighted.mean(tab_pred$Pred, encuesta_hog$Factor2)

# 5.2 Modelo logístico con intercepto aleatorio

# Ajuste del modelo logístico con intercepto aleatorio por dominio (dam)
mod_logit_Int_Aleatorio <- glmer(
  pobreza_LP ~ log_gasto  + (1 | dam),
  data = encuesta_hog, family = binomial(link = "logit"),
  weights  =  Factor2)

# Cálculo del coeficiente de correlación intraclase (ICC)
performance::icc(mod_logit_Int_Aleatorio)

# Visualización de los primeros 10 coeficientes por dominio
coef(mod_logit_Int_Aleatorio)$dam %>% slice(1:10L)

# Generación de datos para predicción del modelo
dat_pred <- encuesta_hog %>% group_by(dam) %>%
  summarise(
    log_gasto = list(seq(min(log_gasto),
                         max(log_gasto), len = 100))) %>%
  tidyr::unnest_legacy()

# Predicción de probabilidades con el modelo ajustado
dat_pred <- mutate(dat_pred,
                   Proba = predict(mod_logit_Int_Aleatorio,
                                   newdata = dat_pred , type = "response"))

# Visualización de curvas de predicción por dominio
plot_mod_Int_Aleatorio02 <- ggplot(data = dat_pred,
                                   aes(y = Proba, x = log_gasto,
                                       colour = dam)) +
  geom_line()+   theme_bw() +
  geom_point(data = encuesta_hog, aes(y = pobreza_LP, x = log_gasto))+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

plot_mod_Int_Aleatorio02

# Tabla de predicciones individuales y observaciones
(tab_pred <- data.frame(
  Pred = predict(mod_logit_Int_Aleatorio,
                 type = "response"), 
  pobreza = encuesta_hog$pobreza_LP, 
  dam = encuesta_hog$dam, 
  Factor2 = encuesta_hog$Factor2)) %>% distinct() %>% 
  slice(1:6L) # Se muestran algunas predicciones

# Comparación entre promedio de predicciones y promedio observado
tab_pred %>% 
  summarise(Pred = weighted.mean(Pred, Factor2), 
            pobreza = weighted.mean(pobreza, Factor2))


# 5.3 Modelo logístico con intercepto y pendiente aleatoria

# Se ajusta un modelo logístico mixto donde tanto el intercepto como la pendiente
# de la variable log_gasto varían por dominio (dam).
mod_logit_Pen_Aleatorio <- glmer(
  pobreza_LP ~ log_gasto + (1 + log_gasto | dam),
  data = encuesta_hog,
  weights = Factor2,
  family = binomial(link = "logit")
)

# performance::icc(mod_logit_Pen_Aleatorio)
# (Opcional) Cálculo del coeficiente de correlación intraclase (ICC)

# Se construye una base de predicción para cada dominio (dam),
# generando 100 valores equiespaciados de log_gasto dentro del rango observado en cada dominio.
dat_pred <- encuesta_hog %>%
  group_by(dam) %>%
  summarise(
    log_gasto = list(seq(min(log_gasto), max(log_gasto), length.out = 100))
  ) %>%
  tidyr::unnest_legacy()

# Se estima la probabilidad de pobreza para cada combinación (dam, log_gasto)
# usando el modelo ajustado.
dat_pred <- mutate(dat_pred,
                   Proba = predict(mod_logit_Pen_Aleatorio,
                                   newdata = dat_pred, type = "response")
)

# Se grafica la probabilidad de pobreza frente al log_gasto por dominio.
# Se incluyen los datos observados como puntos para referencia.
plot_mod_Pen_Aleatorio02 <- ggplot(data = dat_pred,
                                   aes(y = Proba, x = log_gasto, colour = dam)) +
  geom_line() +
  theme_bw() +
  geom_point(data = encuesta_hog, aes(y = pobreza_LP, x = log_gasto)) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

# Visualización de la curva de predicción por dominio
plot_mod_Pen_Aleatorio02

# Se genera una tabla con las predicciones del modelo para las observaciones originales,
# junto con la pobreza observada, el dominio y los factores de expansión.
(tab_pred <- data.frame(
  Pred = predict(mod_logit_Pen_Aleatorio, type = "response"),
  pobreza = encuesta_hog$pobreza_LP,
  dam = encuesta_hog$dam,
  Factor2 = encuesta_hog$Factor2
)) %>%
  distinct() %>%
  slice(1:6L) # Muestra las primeras 6 filas distintas

# Se calcula la media ponderada de la pobreza predicha y observada usando los factores.
tab_pred %>%
  summarise(
    Pred = weighted.mean(Pred, Factor2),
    pobreza = weighted.mean(pobreza, Factor2)
  )


# 5.4 Modelo logístico más complejo con tenencia de vehículos

# Ajuste de un modelo logístico con efectos aleatorios por dominio (dam),
# incluyendo efectos aleatorios para el intercepto y las variables log_gasto,
# TIENEVEHICULOS y mu
mod_logit_Pen_Aleatorio2 <- glmer(
  pobreza_LP ~ 1 + log_gasto + TIENEVEHICULOS + mu +
    (1 + log_gasto + TIENEVEHICULOS + mu | dam),
  data = encuesta_hog,
  weights = Factor2,
  family = binomial(link = "logit")
)

# Generación de una base de datos para predicción
# Se agrupan las observaciones por dominio, tenencia de vehículos y mu
# y se genera una secuencia de valores de log_gasto para graficar las curvas de predicción
dat_pred <- encuesta_hog %>%
  group_by(dam, TIENEVEHICULOS, mu) %>%
  summarise(log_gasto = list(seq(min(log_gasto),
                                 max(log_gasto), length.out = 100))) %>%
  tidyr::unnest_legacy()

# Se calcula la probabilidad predicha usando el modelo ajustado
dat_pred$Proba <- predict(mod_logit_Pen_Aleatorio2,
                          newdata = dat_pred,
                          type = "response")

# Se construye una gráfica de las probabilidades predichas vs. log_gasto,
# separadas por tenencia de vehículos y coloreadas por dominio
plot_mod_Pen_Aleatorio002 <- ggplot(data = dat_pred,
                                    aes(y = Proba, x = log_gasto, colour = dam)) +
  geom_line() +
  theme_bw() +
  facet_grid(. ~ TIENEVEHICULOS) +
  geom_point(data = encuesta_hog, aes(y = pobreza_LP, x = log_gasto)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# Se visualiza la gráfica
plot_mod_Pen_Aleatorio002

# Se crea una tabla con las predicciones para cada hogar junto con las variables relevantes
tab_pred <- data.frame(
  Pred = predict(mod_logit_Pen_Aleatorio2, type = "response"),
  pobreza = encuesta_hog$pobreza_LP,
  dam = encuesta_hog$dam,
  TIENEVEHICULOS = encuesta_hog$TIENEVEHICULOS,
  Factor2 = encuesta_hog$Factor2
) %>%
  distinct() %>%
  slice(1:5L)  # Se visualizan las primeras 5 observaciones distintas

# Se resumen las predicciones agrupando por tenencia de vehículos,
# calculando la media ponderada de la predicción y la pobreza observada
tab_pred %>%
  group_by(TIENEVEHICULOS) %>%
  summarise(
    Pred = weighted.mean(Pred, Factor2),
    pobreza = weighted.mean(pobreza, Factor2)
  )
