# ==============================================================================
# Análisis de encuestas de hogares con R - Módulo 8: Métodos de Imputación
# CEPAL - Unidad de Estadísticas Sociales
# Autor: Andrés Gutiérrez y Stalyn Guerrero 
# Email: andres.gutierrez@cepal.org
# ==============================================================================

#-------------------------------------------------------------------------------
# 1. Configuración inicial y carga de datos
#-------------------------------------------------------------------------------

# Cargar librerías necesarias
library (survey)
library(convey)
library(TeachingSampling)
library(printr)
library(stargazer)
library(broom)
library(jtools)
library(modelsummary)
library(patchwork)
library(ggplot2)
library(stringr)
library(magrittr)
library(nnet)
library(tidyr)
rm(list = ls())


# Cargar la base de datos
encuesta <- readRDS("Slides/Imagenes/06_MLG1/ENIGH_HND_Hogar.rds")

# Preparación de los datos
encuesta <- encuesta %>% # Base de datos.
  transmute(
    LLAVE_HOGAR,
    F1_A0_UPM,
    estrato = haven::as_factor(F1_A0_ESTRATO),
    dam = haven::as_factor(F1_A0_DEPARTAMENTO),
    Area = haven::as_factor(F1_A0_AREA),
    ingreso_per  = ifelse(YDISPONIBLE_PER < 0 , 0 , YDISPONIBLE_PER) ,
    gasto_per = GASTO_CORRIENTE_HOGAR / CANTIDAD_PERSONAS,
    pobreza_LP = case_when(
      ingreso_per < 3046 & Area == "1. Urbana" ~ "1",
      ingreso_per < 1688 & Area == "2. Rural" ~ "1",
      TRUE ~ "0"
    ), 
     TIPOVIVIENDA = haven::as_factor(F1_A1_P1_TIPOVIVIENDA),
     TIENEVEHICULOS = haven::as_factor(F2_A2_P1_TIENEVEHICULOS),
     TECHOVIVIENDA = haven::as_factor(F1_A1_P4_TECHOVIVIENDA),
     log_ingreso_per = log(ingreso_per  + 500),
     log_gasto_per = log(gasto_per  + 500),
    Factor
  )

# Proporción de pobreza antes de generar valores faltantes
(tab_antes <- prop.table(table(encuesta$pobreza_LP)))
(med_antes <- mean(encuesta$log_gasto_per, na.rm = TRUE))


#-------------------------------------------------------------------------------
# 2. Creación de valores faltantes
#-------------------------------------------------------------------------------

# 2.1 Esquema MCAR (Missing Completely at Random)
set.seed(1234)
encuesta_MCAR <-  sample_frac(encuesta, 0.7 )
dat_plot <- bind_rows(
  list(encuesta_MCAR = encuesta_MCAR, 
       encuesta = encuesta), .id = "Caso"  )

# Visualización para esquema MCAR para el log. del ingreso 

p1 <- ggplot(dat_plot, aes(x=Area, y = log_ingreso_per)) +
  geom_boxplot() + facet_grid(.~Caso) + theme_bw()+
  geom_hline(yintercept = mean(encuesta$log_ingreso_per),
             col = "red")

p2 <- ggplot(dat_plot, aes(x=TIPOVIVIENDA, y = log_ingreso_per)) +
  geom_boxplot() + facet_grid(.~Caso) +theme_bw()+
  geom_hline(yintercept = mean(encuesta$log_ingreso_per),
             col = "red")

p0 <- p1|p2
p0

# Visualización para esquema MCAR para la densidad del log. del ingreso 

p1 <- ggplot(dat_plot, aes(x = log_ingreso_per, fill = Caso)) +
  geom_density(alpha = 0.3) + theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = mean(encuesta$log_ingreso_per),
             col = "red")

p2 <- ggplot(dat_plot, aes(x = log_ingreso_per, fill = Caso)) +
  geom_density(alpha = 0.3) + facet_grid(.~TIPOVIVIENDA) +
  theme_bw()+
  geom_vline(xintercept = mean(encuesta$log_ingreso_per),
             col = "red") +
  theme(legend.position = "none")
p0 <- (p1/p2)

p0



# Densidad del log del gasto per cápita por condición del dato (completo/incompleto)

p1 <- ggplot(dat_plot, aes(x = log_gasto_per, fill = Caso)) +
  geom_density(alpha = 0.3) +            # Curvas de densidad suavizadas
  theme_bw() +                           # Tema en blanco y negro
  theme(legend.position = "bottom") +    # Posición de la leyenda
  geom_vline(xintercept = mean(encuesta$log_gasto_per),  # Línea vertical en la media
             col = "red")                # Color rojo para la media

# Densidad del log del gasto per cápita por tipo de vivienda y condición del dato

p2 <- ggplot(dat_plot, aes(x = log_gasto_per, fill = Caso)) +
  geom_density(alpha = 0.3) +
  facet_grid(. ~ TIPOVIVIENDA) +         # Facetas por tipo de vivienda
  theme_bw() +
  geom_vline(xintercept = mean(encuesta$log_gasto_per), col = "red") +
  theme(legend.position = "none")        # Oculta la leyenda para ahorrar espacio

# Combinación vertical de p1 y p2
p0 <- (p1 / p2)
p0  # Muestra el gráfico combinado


# Densidad del log del ingreso per cápita por tipo de vivienda y tenencia de vehículo

p1 <- ggplot(dat_plot, aes(x = log_ingreso_per, fill = Caso)) +
  geom_density(alpha = 0.3) +
  facet_grid(TIPOVIVIENDA ~ TIENEVEHICULOS) +  # Facetas por 2 variables
  theme_bw() +
  geom_vline(xintercept = mean(encuesta$log_ingreso_per), col = "red") +
  theme(legend.position = "none")

p1  # Muestra el gráfico

# Densidad del log del gasto per cápita por tipo de vivienda y tenencia de vehículo
p2 <- ggplot(dat_plot, aes(x = log_gasto_per, fill = Caso)) +
  geom_density(alpha = 0.3) +
  facet_grid(TIPOVIVIENDA ~ TIENEVEHICULOS) +
  theme_bw() +
  geom_vline(xintercept = mean(encuesta$log_gasto_per), col = "red") +
  theme(legend.position = "none")

p2  # Muestra el gráfico


# 2.2 Esquema MAR (Missing at Random)

# Paso 1: Crear un identificador estrato concatenando 'dam' y 'Area' 
# para cada observación
temp_estrato <- paste0(encuesta$dam, encuesta$Area)

# Paso 2: Calcular el total de unidades (Nh) por estrato
temp_Nh <- as.data.frame(table(temp_estrato)) %>%
  rename(Nh = Freq) %>%               # Renombrar la frecuencia como Nh (tamaño poblacional del estrato)
  mutate(nh = ceiling(Nh * .1))       # Definir un tamaño muestral por estrato igual al 10% de Nh

# Paso 3: Verificar dimensiones y una muestra de los primeros estratos
dim(temp_Nh)                          # Número de estratos
head(temp_Nh, 16)                     # Visualizar los primeros 16 estratos

# Paso 4: Seleccionar una submuestra estratificada sin reemplazo (S.STSI)
set.seed(1234)                        # Fijar semilla para reproducibilidad
sel <- S.STSI(S = temp_estrato,      # Variable de estratificación
              Nh = temp_Nh$Nh,       # Tamaño total por estrato
              nh = temp_Nh$nh)       # Tamaño muestral por estrato (10%)

# Paso 5: Eliminar las observaciones seleccionadas para simular datos faltantes MAR
encuesta_MAR <- encuesta[-sel, ]     # MAR: los datos faltan al azar condicionado al estrato

# Paso 6: Preparar los datos para visualización comparativa
dat_plot2 <- bind_rows(
  list(encuesta_MAR = encuesta_MAR,  # Encuesta con valores faltantes simulados
       encuesta = encuesta),         # Encuesta original completa
  .id = "Caso")                      # Identificador del caso para diferenciar en el gráfico

# Paso 7: Visualizar la distribución del log del ingreso per cápita por Caso y otras variables
p1 <- ggplot(dat_plot2, aes(x = Caso, y = log_ingreso_per)) +
  geom_hline(yintercept = mean(encuesta$log_ingreso_per), col = "red") +  # Línea de media
  geom_boxplot() +
  facet_grid(Area ~ TIPOVIVIENDA) + 
  theme_bw()
p1

# Paso 8: Visualizar la distribución del log del gasto per cápita por Caso
p1 <- ggplot(dat_plot2, aes(x = Caso, y = log_gasto_per)) +
  geom_hline(yintercept = mean(encuesta$log_gasto_per), col = "red") +
  geom_boxplot() +
  facet_grid(Area ~ TIPOVIVIENDA) +
  theme_bw()
p1

# Paso 9: Comparar densidades de ingreso per cápita log-transformado
# Gráfico general por Caso
p1 <- ggplot(dat_plot2, aes(x = log_ingreso_per, fill = Caso)) +
  geom_density(alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = mean(encuesta$log_ingreso_per), col = "red")

# Gráfico por tipo de vivienda
p2 <- ggplot(dat_plot2, aes(x = log_ingreso_per, fill = Caso)) +
  facet_grid(. ~ TIPOVIVIENDA) +
  geom_density(alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_vline(xintercept = mean(encuesta$log_ingreso_per), col = "red")

# Combinar ambos gráficos verticalmente
p0 <- p1 / p2
p0


p1 <- ggplot(dat_plot2,
             aes(x = log_gasto_per, fill = Caso)) +
 geom_density(alpha = 0.3) + theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(
    xintercept = mean(encuesta$log_gasto_per),
             col = "red")

p2 <- ggplot(dat_plot2,
             aes(x = log_gasto_per, fill = Caso)) +
  facet_grid(.~TIPOVIVIENDA) +
  geom_density(alpha = 0.3) + theme_bw() +
  theme(legend.position = "none") +
  geom_vline(
    xintercept = mean(encuesta$log_gasto_per),
             col = "red")
p0 <- p1/p2

p0

# 2.3 Esquema MNAR (Not Missing at Random)


# Se define el tamaño de la muestra simulada con datos faltantes MNAR.
# En este caso, se seleccionará el 80% de los hogares con menor ingreso per cápita.
n <- ceiling(nrow(encuesta) * .80)

# Se ordenan los datos según el ingreso per cápita (log transformado)
# y se seleccionan los hogares más pobres (80% más bajo).

encuesta_MNAR <- encuesta %>%
  arrange(log_ingreso_per) %>%
  slice(1:n)

# Se crea un nuevo dataset para graficar las distribuciones comparativas,
# combinando la muestra original con la muestra MNAR.
dat_plot3 <- bind_rows(
  list(encuesta_MNAR = encuesta_MNAR,
       encuesta = encuesta),
  .id = "Caso"  # Etiqueta que indica si los datos provienen de la muestra completa o de la MNAR
)


# Densidad del ingreso per cápita (log)

p1 <- ggplot(dat_plot3, aes(x = log_ingreso_per, fill = Caso)) +
  geom_density(alpha = 0.2) +                # Curvas de densidad con transparencia
  theme_bw() +                               # Estilo de fondo blanco
  theme(legend.position = "bottom") +        # Leyenda en la parte inferior
  geom_vline(xintercept = mean(encuesta$log_ingreso_per),
             col = "red") +   # Línea roja: media de la muestra completa
  geom_vline(xintercept = mean(encuesta_MNAR$log_ingreso_per),
             col = "blue") # Línea azul: media de la muestra MNAR

p1  # Se muestra el gráfico



# Densidad del gasto per cápita (log)

p1 <- ggplot(dat_plot3, aes(x = log_gasto_per, fill = Caso)) +
  geom_density(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = mean(encuesta$log_gasto_per), col = "red") +
  geom_vline(xintercept = mean(encuesta_MNAR$log_gasto_per), col = "blue")

p1  # Se muestra el gráfico



# Boxplots del gasto per cápita según condición MNAR

p1 <- ggplot(dat_plot3, aes(x = Caso, y = log_gasto_per)) +
  geom_hline(yintercept = mean(encuesta$log_gasto_per),
             col = "red") + # Línea horizontal con la media general
  geom_boxplot() +                            # Boxplots comparando la distribución entre casos
  facet_grid(Area ~ TIPOVIVIENDA) +           # Facetas por área (urbano/rural) y tipo de vivienda
  theme_bw()

p1  # Se muestra el gráfico



# Unión de la base original con la muestra MNAR simulada
# Se renombran las variables que contendrán los valores "faltantes"
# (simulados bajo el mecanismo MNAR)

encuesta <- full_join(
  encuesta,
  encuesta_MNAR %>% 
    transmute(
      LLAVE_HOGAR,  # Clave del hogar usada para hacer el join
      pobreza_LP_missin = pobreza_LP,
      TIENEVEHICULOS_missin = TIENEVEHICULOS,
      TECHOVIVIENDA_missin = TECHOVIVIENDA,
      log_ingreso_per_missin = log_ingreso_per,
      log_gasto_per_missin = log_gasto_per
    )
)


# Proporción de valores faltantes en log_gasto_per_missin según área geográfica
encuesta %>%
  group_by(Area) %>%
  summarise(
    prop_NA_log_gasto = sum(is.na(log_gasto_per_missin)) / n()
  )

# Proporción de valores faltantes en log_gasto_per_missin según tenencia de vehículos
encuesta %>%
  group_by(TIENEVEHICULOS) %>%
  summarise(
    prop_NA_log_gasto = sum(is.na(log_gasto_per_missin)) / n()
  )

# Proporción de valores faltantes en log_gasto_per_missin según tipo de vivienda
encuesta %>%
  group_by(TIPOVIVIENDA) %>%
  summarise(
    prop_NA_log_gasto = sum(is.na(log_gasto_per_missin)) / n()
  )


#-------------------------------------------------------------------------------
# 3. Métodos de Imputación
#-------------------------------------------------------------------------------

# 3.1 Imputación por la media no condicional

promedio_ingreso <-
  mean(encuesta$log_ingreso_per_missin, na.rm = TRUE)
promedio_gasto <- mean(encuesta$log_gasto_per_missin, na.rm = TRUE)
encuesta %<>%
  mutate(
    log_ingreso_per_imp = ifelse(
      is.na(log_ingreso_per_missin),
      promedio_ingreso,
      log_ingreso_per_missin
    ),
    log_gasto_per_imp = ifelse(
      is.na(log_gasto_per_missin),
      promedio_gasto,
      log_gasto_per_missin
    )
  )
sum(is.na(encuesta$log_ingreso_per_imp))
sum(is.na(encuesta$log_gasto_per_imp))


## Ordenando la base para gráfica
dat_plot4 <- tidyr::gather(
  encuesta %>% dplyr::select(Area, TIPOVIVIENDA, log_ingreso_per, log_ingreso_per_imp),
  key = "Caso",
  value = "log_ingreso_per2",
  -Area,
  -TIPOVIVIENDA
)

p1 <- ggplot(dat_plot4, aes(x = log_ingreso_per2, fill = Caso)) +
  geom_density(alpha = 0.2) + theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = mean(encuesta$log_ingreso_per),
             col = "red") +
  geom_vline(xintercept = mean(encuesta$log_ingreso_per_imp),
             col = "blue")
p1


## Ordenando la base para gráfica

dat_plot4 <- tidyr::gather(
  encuesta %>% dplyr::select(Area,TIPOVIVIENDA,log_gasto_per,
                             log_gasto_per_imp),
  key = "Caso", value = "log_gasto_per2", -Area,-TIPOVIVIENDA)

p1 <- ggplot(dat_plot4, aes(x = log_gasto_per2, fill = Caso)) +
  geom_density(alpha = 0.2) + theme_bw() +
   theme(legend.position = "bottom") +
  geom_vline(
    xintercept = mean(encuesta$log_gasto_per),
             col = "red") +
  geom_vline(
    xintercept = mean(encuesta$log_gasto_per_imp),
             col = "blue")
 p1

# 3.2 Imputación por la media condicional

# Imputación del logaritmo del gasto per cápita:
# Se reemplazan los valores faltantes con la media condicional por estrato
 
 encuesta %<>% group_by(estrato) %>%
  mutate(
    log_gasto_per_imp = ifelse(is.na(log_gasto_per_missin),
     mean(log_gasto_per_missin, na.rm = TRUE),
     log_gasto_per_missin)) %>% data.frame()

# Verificación de valores NA restantes después de la imputación
sum(is.na(encuesta$log_gasto_per_imp))

# Imputación del logaritmo del ingreso per cápita:
# Igual procedimiento, por estrato

encuesta %<>% group_by(estrato) %>%
  mutate(
    log_ingreso_per_imp = ifelse(is.na(log_ingreso_per_missin),
     mean(log_ingreso_per_missin, na.rm = TRUE),
     log_ingreso_per_missin)) %>% data.frame()

# Verificación de valores NA restantes en log_ingreso_per_imp
sum(is.na(encuesta$log_ingreso_per_imp))


# Comparación de medias y desviaciones estándar antes y después de imputar
# Promedio y desviación estándar del ingreso original vs imputado (sin agrupar)
encuesta %>%
  summarise(
    log_ingreso = mean(log_ingreso_per),
    log_ingreso_sd = sd(log_ingreso_per),
    log_ingreso_imp = mean(log_ingreso_per_imp),
    log_ingreso_imp_sd = sd(log_ingreso_per_imp)
  )


# Comparación de log_gasto por área (urbano/rural) antes y después de la imputación
# Se calcula el sesgo relativo (BR) entre los promedios original e imputado
encuesta %>%
  group_by(Area) %>%
  summarise(
    log_gasto = mean(log_gasto_per),
    log_gasto_sd = sd(log_gasto_per),
    log_gasto_imp = mean(log_gasto_per_imp),
    log_gasto_imp_sd = sd(log_gasto_per_imp)
  ) %>%
  mutate(
    BR = 100 * (log_gasto - log_gasto_imp) / log_gasto
  )


# Comparación de log_gasto por tipo de vivienda, aplicando la misma lógica
encuesta %>%
  group_by(TIPOVIVIENDA) %>%
  summarise(
    log_gasto = mean(log_gasto_per),
    log_gasto_sd = sd(log_gasto_per),
    log_gasto_imp = mean(log_gasto_per_imp),
    log_gasto_imp_sd = sd(log_gasto_per_imp)
  ) %>%
  mutate(
    BR = 100 * (log_gasto - log_gasto_imp) / log_gasto
  )


## Ordenando la base para gráfica
dat_plot5 <- tidyr::gather(
  encuesta %>% dplyr::select(Area, TIPOVIVIENDA, log_gasto_per,
                             log_gasto_per_imp),
  key = "Caso",
  value = "log_gasto_per2",
  -Area,
  -TIPOVIVIENDA
)

p1 <- ggplot(dat_plot5, aes(x = log_gasto_per2, fill = Caso)) +
  geom_density(alpha = 0.2) + theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = mean(encuesta$log_gasto_per),
             col = "red") +
  geom_vline(xintercept = mean(encuesta$log_gasto_per_imp),
             col = "blue")
p1

p1 <- ggplot(dat_plot5, aes(x= Caso, y = log_gasto_per2)) +
   geom_hline(yintercept = mean(encuesta$log_gasto_per),
              col = "red") +  geom_boxplot() +
  facet_grid(Area~TIPOVIVIENDA) + theme_bw()
p1


# 3.3 Imputación por Hot-deck

# Identificar los índices de donantes (no NA) y receptores (NA) para gasto
donante <- which(!is.na(encuesta$log_gasto_per_missin))
receptor <- which(is.na(encuesta$log_gasto_per_missin))

# Inicializar la variable imputada con los valores observados
encuesta$log_gasto_per_imp <- encuesta$log_gasto_per_missin

# Fijar semilla para reproducibilidad
set.seed(1234)

# Imputación aleatoria tipo Hot-deck (sin condicionamiento):
# A cada receptor se le asigna un valor de un donante elegido al azar
for (ii in receptor) {
  don_ii <- sample(x = donante, size = 1)
  encuesta$log_gasto_per_imp[ii] <- encuesta$log_gasto_per_missin[don_ii]
}

# Verificación de que no queden valores NA imputables
sum(is.na(encuesta$log_gasto_per_imp))

# Comparación global entre valores originales e imputados

# Estadísticas resumen a nivel total
encuesta %>%
  summarise(
    log_gasto = mean(log_gasto_per),
    log_gasto_sd = sd(log_gasto_per),
    log_gasto_imp = mean(log_gasto_per_imp),
    log_gasto_imp_sd = sd(log_gasto_per_imp)
  ) %>%
  mutate(
    BR = 100 * (log_gasto - log_gasto_imp) / log_gasto
  )


# Comparación por área (urbano/rural)
encuesta %>%
  group_by(Area) %>%
  summarise(
    log_gasto = mean(log_gasto_per),
    log_gasto_sd = sd(log_gasto_per),
    log_gasto_imp = mean(log_gasto_per_imp),
    log_gasto_imp_sd = sd(log_gasto_per_imp)
  ) %>%
  mutate(
    BR = 100 * (log_gasto - log_gasto_imp) / log_gasto
  )


# Comparación por tipo de vivienda
encuesta %>%
  group_by(TIPOVIVIENDA) %>%
  summarise(
    log_gasto = mean(log_gasto_per),
    log_gasto_sd = sd(log_gasto_per),
    log_gasto_imp = mean(log_gasto_per_imp),
    log_gasto_imp_sd = sd(log_gasto_per_imp)
  ) %>%
  mutate(
    BR = 100 * (log_gasto - log_gasto_imp) / log_gasto
  )


# Visualización: comparación de las distribuciones originales e imputadas

# Reorganizar la base en formato largo para facilitar el uso con ggplot2
dat_plot6 <- tidyr::gather(
  encuesta %>% dplyr::select(Area, TIPOVIVIENDA, log_gasto_per, log_gasto_per_imp),
  key = "Caso", value = "log_gasto_per2", -Area, -TIPOVIVIENDA
)

# Densidad comparativa entre valores observados e imputados
p1 <- ggplot(dat_plot6, aes(x = log_gasto_per2, fill = Caso)) +
  geom_density(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = mean(encuesta$log_gasto_per), col = "red") +
  geom_vline(xintercept = mean(encuesta$log_gasto_per_imp), col = "blue")
p1

# Gráfico de cajas por área y tipo de vivienda para comparar distribuciones
p1 <- ggplot(dat_plot6, aes(x = Caso, y = log_gasto_per2)) +
  geom_hline(yintercept = mean(encuesta$log_gasto_per), col = "red") +
  geom_boxplot() +
  facet_grid(Area ~ TIPOVIVIENDA) +
  theme_bw()
p1


# Imputación por Hot-deck para variable categórica
# Variable: TIENEVEHICULOS


# Identificar donantes (no NA) y receptores (con NA)
donante <- which(!is.na(encuesta$log_gasto_per_missin))
receptor <- which(is.na(encuesta$log_gasto_per_missin))

# Inicializar la variable imputada con los valores observados
encuesta$TIENEVEHICULOS_imp <- encuesta$TIENEVEHICULOS_missin

# Calcular proporciones de las categorías observadas para imputación aleatoria
(prop <- prop.table(
  table(na.omit(encuesta$TIENEVEHICULOS_imp)))
)

# Fijar semilla para garantizar reproducibilidad
set.seed(1234)

# Realizar muestreo aleatorio con reposición según distribución observada
imp <- sample(
  size = length(receptor),
  x = c("1. Sí", "2. No"),
  prob = prop,
  replace = TRUE
)

# Asignar valores imputados a los hogares receptores
encuesta$TIENEVEHICULOS_imp[receptor] <- imp

# Verificación de que no queden valores faltantes
sum(is.na(encuesta$TIENEVEHICULOS_imp))

# Comparación de distribuciones antes y después de imputar
# Distribución marginal de la variable con NA (antes de imputar)

prop.table(
  table(encuesta$TIENEVEHICULOS_missin, useNA = "always")
)

# Distribución marginal de la variable imputada
prop.table(
  table(encuesta$TIENEVEHICULOS_imp, useNA = "always")
)


# Análisis de distribución conjunta por área
# Antes de imputar: distribución conjunta (Area x TIENEVEHICULOS_missin)
prop.table(
  table(encuesta$Area, encuesta$TIENEVEHICULOS_missin, useNA = "always")
) %>%
  addmargins() %>%
  data.frame() %>%
  tidyr::spread(key = "Var2", value = "Freq") %>%
  rename("Area" = Var1)


# Después de imputar: distribución conjunta (Area x TIENEVEHICULOS_imp)
prop.table(
  table(encuesta$Area, encuesta$TIENEVEHICULOS_imp, useNA = "always")
) %>%
  addmargins() %>%
  data.frame() %>%
  tidyr::spread(key = "Var2", value = "Freq") %>%
  rename("Area" = Var1)



# Análisis de distribución conjunta por tipo de vivienda
# Antes de imputar: distribución conjunta (TIPOVIVIENDA x TIENEVEHICULOS_missin)

prop.table(
  table(encuesta$TIPOVIVIENDA, encuesta$TIENEVEHICULOS_missin, useNA = "always")
) %>%
  addmargins() %>%
  data.frame() %>%
  tidyr::spread(key = "Var2", value = "Freq") %>%
  rename("TIPOVIVIENDA" = Var1)


# Después de imputar: distribución conjunta (TIPOVIVIENDA x TIENEVEHICULOS_imp)
prop.table(
  table(encuesta$TIPOVIVIENDA, encuesta$TIENEVEHICULOS_imp, useNA = "always")
) %>%
  addmargins() %>%
  data.frame() %>%
  tidyr::spread(key = "Var2", value = "Freq") %>%
  rename("TIPOVIVIENDA" = Var1)


# 3.4 Imputación por regresión

# Inicializar las variables imputadas con los valores faltantes incluidos
encuesta$log_gasto_per_imp <- encuesta$log_gasto_per_missin
encuesta$TECHOVIVIENDA_imp <- encuesta$TECHOVIVIENDA_missin

# Separar la base entre observaciones completas e incompletas para log_gasto
encuesta_obs    <- filter(encuesta, !is.na(log_gasto_per_missin))
encuesta_no_obs <- filter(encuesta,  is.na(log_gasto_per_missin))

# Modelos de imputación

# Modelo de regresión lineal para log_gasto_per
mod <- lm(log_gasto_per ~ Area + TIPOVIVIENDA + log_ingreso_per,
          data = encuesta_obs)

# Modelo multinomial para TECHOVIVIENDA
mod.mult <- multinom(TECHOVIVIENDA ~ Area + TIPOVIVIENDA + log_ingreso_per,
                     data = encuesta_obs, trace = FALSE)

# Predicción de valores imputados
imp      <- predict(mod, encuesta_no_obs)
imp.mult <- predict(mod.mult, encuesta_no_obs, type = "class")

# Asignación de los valores imputados
encuesta_no_obs$log_gasto_per_imp   <- imp
encuesta_no_obs$TECHOVIVIENDA_imp   <- imp.mult

# Reunir nuevamente la base completa
encuesta <- bind_rows(encuesta_obs, encuesta_no_obs)

# Análisis de la imputación (frecuencias)

# Comparación de proporciones de TECHOVIVIENDA antes y después de imputar
tab_missin <- prop.table(table(encuesta$TECHOVIVIENDA_missin, useNA = "a")) %>%
  data.frame() %>%
  rename(TECHOVIVIENDA = Var1, value_missin = Freq)

tab_imp <- prop.table(table(encuesta$TECHOVIVIENDA_imp, useNA = "a")) %>%
  data.frame() %>%
  rename(TECHOVIVIENDA = Var1, value_imp = Freq)

# Comparación conjunta
inner_join(tab_missin, tab_imp)

# Proporciones cruzadas por Area
prop.table(table(encuesta$TECHOVIVIENDA_missin, encuesta$Area, useNA = "a")) %>%
  addmargins() %>% data.frame() %>%
  tidyr::spread(key = "Var2", value = "Freq") %>%
  rename("TECHOVIVIENDA" = Var1)

prop.table(table(encuesta$TECHOVIVIENDA_imp, encuesta$Area, useNA = "a")) %>%
  addmargins() %>% data.frame() %>%
  tidyr::spread(key = "Var2", value = "Freq") %>%
  rename("TECHOVIVIENDA" = Var1)

# Proporciones cruzadas por TIPOVIVIENDA
prop.table(table(encuesta$TIPOVIVIENDA, encuesta$TECHOVIVIENDA_missin, useNA = "a")) %>%
  addmargins() %>% data.frame() %>%
  tidyr::spread(key = "Var2", value = "Freq") %>%
  rename("TIPOVIVIENDA" = Var1)

prop.table(table(encuesta$TIPOVIVIENDA, encuesta$TECHOVIVIENDA_imp, useNA = "a")) %>%
  addmargins() %>% data.frame() %>%
  tidyr::spread(key = "Var2", value = "Freq") %>%
  rename("TIPOVIVIENDA" = Var1)


# Evaluación del sesgo de imputación (BR) a nivel global y por subgrupos

# Global
encuesta %>%
  summarise(
    log_gasto       = mean(log_gasto_per),
    log_gasto_sd    = sd(log_gasto_per),
    log_gasto_imp   = mean(log_gasto_per_imp),
    log_gasto_imp_sd= sd(log_gasto_per_imp)
  ) %>%
  mutate(BR = 100 * (log_gasto - log_gasto_imp) / log_gasto)

# Por área
encuesta %>%
  group_by(Area) %>%
  summarise(
    log_gasto       = mean(log_gasto_per),
    log_gasto_sd    = sd(log_gasto_per),
    log_gasto_imp   = mean(log_gasto_per_imp),
    log_gasto_imp_sd= sd(log_gasto_per_imp)
  ) %>%
  mutate(BR = 100 * (log_gasto - log_gasto_imp) / log_gasto)

# Por tipo de vivienda
encuesta %>%
  group_by(TIPOVIVIENDA) %>%
  summarise(
    log_gasto       = mean(log_gasto_per),
    log_gasto_sd    = sd(log_gasto_per),
    log_gasto_imp   = mean(log_gasto_per_imp),
    log_gasto_imp_sd= sd(log_gasto_per_imp)
  ) %>%
  mutate(BR = 100 * (log_gasto - log_gasto_imp) / log_gasto)


# Visualización de los resultados de imputación

# Reorganizar los datos para graficar
dat_plot7 <- encuesta %>%
  select(Area, TIPOVIVIENDA, log_gasto_per, log_gasto_per_imp) %>%
  gather(key = "Caso", value = "log_gasto_per2", -Area, -TIPOVIVIENDA)

# Gráfico de densidades comparativas
p1 <- ggplot(dat_plot7, aes(x = log_gasto_per2, fill = Caso)) +
  geom_density(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = mean(encuesta$log_gasto_per), col = "red") +
  geom_vline(xintercept = mean(encuesta$log_gasto_per_imp), col = "blue")
p1

# Gráfico de cajas por grupo
p1 <- ggplot(dat_plot7, aes(x = Caso, y = log_gasto_per2)) +
  geom_hline(yintercept = mean(encuesta$log_gasto_per), col = "red") +
  geom_boxplot() +
  facet_grid(Area ~ TIPOVIVIENDA) +
  theme_bw()
p1



# 3.5 Imputación por el vecino más cercano

# Se copian los valores originales (con NA) a nuevas variables de imputación
encuesta$log_gasto_per_imp   <- encuesta$log_gasto_per_missin
encuesta$TECHOVIVIENDA_imp   <- encuesta$TECHOVIVIENDA_missin

# Se separa la base entre observaciones con datos y sin datos en log_gasto
encuesta_obs    <- filter(encuesta, !is.na(log_gasto_per_missin))
encuesta_no_obs <- filter(encuesta,  is.na(log_gasto_per_missin))

# Imputación determinística usando vecino más cercano

# Itera sobre cada observación sin dato
for(ii in 1:nrow(encuesta_no_obs)) {
  
  # Extrae el valor de referencia para el cual se va a buscar el vecino
  Expen_ii <- encuesta_no_obs$log_gasto_per[[ii]]
  
  # Identifica el índice del donante con valor más cercano (menor distancia absoluta)
  don_ii <- which.min(abs(Expen_ii - encuesta_obs$log_gasto_per))
  
  # Imputa el valor de log_gasto desde el vecino más cercano
  encuesta_no_obs$log_gasto_per_imp[[ii]] <- encuesta_obs$log_gasto_per_missin[[don_ii]]
  
  # Imputa el valor de TECHOVIVIENDA desde el mismo donante
  encuesta_no_obs$TECHOVIVIENDA_imp[[ii]]  <- encuesta_obs$TECHOVIVIENDA_missin[[don_ii]]
}

# Se reintegra la base imputada con la observada
encuesta <- bind_rows(encuesta_obs, encuesta_no_obs)

# Evaluación de distribución marginal antes y después de imputar

# Frecuencia marginal antes de imputar TECHOVIVIENDA
tab_missin <- prop.table(table(encuesta$TECHOVIVIENDA_missin, useNA = "a")) %>%
  data.frame() %>%
  rename(TECHOVIVIENDA = Var1, value_missin = Freq)

# Frecuencia marginal después de imputar
tab_imp <- prop.table(table(encuesta$TECHOVIVIENDA_imp, useNA = "a")) %>%
  data.frame() %>%
  rename(TECHOVIVIENDA = Var1, value_imp = Freq)

# Comparación
inner_join(tab_missin, tab_imp)

# Frecuencias condicionales antes y después de imputar por Area
prop.table(table(encuesta$TECHOVIVIENDA_missin, encuesta$Area, useNA = "a")) %>%
  addmargins() %>%
  data.frame() %>%
  tidyr::spread(key = "Var2", value = "Freq") %>%
  rename("TECHOVIVIENDA" = Var1)

prop.table(table(encuesta$TECHOVIVIENDA_imp, encuesta$Area, useNA = "a")) %>%
  addmargins() %>%
  data.frame() %>%
  tidyr::spread(key = "Var2", value = "Freq") %>%
  rename("TECHOVIVIENDA" = Var1)

# Frecuencias condicionales antes y después de imputar por TIPOVIVIENDA
prop.table(table(encuesta$TECHOVIVIENDA_missin, encuesta$TIPOVIVIENDA, useNA = "a")) %>%
  addmargins() %>%
  data.frame() %>%
  tidyr::spread(key = "Var2", value = "Freq") %>%
  rename("TECHOVIVIENDA" = Var1)

prop.table(table(encuesta$TECHOVIVIENDA_imp, encuesta$TIPOVIVIENDA, useNA = "a")) %>%
  addmargins() %>%
  data.frame() %>%
  tidyr::spread(key = "Var2", value = "Freq") %>%
  rename("TECHOVIVIENDA" = Var1)

# Cálculo del sesgo relativo (BR) a nivel global y por subgrupos

# Nivel global
encuesta %>%
  summarise(
    log_gasto        = mean(log_gasto_per),
    log_gasto_sd     = sd(log_gasto_per),
    log_gasto_imp    = mean(log_gasto_per_imp),
    log_gasto_imp_sd = sd(log_gasto_per_imp)
  ) %>%
  mutate(BR = 100 * (log_gasto - log_gasto_imp) / log_gasto)

# Por área
encuesta %>%
  group_by(Area) %>%
  summarise(
    log_gasto        = mean(log_gasto_per),
    log_gasto_sd     = sd(log_gasto_per),
    log_gasto_imp    = mean(log_gasto_per_imp),
    log_gasto_imp_sd = sd(log_gasto_per_imp)
  ) %>%
  mutate(BR = 100 * (log_gasto - log_gasto_imp) / log_gasto)

# Por tipo de vivienda
encuesta %>%
  group_by(TIPOVIVIENDA) %>%
  summarise(
    log_gasto        = mean(log_gasto_per),
    log_gasto_sd     = sd(log_gasto_per),
    log_gasto_imp    = mean(log_gasto_per_imp),
    log_gasto_imp_sd = sd(log_gasto_per_imp)
  ) %>%
  mutate(BR = 100 * (log_gasto - log_gasto_imp) / log_gasto)

# Visualización comparativa

# Reorganización para graficar distribuciones imputadas y observadas
dat_plot8 <- encuesta %>%
  select(Area, TIPOVIVIENDA, log_gasto_per, log_gasto_per_imp) %>%
  gather(key = "Caso", value = "log_gasto_per2", -Area, -TIPOVIVIENDA)

# Gráfico de densidad
p1 <- ggplot(dat_plot8, aes(x = log_gasto_per2, fill = Caso)) +
  geom_density(alpha = 0.2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = mean(encuesta$log_gasto_per), col = "red") +
  geom_vline(xintercept = mean(encuesta$log_gasto_per_imp), col = "blue")
p1

# Gráfico de cajas por grupo
p1 <- ggplot(dat_plot8, aes(x = Caso, y = log_gasto_per2)) +
  geom_hline(yintercept = mean(encuesta$log_gasto_per), col = "red") +
  geom_boxplot() +
  facet_grid(Area ~ TIPOVIVIENDA) +
  theme_bw()
p1


# Imputación de valores faltantes en las columnas

# 'log_gasto_per_imp' y 'TECHOVIVIENDA_imp'
encuesta$log_gasto_per_imp <- encuesta$log_gasto_per_missin
encuesta$TECHOVIVIENDA_imp <- encuesta$TECHOVIVIENDA_missin

# Filtrar observaciones con valores disponibles 
# en la variable 'log_gasto_per_missin'
encuesta_obs <- filter(encuesta,
                       !is.na(log_gasto_per_missin))

# Filtrar observaciones con valores faltantes 
# en la variable 'log_gasto_per_missin'
encuesta_no_obs <- filter(encuesta,
                          is.na(log_gasto_per_missin))

# Ajuste de un modelo de regresión lineal utilizando las observaciones con 'log_gasto_per_missin' disponibles
mod <- lm(log_gasto_per ~ Area + TIENEVEHICULOS + log_ingreso_per  ,
          data = encuesta_obs)

# Predicciones para las observaciones con 'log_gasto_per_missin' 
# disponibles y sin valores
pred_Obs <- predict(mod, encuesta_obs)
pred_no_Obs <- predict(mod, encuesta_no_obs)

# Imputación de valores faltantes utilizando el vecino
# más cercano en las predicciones
for (ii in 1:nrow(encuesta_no_obs)) {
  don_ii <- which.min(abs(pred_no_Obs[ii] - pred_Obs))
  encuesta_no_obs$log_gasto_per_imp[[ii]] <-
    encuesta_obs$log_gasto_per_missin[[don_ii]]
  encuesta_no_obs$TECHOVIVIENDA_imp[[ii]] <-
    encuesta_obs$TECHOVIVIENDA_missin[[don_ii]]
}

# Combinar las observaciones imputadas con las observaciones originales
encuesta <- bind_rows(encuesta_obs, encuesta_no_obs)

# Evaluación de distribución marginal antes y después de imputar

# Frecuencia marginal antes de imputar TECHOVIVIENDA

tab_missin <- prop.table(
  table(encuesta$TECHOVIVIENDA_missin, useNA = "a")) %>% data.frame() %>%
  rename(TECHOVIVIENDA = Var1, 
        value_missin =Freq)

# Frecuencia marginal después de imputar

tab_imp <-  prop.table(
  table(encuesta$TECHOVIVIENDA_imp, useNA = "a")) %>% data.frame() %>%
  rename(TECHOVIVIENDA = Var1, 
        value_imp = Freq)

# Comparación
inner_join(tab_missin, tab_imp)

# Frecuencias condicionales antes y después de imputar por Area

prop.table( table(encuesta$TECHOVIVIENDA_missin, encuesta$Area, 
        useNA = "a")) %>% addmargins() %>% data.frame() %>% 
  tidyr::spread(key = "Var2",value = "Freq") %>% 
  rename("TECHOVIVIENDA" = Var1 )



prop.table( table(encuesta$TECHOVIVIENDA_imp,encuesta$Area,
        useNA = "a")) %>% addmargins()%>% data.frame() %>% 
  tidyr::spread(key = "Var2",value = "Freq") %>% 
  rename("TECHOVIVIENDA" = Var1 ) 


# Frecuencias condicionales antes y después de imputar por TIPOVIVIENDA
prop.table( table(encuesta$TECHOVIVIENDA_missin,  encuesta$TIPOVIVIENDA, 
        useNA = "a")) %>% addmargins()%>% data.frame() %>% 
  tidyr::spread(key = "Var2",value = "Freq") %>% 
  rename("TIPOVIVIENDA" = Var1 )



prop.table( table(encuesta$TECHOVIVIENDA_imp, encuesta$TIPOVIVIENDA, 
        useNA = "a")) %>% addmargins()%>% data.frame() %>% 
  tidyr::spread(key = "Var2",value = "Freq") %>% 
  rename("TIPOVIVIENDA" = Var1 ) 

# Cálculo del sesgo relativo (BR) a nivel global y por subgrupos

# Nivel global
encuesta %>% summarise(
  log_gasto = mean(log_gasto_per),
  log_gasto_sd = sd(log_gasto_per),
  log_gasto_imp = mean(log_gasto_per_imp),
  log_gasto_imp_sd = sd(log_gasto_per_imp))%>% 
  mutate(BR = 100*(log_gasto - log_gasto_imp)/log_gasto)

# Por área
encuesta %>%group_by(Area) %>% summarise(
  log_gasto = mean(log_gasto_per),
  log_gasto_sd = sd(log_gasto_per),
  log_gasto_imp = mean(log_gasto_per_imp),
  log_gasto_imp_sd = sd(log_gasto_per_imp))%>% 
  mutate(BR = 100*(log_gasto - log_gasto_imp)/log_gasto)

# Por tipo de vivienda

encuesta %>%group_by(TIPOVIVIENDA) %>% summarise(
  log_gasto = mean(log_gasto_per),
  log_gasto_sd = sd(log_gasto_per),
  log_gasto_imp = mean(log_gasto_per_imp),
  log_gasto_imp_sd = sd(log_gasto_per_imp))%>% 
  mutate(BR = 100*(log_gasto - log_gasto_imp)/log_gasto)


# Visualización comparativa

# Reorganización para graficar distribuciones imputadas y observadas

dat_plot9 <- tidyr::gather(
  encuesta %>% dplyr::select(Area, TIPOVIVIENDA, log_gasto_per,
                             log_gasto_per_imp),
  key = "Caso",
  value = "log_gasto_per2",
  -Area,
  -TIPOVIVIENDA
)

# Gráfico de densidad

p1 <- ggplot(dat_plot9, aes(x = log_gasto_per2, fill = Caso)) +
  geom_density(alpha = 0.2) + theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = mean(encuesta$log_gasto_per),
             col = "red") +
  geom_vline(xintercept = mean(encuesta$log_gasto_per_imp),
             col = "blue")
p1

# Gráfico de cajas por grupo

p1 <- ggplot(dat_plot9, aes(x= Caso, y = log_gasto_per2)) +
   geom_hline(yintercept = mean(encuesta$log_gasto_per),
              col = "red") +  geom_boxplot() +
  facet_grid(Area~TIPOVIVIENDA) + theme_bw()
p1


#-------------------------------------------------------------------------------
# 4. Imputación Múltiple
#-------------------------------------------------------------------------------

## Simular población 
generar <- function(n = 500, n_0 = 200, 
                    beta = 10, sigma = 2){
  x <- runif(n)
  mu <- beta * x
  y <- mu + rnorm(n, mean = 0, sd = sigma)
  datos <- data.frame(x = x, y = y)
  faltantes <- sample(n, n_0)
  datos$faltantes <- "No"
  datos$faltantes[faltantes] <- "Si"
  datos$y.per <- y
  datos$y.per[faltantes] <- NA
  return(datos)
}

## Creando una población 
set.seed(1234)
datos <- generar()
head(datos,12)

## scaterplot 

p1 <- ggplot(data = datos, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(formula = y ~ x , method = "lm")

p2 <- ggplot(data = datos, aes(x = x, y = y.per)) +
  geom_point() +
  geom_smooth(formula = y ~ x , method = "lm")

p1 | p1

# 4.1 Imputación Bootstrap

im.bootstrap <- function(datos, M = 15){
  library(dplyr)
  n <- nrow(datos)
  datos1 <- na.omit(datos)
  n1 <- nrow(datos1)
  n0 <- n - n1
  Ind <- is.na(datos$y.per)
  faltantes.boot <- NULL
  beta1 <- NULL
  sigma1 <- NULL
  
  for (m in 1:M){
    datos.m <- dplyr::sample_n(datos1, n1, replace = TRUE)
    model1 <- lm(y ~ 0 + x, data = datos.m)
    beta <- model1$coeff
    sigma <- sqrt(anova(model1)[["Mean Sq"]][2])
    faltantes.boot <- rnorm(n0, datos$x[Ind] * beta, sd = sigma)
    datos$y.per[Ind] <-  faltantes.boot
    model.input <- lm(y.per ~ 0 + x, data = datos)
    beta1[m] <- model.input$coeff
    sigma1[m] <- summary(model.input)$coeff[2]
  }
  beta.input <- mean(beta1)
  u.bar <- mean(sigma1 ^ 2)
  B <- var(beta1)
  beta.sd <- sqrt(u.bar + B + B/M)
  result <- list(new = datos, beta = beta.input, sd = beta.sd)
}


set.seed(1234)
datos <- generar()
im.bootstrap(datos)$beta
im.bootstrap(datos)$sd
head(im.bootstrap(datos)$new, 4)


nuevos <- im.bootstrap(datos)$new
ggplot(data = nuevos, aes(x = x, y = y.per, color = faltantes)) +
  geom_point()

# 4.2 Aplicación en la encuesta: Imputación Múltiple por Bootstrap

encuesta$log_gasto_per_imp   <- encuesta$log_gasto_per_missin
encuesta$TECHOVIVIENDA_imp   <- encuesta$TECHOVIVIENDA_missin

# Separar la base en observados y no observados
encuesta_obs     <- filter(encuesta, !is.na(log_gasto_per_missin))
encuesta_no_obs  <- filter(encuesta,  is.na(log_gasto_per_missin))

# Número de observaciones en cada grupo
n0 <- nrow(encuesta_no_obs)  # No observados
n1 <- nrow(encuesta_obs)     # Observados

# Número de imputaciones
M <- 10
set.seed(1234)

# Bucle de imputación múltiple
for (ii in 1:M) {
  vp  <- paste0("log_gasto_per_vp_", ii)
  vp2 <- paste0("TECHOVIVIENDA_vp_", ii)
  
  # Bootstrap sobre la muestra observada
  encuesta_temp <- dplyr::sample_n(encuesta_obs, size = n1, replace = TRUE)
  
  # Modelo para variable continua
  mod <- lm(log_gasto_per ~ Area + TIPOVIVIENDA + log_ingreso_per,
            data = encuesta_temp)
  
  # Modelo para variable categórica
  mod.mult <- nnet::multinom(TECHOVIVIENDA ~ Area + TIPOVIVIENDA + log_ingreso_per,
                             data = encuesta_temp)
  
  # Predicciones para casos no observados
  encuesta_no_obs[[vp]]  <- predict(mod, encuesta_no_obs)
  encuesta_obs[[vp]]     <- encuesta_obs$log_gasto_per  # Valor real
  
  encuesta_no_obs[[vp2]] <- predict(mod.mult, encuesta_no_obs, type = "class")
  encuesta_obs[[vp2]]    <- encuesta_obs$TECHOVIVIENDA  # Valor real
}

# Verificación rápida de imputaciones (muestra 10 filas y 4 columnas)
dplyr::select(encuesta_no_obs, 
              log_gasto_per, matches("log_gasto_per_vp_"))[1:10, 1:4]

# Reconstrucción de la base
encuesta <- bind_rows(encuesta_obs, encuesta_no_obs)

# Gráfico de densidad para comparar imputaciones de log_gasto_per

# Reestructuración de la base para graficar
dat_plot10 <- tidyr::gather(
  encuesta %>% dplyr::select(Area, TIPOVIVIENDA, 
                             matches("log_gasto_per_vp_")),
  key = "Caso", value = "log_gasto_per2", -Area, -TIPOVIVIENDA)

# Gráfico de densidades imputadas vs. observadas
p1 <- ggplot2::ggplot(dat_plot10, ggplot2::aes(x = log_gasto_per2, col = Caso)) +
  ggplot2::geom_density(alpha = 0.2) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::geom_density(data = encuesta, 
                        aes(x = log_gasto_per),
                        col = "black", size = 1.2)

# Mostrar gráfico
p1






## Ordenando la base para gráfica
dat_plot11 <- tidyr::gather(
  encuesta %>%
  dplyr::select(Area,TIPOVIVIENDA, TECHOVIVIENDA,
                matches("TECHOVIVIENDA_vp_")),
  key = "Caso", value = "TECHOVIVIENDA2", -Area,-TIPOVIVIENDA) %>%
  group_by(Caso,TECHOVIVIENDA2) %>% tally() %>%
  group_by(Caso) %>% mutate(prop = n/sum(n))

p1 <- ggplot(dat_plot11,
        aes(x = TECHOVIVIENDA2, y = prop,
            fill = Caso, color="red")) +
       geom_bar(stat="identity",
          position = position_dodge(width = 0.5))  +
   theme_bw() +
   theme(legend.position = "bottom") +
  scale_fill_manual(values = c("TECHOVIVIENDA" = "black"))
p1

# -----------------------------------------------------------------------------
# 5. Análisis con Diseño Muestral
# -----------------------------------------------------------------------------

# Definir el diseño muestral complejo utilizando la librería 'survey'

diseno <- encuesta %>% as_survey_design(
  strata = estrato,     # Estratos definidos en el diseño
  ids = F1_A0_UPM,       # Unidades primarias de muestreo (UPM)
  weights = Factor,      # Factor de expansión (ponderadores)
  nest = TRUE            # Anidamiento dentro de los estratos
)


# Estimación de medias por imputación (para 10 imputaciones múltiples)

estimacion_vp <- diseno %>% 
  summarise(
    vp1  = survey_mean(log_gasto_per_vp_1, vartype = "var"),
    vp2  = survey_mean(log_gasto_per_vp_2, vartype = "var"),
    vp3  = survey_mean(log_gasto_per_vp_3, vartype = "var"),
    vp4  = survey_mean(log_gasto_per_vp_4, vartype = "var"),
    vp5  = survey_mean(log_gasto_per_vp_5, vartype = "var"),
    vp6  = survey_mean(log_gasto_per_vp_6, vartype = "var"),
    vp7  = survey_mean(log_gasto_per_vp_7, vartype = "var"),
    vp8  = survey_mean(log_gasto_per_vp_8, vartype = "var"),
    vp9  = survey_mean(log_gasto_per_vp_9, vartype = "var"),
    vp10 = survey_mean(log_gasto_per_vp_10, vartype = "var")
  )

# Reestructuración de resultados para combinar estimaciones y varianzas

estimacion_vp <- estimacion_vp %>%
  tidyr::gather() %>%
  tidyr::separate(key, c("vp", "estimacion")) %>%
  dplyr::mutate(estimacion = ifelse(is.na(estimacion), "promedio", "var")) %>%
  tidyr::spread(estimacion, value) %>%
  dplyr::mutate(vp = 1:10)


# Combinación de estimaciones usando la regla de Rubin (para la media)

Media_vp <- mean(estimacion_vp$promedio)           # Media combinada
Ubar     <- mean(estimacion_vp$var)                # Varianza media (intra-imputación)
B        <- var(estimacion_vp$promedio)            # Varianza entre imputaciones
var_vp   <- Ubar + (1 + 1/M) * B                   # Varianza total según Rubin

# Resultado combinado
resultado <- data.frame(
  Media_vp = Media_vp,
  Media_vp_se = sqrt(var_vp)
)


# Validación adicional: estimación de varianzas directamente

estimacion_var_vp <- diseno %>%
  summarise_at(vars(matches("log_gasto_per_vp")),
               survey_var, vartype = "var")

# Reestructurar para aplicar fórmula de Rubin
estimacion_var_vp <- estimacion_var_vp %>%
  tidyr::gather() %>%
  tidyr::separate(key, c("A", "B", "C", "D", "vp", "estimacion")) %>%
  dplyr::transmute(
    estimacion = ifelse(is.na(estimacion), "promedio", "var"),
    vp = as.numeric(vp),
    value
  ) %>%
  tidyr::spread(estimacion, value)

# Aplicación de Rubin sobre las varianzas
Media_var_vp <- mean(estimacion_var_vp$promedio)
Ubar         <- mean(estimacion_var_vp$var)
B            <- var(estimacion_var_vp$promedio)
var_var_vp   <- Ubar + (1 + 1/M) * B

# Agregar a tabla de resultados
resultado$var_vp     <- Media_var_vp
resultado$var_vp_se  <- sqrt(var_var_vp)


# Estimación directa sin imputación para comparar

diseno %>% summarise(
  Media = survey_mean(log_gasto_per),
  Var   = survey_var(log_gasto_per)
)

# Mostrar resultados combinados
resultado


# Estimación de proporciones para variable categórica imputada

# Estimaciones por imputación para TECHOVIVIENDA
estimacion_prop_vp <- 
  lapply(paste0("TECHOVIVIENDA_vp_", 1:10), function(vp) {
    diseno %>%
      group_by_at(vars(TECHOVIVIENDA = vp)) %>%
      summarise(prop = survey_mean(vartype = "var"),
                .groups = "drop") %>%
      mutate(vp = vp)
  }) %>%
  bind_rows()

# Procesamiento de los nombres de las variables
estimacion_prop_vp <- estimacion_prop_vp %>%
  tidyr::separate(vp, c("A", "B", "vp")) %>%
  dplyr::transmute(
    TECHOVIVIENDA,
    vp = as.numeric(vp),
    prop,
    prop_var
  )

# Filtrado para un nivel específico (opcional)
estimacion_prop_vp %>%
  dplyr::filter(TECHOVIVIENDA == "1. Teja de barro")


# Regla de Rubin para estimaciones de proporción

resultado <- estimacion_prop_vp %>%
  dplyr::group_by(TECHOVIVIENDA) %>%
  dplyr::summarise(
    prop_pv = mean(prop),
    Ubar = mean(prop_var),
    B = var(prop)
  ) %>%
  dplyr::mutate(prop_pv_var = Ubar + (1 + 1/M) * B)


# Estimación directa de proporciones para comparación

diseno %>% 
  dplyr::group_by(TECHOVIVIENDA) %>%
  summarise(prop = survey_mean(vartype = "var"))


# Mostrar resultado final
resultado

