# ==============================================================================
# Análisis de encuestas de hogares con R - Módulo 5: Análisis gráfico en R
# CEPAL - Unidad de Estadísticas Sociales
# Autor: Andrés Gutiérrez y Stalyn Guerrero 
# Email: andres.gutierrez@cepal.org
# ==============================================================================


#-------------------------------------------------------------------------------
# Cargar librerías necesarias
#-------------------------------------------------------------------------------
library(haven)
library(dplyr)
library(srvyr)
library(ggplot2)
library(patchwork)
library(tmap)


#-------------------------------------------------------------------------------
# 1. Lectura y preparación de datos
#-------------------------------------------------------------------------------
# Leer la base de datos
encuesta <-
  readRDS("Slides/Imagenes/02_variable_continua/ENIGH_HND_Hogar.rds")

# Transformar variables a factores

encuesta <- encuesta %>% # Base de datos.
  mutate(
    estrato = haven::as_factor(F1_A0_ESTRATO),
    TIPOVIVIENDA = haven::as_factor(F1_A1_P1_TIPOVIVIENDA),
    Area = haven::as_factor(F1_A0_AREA),
    TIENEVEHICULOS = haven::as_factor(F2_A2_P1_TIENEVEHICULOS)
  )

# Definir diseño muestral con srvyr
diseno <- encuesta %>%
  as_survey_design(
    strata = estrato,    # Id de los estratos
    ids = F1_A0_UPM,     # Id para las observaciones
    weights = Factor,    # Factores de expansión
    nest = TRUE          # Valida el anidado dentro del estrato
  )

# Crear nuevas variables
diseno <- diseno %>%
  mutate(
    ingreso_per = ifelse(YDISPONIBLE_PER < 0, 0, YDISPONIBLE_PER),
    pobreza_LP = case_when(
      ingreso_per < 3046 & Area == "1. Urbana" ~ 1,
      ingreso_per < 1688 & Area == "2. Rural" ~ 1,
      TRUE ~ 0
    ),
    pobreza_LI = case_when(
      ingreso_per < 1955 & Area == "1. Urbana" ~ 1,
      ingreso_per < 1110 & Area == "2. Rural" ~ 1,
      TRUE ~ 0
    ),
    ingreso_hog = ingreso_per * CANTIDAD_PERSONAS,
    log_ingreso_per = log(ingreso_per + 500),
    log_ingreso_hog = log(ingreso_hog + 500),
    log_gasto = log(`GASTO_CORRIENTE_HOGAR` + 500)
  )

## Variables creadas 
diseno$variable %>% # Base de datos.
  dplyr::select(CANTIDAD_PERSONAS, 
         log_gasto, 
         pobreza_LI,
         pobreza_LP,
         ingreso_per, 
         ingreso_hog) %>% arrange(-ingreso_hog) %>% head(20)


# Crear subgrupos por área
sub_Urbano <- diseno %>%  filter(Area == "1. Urbana") # 
sub_Rural  <- diseno %>%  filter(Area == "2. Rural") # 


#-------------------------------------------------------------------------------
# 2. Definición de tema gráfico
#-------------------------------------------------------------------------------

theme_cepal <- function(...) {
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
    )
}

# Definir paletas de colores
colorArea <- c("1. Urbana" = "#48C9B0", "2. Rural" = "#117864")
colorVehiculo <- c("1. Sí" = "#5DADE2", "2. No" = "#2874A6")
colorvivienda <- c(
  "1. Casa" = "#D6EAF8",
  "2. Apartamento" = "#85C1E9",
  "4. Local no construido para vivienda" = "#3498DB",
  "3. Cuarto en mesón o cuartería" = "#2E86C1",
  "5. Otro, especifique" = "#21618C"
)


#-------------------------------------------------------------------------------
# 3. Gráficas de variables continuas (Histogramas)
#-------------------------------------------------------------------------------
# Histograma básico para log_ingreso_hog (ponderado y sin ponderar)

plot1_Ponde <- ggplot(data = diseno$variables,              # Fuente de datos.
                      aes(x = log_ingreso_hog,
                          weight = Factor)  # Parámetros gráficos general.
                      ) +
                      geom_histogram(# Parámetro geométrico.
                        aes(y = ..density..)) +     # Parámetros del gráfico
                        ylab("") +                    # Nombre para el eje Y
                        ggtitle("Ponderado") +        # Titulo.
                        theme_cepal()                 # Aplicando tema
                      

plot1_SinPonde <-
  ggplot(data = diseno$variables,
         aes(x = log_ingreso_hog)) +
  geom_histogram(aes(y = ..density..)) +
  ylab("") +
  ggtitle("Sin ponderar") +
  theme_cepal()

# Combinar gráficas
plot1 <-  plot1_Ponde | plot1_SinPonde
plot1

# Histograma para log_gasto (ponderado y sin ponderar)

plot2_Ponde <- ggplot(data =  diseno$variables,
                      aes(x = log_gasto  , weight = Factor)) +
  geom_histogram(aes(y = ..density..)) +
  ylab("") +
  ggtitle("Ponderado") +
  theme_cepal()

plot2_SinPonde <- ggplot(data = diseno$variables,
                         aes(x = log_gasto)) +
  geom_histogram(aes(y = ..density..)) +
  ylab("") +
  ggtitle("Sin ponderar") +
  theme_cepal()

plot2 <-  plot2_Ponde | plot2_SinPonde

plot2

# Histogramas por subgrupos (Área) para el log_ingreso  (ponderado y sin ponderar)



plot3_Ponde <- ggplot(
  diseno$variables,
  aes(x = log_ingreso_hog , weight = Factor)
) +
  geom_histogram(
    aes(y = ..density.., fill = Area),
    alpha = 0.5,
    position = "identity" # Para que las barras no estén apiladas.
  ) +
  ylab("") +
  ggtitle("Ponderado") +
  theme_cepal()

plot3_SinPonde <-
  ggplot(diseno$variables, aes(x = log_ingreso_hog )) +
  geom_histogram(aes(y = ..density.., fill = Area),
    alpha = 0.5, position = "identity"
  ) +
  ggtitle("Sin ponderar") +
  theme_cepal() +
  ylab("")
plot3 <-  plot3_Ponde | plot3_SinPonde

plot3

# Histogramas por subgrupos (Área) para el log_gasto  (ponderado y sin ponderar)


plot4_Ponde <- ggplot(diseno$variables,
                      aes(x = log_gasto, weight = Factor)) +
  geom_histogram(aes(y = ..density.., fill = Area),
                 alpha = 0.5,
                 position = "identity") +
  ylab("") +
  ggtitle("Ponderado") +
  theme_cepal()



plot4_SinPonde <- ggplot(diseno$variables,
                         aes(x = log_gasto)) +
  geom_histogram(aes(y = ..density.., fill = Area),
                 alpha = 0.5,
                 position = "identity") +
  ggtitle("Sin ponderar") +
  theme_cepal() +
  ylab("")

plot4 <-  plot4_Ponde | plot4_SinPonde

plot4
 
# Histogramas por subgrupos (TIENEVEHICULOS) para el log_ingreso_hog
# (ponderado y sin ponderar)

plot5_Ponde <-
  ggplot(diseno$variables,
         aes(x = log_ingreso_hog, weight = Factor)) +
  geom_histogram(
    aes(y = after_stat(density), fill = TIENEVEHICULOS),
                 alpha = 0.5,
                 position = "identity") +
  ylab("") +
  ggtitle("Ponderado") +
  theme_cepal()


plot5_SinPonde <- ggplot(diseno$variables,
         aes(x = log_ingreso_hog)) +
  geom_histogram(
    aes(y = after_stat(density), fill = TIENEVEHICULOS),
                 alpha = 0.5,
                 position = "identity") +
  ggtitle("Sin ponderar") +
  theme_cepal() +
  ylab("")

plot5 <-   plot5_Ponde | plot5_SinPonde
plot5

# Histogramas por subgrupos (TIENEVEHICULOS) para el log_gasto
# (ponderado y sin ponderar)

plot6_Ponde <- ggplot(diseno$variables,
                      aes(x = log_gasto, weight = Factor)) +
  geom_histogram(
    aes(y = after_stat(density),
        fill = TIENEVEHICULOS),
        alpha = 0.5,
        position = "identity") +
  ylab("") +
  ggtitle("Ponderado") +
  theme_cepal()


plot6_SinPonde <- ggplot(diseno$variables,
                      aes(x = log_gasto)) +
  geom_histogram(
    aes(y = after_stat(density),
        fill = TIENEVEHICULOS),
        alpha = 0.5,
        position = "identity") +
  ylab("")  +
  ggtitle("Sin ponderar") +
  theme_cepal() +
  ylab("")
plot6 <-  plot6_Ponde | plot6_SinPonde

# Agregar densidad a los histogramas
plot1_desy <-
  plot1_Ponde + geom_density(fill = "#ADD8E6", alpha = 0.3)  |
  plot2_Ponde + geom_density(fill = "#ADD8E6", alpha = 0.3)
plot1_desy


plot3_densy <-
  plot3_Ponde + geom_density(aes(fill = Area), alpha = 0.3) |
  plot4_Ponde + geom_density(aes(fill = Area), alpha = 0.3)

plot3_densy


plot5_densy <- plot5_Ponde +
  geom_density(aes(fill = TIENEVEHICULOS),
               alpha = 0.3) |
  plot6_Ponde + geom_density(aes(fill = TIENEVEHICULOS),
                             alpha = 0.3)

plot5_densy

# ------------------------------------------------------------------------------
# 4. Boxplots
# ------------------------------------------------------------------------------

# Boxplot básico para log_ingreso_hog y log_gasto

plot7_Ponde <- ggplot(  diseno$variables,
                        aes(x = log_ingreso_hog,
                            weight = Factor)) +
  geom_boxplot() +  ggtitle("Ponderado") +
  coord_flip() +   theme_cepal()

plot8_Ponde <- ggplot( diseno$variables,
  aes(x = log_gasto, weight = Factor)
) + geom_boxplot() + ggtitle("Ponderado") + coord_flip() +
  theme_cepal()

plot_78 <- plot7_Ponde | plot8_Ponde
plot_78

# Boxplot por área

plot9_Ponde <- ggplot(diseno$variables,
                      aes(x = log_ingreso_hog,
                          weight = Factor)) +
  geom_boxplot(aes(fill = Area)) +  ggtitle("Ponderado") +
  coord_flip() +   theme_cepal()

plot10_Ponde <- ggplot(diseno$variables,
                       aes(x = log_gasto, weight = Factor)) +
  geom_boxplot(aes(fill = Area)) + ggtitle("Ponderado") +
  coord_flip() +  theme_cepal()

plot910_Ponde <- plot9_Ponde | plot10_Ponde
plot910_Ponde

## usando una paleta de colores 

plot910_temp <-
  plot9_Ponde + scale_fill_manual(values = colorArea) |
  plot10_Ponde + scale_fill_manual(values = colorArea)
plot910_temp


# Boxplot por tenencia de vehículo

plot11_Ponde <- ggplot(diseno$variables,
                       aes(x = log_ingreso_hog , weight = Factor)) +
  geom_boxplot(aes(fill = TIENEVEHICULOS)) +
  ggtitle("Ponderado") +   coord_flip() +
  theme_cepal()

plot12_Ponde <- ggplot(diseno$variables,
                       aes(x = log_gasto, weight = Factor)) +
  geom_boxplot(aes(fill = TIENEVEHICULOS)) +
  ggtitle("Ponderado") +  coord_flip() +
  theme_cepal()

plot11_temp <- plot11_Ponde | plot12_Ponde

plot11_temp

## usando una paleta de colores 
 
plot11_temp <-
plot11_Ponde + scale_fill_manual(values = colorVehiculo) |
  plot12_Ponde + scale_fill_manual(values = colorVehiculo)

plot11_temp


# Boxplot por tipo de vivienda 

plot13_Ponde <- ggplot(diseno$variables,
                       aes(x = log_ingreso_hog, weight = Factor)) +
  geom_boxplot(aes(fill = TIPOVIVIENDA)) +
  ggtitle("Ponderado") +   coord_flip() +
  theme_cepal()

plot14_Ponde <- ggplot(diseno$variables,
                       aes(x = log_gasto, weight = Factor)) +
  geom_boxplot(aes(fill = TIPOVIVIENDA)) +
  ggtitle("Ponderado") +   coord_flip() +
  theme_cepal()

plot13_Ponde | plot14_Ponde

pplot14_temp <-
  plot13_Ponde + scale_fill_manual(values = colorvivienda) |
  plot14_Ponde + scale_fill_manual(values = colorvivienda)

pplot14_temp

# Boxplot por tenencia de vehículo y área 

plot15_Ponde <-
  ggplot(
    diseno$variables,
    aes(x = log_ingreso_hog, y = Area, weight = Factor)
  ) +
  geom_boxplot(aes(fill = TIENEVEHICULOS)) +
  ggtitle("Ponderado") +
  scale_fill_manual(values = colorVehiculo) +
  coord_flip()

plot16_Ponde <-
  ggplot(
   diseno$variables,
    aes(x = log_gasto, y = Area, weight = Factor)
  ) +
  geom_boxplot(aes(fill = TIENEVEHICULOS )) +
  ggtitle("Ponderado") +
  scale_fill_manual(values = colorVehiculo) +
  coord_flip()

plot15_Ponde/plot16_Ponde

# Boxplot por tipo de vivienda y área 

plot17_Ponde <-
  ggplot(
    diseno$variables,
    aes(x = log_ingreso_hog , y = TIPOVIVIENDA, weight = Factor)
  ) +
  geom_boxplot(aes(fill = TIENEVEHICULOS )) +
  ggtitle("Ponderado") +
  scale_fill_manual(values = colorVehiculo) +
  coord_flip()


plot18_Ponde <-
  ggplot(
     diseno$variables,
    aes(
      x = log_gasto,
      y = TIPOVIVIENDA , weight = Factor
    )
  ) +
  geom_boxplot(aes(fill = TIENEVEHICULOS )) +
  ggtitle("Ponderado") +
  scale_fill_manual(values = colorVehiculo) +
  coord_flip()

plot17_Ponde / plot18_Ponde


# ------------------------------------------------------------------------------
# 5. Scatterplots
# ------------------------------------------------------------------------------
# Scatterplot básico
plot19_Ponde <-
  ggplot(
    diseno$variables,
    aes( y = log_gasto, x = log_ingreso_hog,
      weight = Factor
    )
  ) +
  geom_point() +
  theme_cepal()
plot19_Ponde

# Scatterplot con tamaño según Factor
plot20_Ponde <-
  ggplot(
    diseno$variables,
    aes( y = log_gasto, x = log_ingreso_hog)
  ) +
  geom_point(aes(size = Factor), alpha = 0.3) +
  theme_cepal()

plot20_Ponde

# Scatterplot con color según Factor
plot21_Ponde <-
  ggplot(
     diseno$variables,
    aes( y = log_gasto, x = log_ingreso_hog)
  ) +
  geom_point(aes(col = Factor), alpha = 0.3) +
  theme_cepal()

plot21_Ponde

# Scatterplot por área

plot22_Ponde <-
  ggplot(
    diseno$variables,
    aes(
      y = log_gasto, x = log_ingreso_hog,
      shape = Area) # Formas por zona
  ) + geom_point(aes(
    size = Factor, color = Area
  ), alpha = 0.3) +
  labs(size = "Peso") +
  scale_color_manual(values = colorArea) +
  theme_cepal()

plot22_Ponde

# Scatterplot por tenencia de vehículo

plot23_Ponde <-
  ggplot( diseno$variables,
    aes(
      y = log_gasto, x = log_ingreso_hog,
      shape = TIENEVEHICULOS )) +
  geom_point(aes( size = Factor,
    color = TIENEVEHICULOS), alpha = 0.3 ) +
  labs(size = "Peso") +
  scale_color_manual(values = colorVehiculo) +
  theme_cepal()

plot23_Ponde

# Scatterplot por tipo de vivienda

plot24_Ponde <-
  ggplot(diseno$variables,
    aes( y = log_gasto, x = log_ingreso_hog, shape = TIPOVIVIENDA  ) ) +
  geom_point(aes( size = Factor, color = TIPOVIVIENDA ),
  alpha = 0.3 ) +   labs(size = "Peso") +
  scale_color_manual(values = colorvivienda) +
  theme_cepal()

plot24_Ponde

# ------------------------------------------------------------------------------
# 6. Diagramas de barras para variables categóricas
# ------------------------------------------------------------------------------
# Estimación del tamaño por área
(tamano_area <- diseno %>%
  group_by(Area) %>%
  summarise(
    Nd = survey_total(vartype = c("se", "ci"))
  ))



plot25_Ponde <- ggplot(
  data = tamano_area,           # Fuente de los datos
  aes(x = Area,                 # Valores en el eje x
    y = Nd,                     # Altura de la barras
    ymax = Nd_upp,              # Limite superior del IC
    ymin = Nd_low,              # Limite inferior del IC
    fill = Area                 # Color del relleno
  )) +  geom_bar( stat = "identity",# Valor incluido en la tabla
    position = "dodge") +
  geom_errorbar(      # Gráfica del IC.
    position = position_dodge(width = 0.9),
    width = 0.3
  ) + theme_bw()

plot25_Ponde

# Diagrama de barras para pobreza

(tamano_pobreza <- diseno %>%
  group_by(pobreza_LP = as.character(pobreza_LP)) %>%
  summarise(
    Nd = survey_total(vartype = c("se", "ci"))
  ))


plot26_Ponde <- ggplot(
  data = tamano_pobreza,
  aes( x = pobreza_LP, y = Nd,
    ymax = Nd_upp,  ymin = Nd_low,
    fill = pobreza_LP ) ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(
    position = position_dodge(width = 0.9),
    width = 0.3
  ) +   theme_bw()

plot26_Ponde

# Diagrama de barras para pobreza por  tenencia de vehículo

tamano_vehiculo_pobreza <- diseno %>%
  group_by(TIENEVEHICULOS ,
           pobreza_LP = as.character(pobreza_LP)) %>%
  summarise(
    Nd = survey_total(vartype = c("se", "ci"))
  ) %>%   as.data.frame()
tamano_vehiculo_pobreza

plot27_Ponde <-
  ggplot(
    data = tamano_vehiculo_pobreza,
    aes(  x = pobreza_LP,  y = Nd,
      ymax = Nd_upp,  ymin = Nd_low,
      fill = as.factor(TIENEVEHICULOS) ) ) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(  position = position_dodge(width = 0.9),
    width = 0.3 ) + theme_bw() + labs(fill = "Tiene\nvehículo")
plot27_Ponde


# Diagrama de barras para pobreza por  tenencia de vehículo

(prop_vehiculo_Pobreza <- sub_Urbano %>%
  group_by(TIENEVEHICULOS,
            pobreza_LP = as.character(pobreza_LP) ) %>%
  summarise(
    prop = survey_prop(
      vartype = c("se", "ci")
    )
  ) %>%
  data.frame())


plot28_Ponde <- ggplot(
  data = prop_vehiculo_Pobreza,
  aes(
    x = pobreza_LP, y = prop,
    ymax = prop_upp, ymin = prop_low,
    fill = TIENEVEHICULOS
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(
    position = position_dodge(width = 0.9),
    width = 0.3
  ) + scale_fill_manual(values = colorVehiculo) +
  theme_bw()

plot28_Ponde


# Diagrama de barras para el tipo de vivienda  por  tenencia de vehículo

prop_vehiculo_vivienda <- sub_Rural %>%
  group_by(TIPOVIVIENDA , TIENEVEHICULOS) %>%
  summarise(
    prop = survey_prop(vartype = c("se", "ci"))
  ) %>%
  data.frame()


plot29_Ponde <- ggplot(data = prop_vehiculo_vivienda,
                       aes(
                         x = TIPOVIVIENDA,
                         y = prop,
                         ymax = prop_upp,
                         ymin = prop_low,
                         fill = as.factor(TIENEVEHICULOS)
                       )) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9),
                width = 0.3) + labs(fill = "Tiene vehículo") +
  scale_fill_manual(values = colorVehiculo) +
  theme_bw()

plot29_Ponde

# ------------------------------------------------------------------------------
# 7. Mapas con tmap
# ------------------------------------------------------------------------------
library(sf)
library(tmap)
# Nota: Para ejecutar esta sección se necesita el shapefile correspondiente

shapePais <- read_sf("Slides/Imagenes/05_Plots/30_shape/HND.shp")

map1 <- tm_shape(shapePais) +   tm_polygons(col = "dam")

map1

# tmap_save(map1, filename = "Slides/Imagenes/05_Plots/31_Fig_mapa1.png",
#           width = 2000, height = 1500, units = "px", dpi = 300)


# Preparar datos para mapeo
diseno <-  diseno %>%
  mutate(
    dam = haven::as_factor(F1_A0_DEPARTAMENTO, levels  = "values"),
    dam = stringi::stri_pad(str = dam, pad = "0", width = 2)
  )

# Estimación de pobreza por departamento

prop_dam_pobreza <- diseno %>%  group_by(dam) %>%
  summarise(prop = survey_mean(pobreza_LP, vartype = c("se"))) %>%
  data.frame()
prop_dam_pobreza %>% head(10)


# Crear mapa de pobreza (requiere shapefile)

brks <- c(0, .05, 0.1, 0.15, .2, 0.25, 1)
shape_temp <- inner_join(shapePais, prop_dam_pobreza) %>%
  tm_shape()

map2 <- shape_temp + tm_polygons(
  "prop",              # Nombre de la columna
  breaks = brks,       # Puntos de corte
  title = "Pobreza",   # Titilo del labels.
  palette = "YlOrRd"   # Paleta y dirección de colores
)


# Estimación del promedio para log_ingreso_hog por departamento

prom_dam <- svyby(~log_ingreso_hog, ~dam, diseno,
  svymean,
  na.rm = T, covmat = TRUE,
  vartype = c("cv")
) %>% mutate(cv = cv*100)
head(prom_dam)



brks <- c(0, 1, 3)
shape_temp <- inner_join(shapePais, prom_dam) %>%
  tm_shape()

map3 <- shape_temp + tm_polygons(
  "cv",
  breaks = brks,
  title = "cv",
  palette = c("#FFFFFF", "#000000"),
) + tm_layout(asp = 0)
map3

# Estimación de la pobreza por departamento y área

prom_dam_vehiculo <- diseno %>%
  group_by(dam, Area, TIENEVEHICULOS) %>%
  summarise(prop = survey_mean(pobreza_LP , vartype = "cv")) %>%
  filter(TIENEVEHICULOS == "1. Sí", Area == "1. Urbana")
data.frame(prom_dam_vehiculo) %>% head()


shape_temp <- inner_join(shapePais, prom_dam_vehiculo) %>%
  tm_shape()

map4 <- shape_temp + tm_polygons(
  "prop",
  title = "Pobreza",
) + tm_layout(asp = 0)

map4 

map5 <- shape_temp + tm_polygons(
  "prop_cv",
  title = "cv",
  palette = c("#FFFFFF", "#000000"),
  breaks = c(0, 0.2, 1)
) + tm_layout(asp = 0)

map5

#-------------------------------------------------------------------------------
# FIN DEL SCRIPT
#-------------------------------------------------------------------------------
