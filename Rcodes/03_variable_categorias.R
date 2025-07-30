# ==============================================================================
# Análisis de encuestas de hogares con R - Módulo 2: Análisis de variables categóricas
# CEPAL - Unidad de Estadísticas Sociales
# Autor: Andrés Gutiérrez y Stalyn Guerrero 
# Email: andres.gutierrez@cepal.org
# ==============================================================================

#-------------------------------------------------------------------------------
# Cargar librerías necesarias
#-------------------------------------------------------------------------------
options(digits = 4)
library(magrittr)
library (survey)
library(srvyr)
library(convey)
library(TeachingSampling)
library(printr)
select <- dplyr::select

#-------------------------------------------------------------------------------
# 1. Lectura de la base de datos y diseño de encuesta
#-------------------------------------------------------------------------------

# Leer la base de datos
encuesta <- readRDS("Slides/Imagenes/02_variable_continua/ENIGH_HND_Pers.rds")

# Configurar opciones para el diseño de encuesta
options(survey.lonely.psu = "adjust")

# Crear el objeto de diseño de encuesta
diseno <- encuesta %>%
  mutate(
    estrato = haven::as_factor(F1_A0_ESTRATO),
    Sexo = haven::as_factor(F2_A6_P3_SEXO),
    Area = haven::as_factor(F1_A0_AREA)
  ) %>%
  as_survey_design(
    strata = estrato,  # Identificador de los estratos
    ids = F1_A0_UPM,   # Identificador para las observaciones (UPM)
    weights = Factor,  # Factores de expansión
    nest = TRUE        # Validar el anidado dentro del estrato
  )

#-------------------------------------------------------------------------------
# 2. Creación de nuevas variables
#-------------------------------------------------------------------------------

# Crear variable categórica para edad
diseno <- diseno %>%
  mutate(
    Edad_cat = case_when(
      F2_A6_P4_EDAD < 16 ~ "0 - 15",
      F2_A6_P4_EDAD < 31 ~ "16 - 30",
      F2_A6_P4_EDAD < 46 ~ "31 - 45",
      F2_A6_P4_EDAD < 61 ~ "46 - 60",
      F2_A6_P4_EDAD > 60 ~ "60 +",
      TRUE ~ NA_character_
    )
  )

#-------------------------------------------------------------------------------
# 3. Dividir la muestra en subgrupos
#-------------------------------------------------------------------------------

# Crear subgrupos basados en área y sexo
sub_Urbano <- diseno %>% filter(Area == "1. Urbana")
sub_Rural <- diseno %>% filter(Area == "2. Rural")
sub_Mujer <- diseno %>% filter(Sexo == "2. Mujer")
sub_Hombre <- diseno %>% filter(Sexo == "1. Hombre")


#-------------------------------------------------------------------------------
# 4. Estimación del tamaño poblacional
#-------------------------------------------------------------------------------

# Estimación del tamaño por área
tamano_zona <- diseno %>%
  group_by(Area) %>%
  summarise(
    n = unweighted(n()),  # Observaciones en la muestra
    Nd = survey_total(vartype = c("se", "ci"))  # Total poblacional con SE y CI
  )
tamano_zona
# Estimación del tamaño por decil de ingreso
tamano_decil <- diseno %>%
  mutate(DECIL = haven::as_factor(DECIL_YDISPO_PER)) %>%
  group_by(DECIL) %>%
  summarise(Nd = survey_total(vartype = c("se", "ci")))
tamano_decil

# Estimación del tamaño por etnia
tamano_etnia<- diseno %>% 
    mutate(etnia = haven::as_factor(F2_A6_P5_ETNIA) ) %>% 
    group_by(etnia) %>% 
    summarise(
      Nd = survey_total(vartype = c("se","ci")))
tamano_etnia

# Estimación del tamaño por ocupación
(
  tamano_ocupacion <- diseno %>%
    mutate(ocupacion = haven::as_factor(F2_A9_P3_TIPOEMPLEADO)) %>%
    group_by(ocupacion) %>%
    summarise(Nd = survey_total(vartype = c("se", "ci")))
)


# Estimación del tamaño por etnia y sexo

(
  tamano_etnia_sexo <- diseno %>%
    mutate(etnia = haven::as_factor(F2_A6_P5_ETNIA)) %>%
    group_by(etnia, Sexo) %>%
    cascade(
      Nd = survey_total(vartype = c("se", "ci")),
      .fill = "Total"
    ) %>%
    data.frame()
)

#-------------------------------------------------------------------------------
# 5. Estimación de proporciones poblacionales
#-------------------------------------------------------------------------------

# Proporción por área
(
  prop_Area <- diseno %>% group_by(Area) %>%
   summarise(prop = survey_mean(
     vartype = c("se", "ci"),
     proportion = TRUE
   ))
 )

# Mostrar resultados
(
  prop_area2 <- diseno %>% group_by(Area) %>%
    summarise(prop = survey_prop(vartype = c("se", "ci")))
  )

# Proporción de hombres y mujeres en área urbana
(
  prop_sexoU <- sub_Urbano %>% group_by(Sexo) %>%
    summarise(prop = survey_prop(vartype = c("se", "ci")))
  )

# Proporción de hombres y mujeres en área rural
(
  prop_sexoR <- sub_Rural %>% group_by(Sexo) %>%
    summarise(n = unweighted(n()),
              prop = survey_prop(vartype = c("se", "ci")))
  )

# Proporción de hombres en área urbana y rural

(
  prop_AreaH <- sub_Hombre %>% group_by(Area) %>%
    summarise(prop = survey_prop(vartype = c("se", "ci")))
  )

# Proporción de mujeres en área urbana y rural

(
  prop_AreaM <- sub_Mujer %>% group_by(Area) %>%
    summarise(prop = survey_prop(vartype = c("se", "ci")))
  )


# Proporción de hombres por área y categoría de edad
(prop_AreaH_edad <- sub_Hombre %>%
    group_by(Area, Edad_cat ) %>% 
    summarise(
      prop = survey_prop(vartype = c("se","ci")))%>%
    data.frame())

# Proporción de mujer por área y categoría de edad
(prop_AreaM_edad <- sub_Mujer %>% 
    group_by(Area, Edad_cat) %>% 
    summarise(
      prop = survey_prop(vartype = c("se","ci"))) %>%
    data.frame())




# Proporción de hombres por área disponibles para trabajar 

#F2_A8_P13_DISPONIBLETRABAJAR:	Estaba disponible para trabajar

(prop_AreaH_disponible <- sub_Hombre %>%
    mutate(disponible = haven::as_factor(F2_A8_P13_DISPONIBLETRABAJAR)) %>%
    group_by(Area, disponible) %>% 
    summarise(
      prop = survey_prop(vartype = c("se","ci"))) %>%
    data.frame())

# Proporción de mujeres por área disponibles para trabajar 

(prop_AreaM_disponible <- sub_Mujer %>% 
    mutate(
      disponible = haven::as_factor(F2_A8_P13_DISPONIBLETRABAJAR)) %>%
    group_by(Area, disponible) %>% 
    summarise( prop = survey_prop(vartype = c("se","ci"))) %>%
    data.frame())

## Transformación de la variable F2_A8_P13_DISPONIBLETRABAJAR

diseno <-  diseno %>%
  mutate(
    disponible = case_when(
      F2_A8_P13_DISPONIBLETRABAJAR == 1 ~ "Sí",
      F2_A8_P13_DISPONIBLETRABAJAR %in% c(2:5) ~
        "No",
      TRUE ~ NA_character_
    )
  )

# Proporción de disponibles para trabajar por rango de edad
diseno %>%  group_by(disponible, Edad_cat) %>%
  summarise(Prop = survey_prop(vartype =  c("se", "ci"))) %>%
  data.frame()

# Proporción de personas por rango de edad en zona rural
sub_Rural %>%
  group_by(Edad_cat) %>% 
  summarise(
    Prop = survey_prop(
      vartype =  c("se", "ci"))) %>%
  data.frame()

## Transformación de la variable F2_A8_P13_DISPONIBLETRABAJAR para 
## sub población de mujeres

# Proporción de disponibles para trabajar por rango de edad
sub_Mujer %>% 
  mutate(
    disponible = case_when(
      F2_A8_P13_DISPONIBLETRABAJAR == 1 ~ "Sí",
      F2_A8_P13_DISPONIBLETRABAJAR %in% c(2:5) ~
        "No",
      TRUE ~ NA_character_
    )
  )  %>% group_by(disponible, Edad_cat) %>% 
  summarise(Prop = survey_prop(
    vartype =  c("se", "ci"))) %>%   data.frame()


## Transformación de la variable F2_A8_P13_DISPONIBLETRABAJAR para 
## sub población de hombres

# Proporción de disponibles para trabajar por rango de edad
sub_Hombre %>% 
  mutate(
    disponible = case_when(
      F2_A8_P13_DISPONIBLETRABAJAR == 1 ~ "Sí",
      F2_A8_P13_DISPONIBLETRABAJAR %in% c(2:5) ~
        "No",
      TRUE ~ NA_character_
    )
  )  %>% group_by(disponible, Edad_cat) %>% 
  summarise(Prop = survey_prop(
    vartype =  c("se", "ci"))) %>%   data.frame()

#-------------------------------------------------------------------------------
# 6. Tablas cruzadas y pruebas de independencia
#-------------------------------------------------------------------------------

## Transformación de la variable F2_A6_P5_ETNIA  

diseno <- diseno %>% 
  mutate(etnia = haven::as_factor(F2_A6_P5_ETNIA))

# Tabla cruzada: Etnia vs Sexo
(
  prop_sexo_etnia <- diseno %>% 
    group_by(etnia, Sexo) %>%
    summarise(
      prop = survey_prop(vartype = c("se", "ci"))) %>% 
    data.frame()
)

# Tabla cruzada usando svyby

tab_Sex_etnia <- svyby(~Sexo,  ~etnia, diseno, svymean)
tab_Sex_etnia

# Intervalos de confianza para la tabla cruzada
confint(tab_Sex_etnia) %>% as.data.frame()

# Prueba de independencia chi-cuadrado
svychisq(~Sexo + etnia, diseno, statistic="F")

# Tabla cruzada: Sexo vs Disponibilidad para trabajar
(tab_Sex_Ocupa <- svyby(~Sexo,  ~disponible,
                        diseno, svymean))

# Intervalos de confianza para la tabla cruzada
confint(tab_Sex_Ocupa) %>% as.data.frame()

# Prueba de independencia chi-cuadrado
svychisq(~Sexo + disponible, 
         design = diseno,  statistic="F")


## Transformación de la variable F2_A6_P7_SEGUROMEDIC__1  y 
## F1_A0_DEPARTAMENTO

# F2_A6_P7_SEGUROMEDIC__1	Cobertura seguro social (IHSS)

diseno <- diseno %>%
  mutate(SEGUROMEDIC = haven::as_factor(F2_A6_P7_SEGUROMEDIC__1), 
         DEPARTAMENTO  = haven::as_factor(F1_A0_DEPARTAMENTO ))

# Tabla cruzada: Departamento vs Cobertura de seguro médico
tab_dam_IHSS <- 
  svyby( ~SEGUROMEDIC, ~DEPARTAMENTO, diseno, svymean)

# Prueba de independencia chi-cuadrado

svychisq(~ SEGUROMEDIC + DEPARTAMENTO,
         design = diseno,
         statistic = "F")

#-------------------------------------------------------------------------------
# 7. Razón de odds y contrastes
#-------------------------------------------------------------------------------

## Transformación de la variable F2_A6_P7_SEGUROMEDIC__1  

diseno <-
  diseno %>% 
  mutate(SEGUROMEDIC2 = case_when(SEGUROMEDIC == "Sí" ~ 1,
                                  SEGUROMEDIC == "No" ~ 0,
                                  TRUE ~ NA_real_))

# Estimación de proporciones por sexo
(tab_Sex <- svyby(~SEGUROMEDIC2,  ~Sexo, diseno,
                  svymean, vartype = c("se", "ci")))

# Razón de odds
svycontrast(tab_Sex, quote(`1. Hombre`/`2. Mujer`)  )


# Estimación de interacción Sexo-SEGUROMEDIC
tab_Sex_IHSS <- 
  svymean(~interaction (Sexo, SEGUROMEDIC), diseno, 
          se=T, na.rm=T, ci=T, keep.vars=T) 
tab_Sex_IHSS %>%  as.data.frame()



# Razón de odds para interacción
svycontrast(tab_Sex_IHSS,
            quote(
              (
                `interaction(Sexo, SEGUROMEDIC)1. Hombre.No` / `interaction(Sexo, SEGUROMEDIC)1. Hombre.Sí`
              ) /
                (
                  `interaction(Sexo, SEGUROMEDIC)2. Mujer.No` / `interaction(Sexo, SEGUROMEDIC)2. Mujer.Sí`
                )
            ))

#-------------------------------------------------------------------------------
# 8. Contrastes de diferencias de proporciones
#-------------------------------------------------------------------------------

# Diferencia de proporciones en seguro médico por sexo

(tab_sex_IHSS <- svyby(~SEGUROMEDIC2, ~Sexo, 
                       diseno , 
                       svymean, na.rm=T,
                       covmat = TRUE,
                       vartype = c("se", "ci")))

# Calcular diferencia manualmente
(diferencia <- tab_sex_IHSS$SEGUROMEDIC2[1] - tab_sex_IHSS$SEGUROMEDIC2[2])

# Matriz de covarianza
vcov(tab_sex_IHSS)%>% as.data.frame() %>% 
  kable(digits = 10,
        format.args = list(scientific = FALSE))

# Error estándar de la diferencia
sqrt(0.00002978 + 0.00002129 - 2*0.00001523)

# Diferencia usando svycontrast
svycontrast(tab_sex_IHSS,
            list(diff_Sex = c(1, -1))) %>%
  data.frame()

# Diferencia en disponibilidad para empleo por sexo

diseno <-
  diseno %>%
  mutate(disponible2 = case_when(disponible == "Sí" ~ 1,
                                 disponible == "No" ~ 0,
                                 TRUE ~ NA_real_))
(
  tab_sex_desempleo <- svyby(
    ~ disponible2,
    ~ Sexo,
    diseno ,
    svymean,
    na.rm = T,
    covmat = TRUE,
    vartype = c("se", "ci")
  )
)


# Calcular diferencia manualmente
diferencia_desempleo <- tab_sex_desempleo$disponible2[1] - tab_sex_desempleo$disponible2[2]
print(diferencia_desempleo)	

# Matriz de covarianza
vcov(tab_sex_desempleo) %>% as.data.frame() %>% 
  kable(digits = 10,
        format.args = list(scientific = FALSE))

# Error estándar de la diferencia
sqrt(0.00011491	 + 0.00008382 - 2*0.00002256)

# Diferencia usando svycontrast
svycontrast(tab_sex_desempleo,
            list(diff_Sex = c(1, -1))) %>%
  data.frame()


#-------------------------------------------------------------------------------
# FIN DEL SCRIPT
#-------------------------------------------------------------------------------