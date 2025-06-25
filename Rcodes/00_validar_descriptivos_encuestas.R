#########################################################
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())
gc()

#################
### Libraries ###
#################

library(tidyverse)
library(survey)
library(srvyr)
library(data.table)
library(haven)
library(magrittr)
library(fastDummies)
library(stringr)
library(openxlsx)
library(furrr)
select <- dplyr::select

###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(250000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

base_hogar <- read_dta("Data/HND - ENIGH/Bases/Stata/UEB_HOGARES_ENIGH_HND_ETAPA5.dta")
saveRDS(base_hogar, "Slides/Imagenes/02_variable_continua/ENIGH_HND_Hogar.rds")
base_hogar2 <- base_hogar %>% select(
                      INTERVIEW__KEY, #	Llave de entrevista 08 dígitos
                      INTERVIEW__ID, #	Llave única de entrevista 32 caracteres
                      LLAVE_HOGAR, #	LLave de hogar (Interview key + F2_R1_HOGAR__ID)
                      F2_R1_HOGAR__ID, #	Código identificador del hogar
                      CANTIDAD_PERSONAS, #	Cantidad de miembros en el hogar
                      F1_A0_AREA, #	Área
                      F1_A0_ESTRATO,  #	Estrato
                      F1_A0_DEPARTAMENTO, #	Departamento
                      F1_A0_MUNICIPIO, #	Municipio
                      F1_A0_UPM, #	Unidad Primaria de Muestreo
                      Factor, #	Factor de expansión
                      
                      F1_A1_P2_PAREDESEXTVIVIENDA, #	Material predominante en paredes exteriores
                      F1_A1_P3_PAREDESINTVIVIENDA, #	Material predominante en paredes interiores
                      F1_A1_P4_TECHOVIVIENDA, #	Material predominante en el techo
                      F1_A1_P5_PISOVIVIENDA, #	Material predominante en el piso
                      F1_A1_P6_ABASTECEAGUA, #	Medio abastecimiento agua
                      F1_A1_P9_TIPOALUMBRADOVIVIEND, #	Tipo alumbrado
                      F1_A1_P10_ELIMINANBASURAVIV, #	Como elimina la basura de la vivienda
                      F1_A1_P11_TIPOSANITARIOVIV, #	Tipo de servicio sanitario
                      F1_A1_P12_CANTIDADINODOROSVIV, #	Cantidad inodoros, letrinas o tazas sanitarias
                      F2_A1_P1_COCINA__1, #	Tiene Estufa
                      F2_A1_P1_COCINA__2, #	Tiene Eco fogón
                      F2_A1_P1_COCINA__3, #	Tiene Refrigeradora
                      F2_A1_P1_COCINA__4, #	Tiene Microondas
                      F2_A1_P1_COCINA__5, #	Tiene Horno eléctrico
                      F2_A1_P1_COCINA__6, #	Tiene Licuadora
                      F2_A1_P2_LIMPIEZA__7, #	Tiene Lavadora
                      F2_A1_P2_LIMPIEZA__8, #	Tiene Secadora
                      F2_A1_P3_ENTRETENIM__9, #	Tiene Televisor
                      F2_A1_P3_ENTRETENIM__10, #	Tiene Radio
                      F2_A1_P3_ENTRETENIM__11, #	Tiene Equipo  de sonido
                      F2_A1_P3_ENTRETENIM__12, #	Tiene Teatro en casa
                      F2_A1_P3_ENTRETENIM__13, #	Tiene Consolas videojuegos
                      F2_A1_P4_OTROSEQUIPO__14, #	Tiene Teléfono fijo
                      F2_A1_P4_OTROSEQUIPO__15, #	Tiene Computadora
                      F2_A1_P4_OTROSEQUIPO__16, #	Tiene Laptop
                      F2_A1_P4_OTROSEQUIPO__17, #	Tiene Tableta
                      F2_A2_P1_TIENEVEHICULOS, #	Tiene vehículos
                      F2_A2_P2_CANTIDADDEVEHICULOS, #	Cantidad vehículos
                      F2_A4_P4_COMBUSTIBLECOCINAR,  #	Principal combustible para cocinar
                      F2_A5_P1_CUBREGASTOSMENSUALES, #	Cubre sus gastos mensuales con los ingresos totales del hogar
                      F2_A5_P3_LLEVACONTROLGASTOSME, #	Control escrito o electrónico de gastos mensuales
                      GASTO_INVERSION_HOGAR, #	Gastos de inversión del hogar
                      YDISPONIBLE, #	Ingreso disponible
                      YCORRIENTE, #	Ingreso corriente
                      YPRIMARIO, #	Ingreso primario
                      YPRODU, #	Ingreso de la producción
                      YPDCP, #	Ingreso producción doméstica para consumo propio o valor neto de alquiler imputado
                      TRANSF_REC, #	Ingreso de transferencias recibidas monetarias y en especie
                      YPSS, #	Ingreso pensiones y jubilaciones y seguridad social
                      YAS, #	Ingreso prestaciones de asistencia social
                      YISFL, #	Ingreso transferencias corrientes ISFL
                      THR, #	Ingreso transferencias hogares residentes
                      TEP, #	Ingreso transferencias de empresas privadas
                      THNR, #	Ingreso transferencias hogares no residentes remesas
                      YDISPONIBLE_PER, #	Ingreso disponible per cápita del hogar
                      QUINTIL_YDISPO_HOG, #	Quintil de ingreso ordenado según el ingreso disponible del hogar
                      QUINTIL_YDISPO_PER, #	Quintil de ingreso ordenado según el ingreso disponible per cápita del hogar
                      DECIL_YDISPO_HOG, #	Decil según ingreso disponible del hogar
                      DECIL_YDISPO_PER, #	Decil según ingreso disponible per cápita del hogar
                      PERCENTIL_YDISPO_HOG, #	Percentil según ingreso disponible del hogar
                      PERCENTIL_YDISPO_PER, #	Percentil según ingreso disponible per cápita del hogar
                      )


## El país tiene características muy particulares en cuanto a la conformación 
## de las jefaturas de los hogares. En 2021 se estimaron 2 335 100 hogares.
sum(base_hogar$Factor) # 2.618.302
hist(base_hogar$Factor)

diseno_hogar <- as_survey_design_(.data = base_hogar2, 
                  ids = ~F1_A0_UPM,
                  weights = ~Factor, 
                  strata = ~F1_A0_ESTRATO,
                  nest = TRUE)
summary(diseno_hogar)


resumen_variable(
  .data = diseno_hogar,
  var = "F2_A1_P1_COCINA__3"
)

resumen_variable(
  .data = diseno_hogar,
  var = "YDISPONIBLE"
)

resumen_variable(
  .data = diseno_hogar,
  var = "F1_A1_P2_PAREDESEXTVIVIENDA"
)


var_sel <- base_hogar2 %>% select(F1_A1_P2_PAREDESEXTVIVIENDA:PERCENTIL_YDISPO_PER) %>% names()



resumen_total <- map(
  .x = setNames(var_sel, var_sel),
  .f = ~resumen_variable(
    .data = diseno_hogar,
    var = .x
  ),
  .progress = TRUE
)


resumen_Area <- map(
  .x = setNames(var_sel, var_sel),
  .f = ~resumen_variable(
    .data = diseno_hogar %>% mutate(Dominio = F1_A0_AREA),
    var = .x, 
    dominios = "Dominio"
  
  ),
  .progress = TRUE
)


resumen_depto <- map(
  .x = setNames(var_sel, var_sel),
  .f = ~resumen_variable(
    .data = diseno_hogar %>% mutate(Dominio = F1_A0_DEPARTAMENTO),
    var = .x, 
    dominios = "Dominio"
    
  ),
  .progress = TRUE
)

resumen_Area  <- resumen_Area %>%
  map(~ .x %>%
        mutate(Dominio = Dominio %>% haven::as_factor() %>% as.character()))

resumen_depto  <- resumen_depto %>%
  map(~ .x %>%
        mutate(Dominio = Dominio %>% haven::as_factor() %>% as.character()))


lista_combinada <- pmap(
  list(resumen_total, resumen_Area, resumen_depto),
  bind_rows
)


guardar_resultados_excel(lista_combinada, archivo = "Data/HND - ENIGH/Bases/Stata/resultados_combinados.xlsx")

################################################################################

base_persona <- read_dta("Data/HND - ENIGH/Bases/Stata/UEB_PERSONAS_ENIGH_HND_ETAPA5.dta")
saveRDS(base_persona, "Slides/Imagenes/02_variable_continua/ENIGH_HND_Pers.rds")
## población proyectada 11,005,850

sum(base_persona$Factor)# 9.786.113
hist(base_persona$Factor)


diseno_pers <- as_survey_design_(.data = base_persona, 
                            ids = ~F1_A0_UPM, 
                            weights = ~Factor, 
                            strata = ~F1_A0_ESTRATO,
                            nest = TRUE)
summary(diseno_pers)




var_sel <- base_persona %>% select(F2_A6_P3_SEXO:PERCENTIL_YDISPO_PER) %>% names()


seguro_resumen <- safely(resumen_variable)

resumen_total <- map(
  .x = setNames(var_sel, var_sel),
  .f = ~seguro_resumen(
    .data = diseno_pers,
    var = .x
  )
)

resumen_Area <- map(
  .x = setNames(var_sel, var_sel),
  .f = ~seguro_resumen(
    .data = diseno_pers %>% mutate(Dominio = F1_A0_AREA),
    var = .x,
    dominios = "Dominio"
  )
)

resumen_depto <- map(
  .x = setNames(var_sel, var_sel),
  .f = ~seguro_resumen(
    .data = diseno_pers %>% mutate(Dominio = F1_A0_DEPARTAMENTO),
    var = .x,
    dominios = "Dominio"
  )
)

resumen_total <- map(resumen_total, "result")
resumen_Area  <- map(resumen_Area,  "result")
resumen_depto <- map(resumen_depto, "result")

resumen_Area2 <- map_if(
  resumen_Area,
  ~ !is.null(.x),
  ~ .x %>%
    mutate(Dominio = Dominio %>%
             haven::as_factor() %>%
             as.character())
)

resumen_depto2  <- map_if(
  resumen_depto,
  ~ !is.null(.x),
  ~ .x %>%
    mutate(Dominio = Dominio %>%
             haven::as_factor() %>%
             as.character())
)

lista_combinada <- pmap(
  list(resumen_total, resumen_Area2, resumen_depto2),
  bind_rows
)


guardar_resultados_excel(lista_combinada, archivo = "Data/HND - ENIGH/Bases/Stata/resultados_combinados_pers.xlsx")


################################################################################

base_gastos <- read_dta("Data/HND - ENIGH/Bases/Stata/UEB_GASTOS_ENIGH_HND_ETAPA5.dta")

base_gastos %>% distinct(LLAVE_PERSONA, Factor) %>% 
  summarise(tot = sum(Factor))

sum(base_gastos$Factor)# No tiene sentido 218.182.919
hist(base_gastos$Factor)



base_gastos2 <-  base_gastos %>%
  select(
    INTERVIEW__KEY, #	Llave de entrevista 08 dígitos
   INTERVIEW__ID, #	Llave única de entrevista 32 caracteres
    LLAVE_HOGAR, #	LLave de hogar (Interview key + F2_R1_HOGAR__ID)
    LLAVE_PERSONA, #	Código identificador (Interview key + F2_R1_HOGAR__ID+F3_R2_MIEMBROS__ID)
    F1_A0_AREA, #	Área
    F1_A0_ESTRATO, #	Estrato
    F1_A0_DEPARTAMENTO, #	Departamento
    F1_A0_MUNICIPIO, #	Municipio
    F1_A0_UPM, #	Unidad Primaria de Muestreo
    
    GASTO_INVERSION_HOGAR, #	Gasto de inversión del hogar
    CONSUMO_FINAL_HOGAR, #	Gasto de consumo final del hogar
    TRANSF_PAGADA_HOGAR, #	Transferencias pagadas por el hogar
    GASTO_CORRIENTE_HOGAR, #	Gasto corriente del Hogar
    Factor, #	Factor de expansión
    QUINTIL_YDISPO_HOG, #	Quintil de ingreso ordenado según el ingreso disponible del hogar
    QUINTIL_YDISPO_PER, #	Quintil de ingreso ordenado según el ingreso disponible per cápita del hogar
    DECIL_YDISPO_HOG, #	Decil según ingreso disponible del hogar
    DECIL_YDISPO_PER, #	Decil según ingreso disponible per cápita del hogar
    PERCENTIL_YDISPO_HOG, #	Percentil según ingreso disponible del hogar
    PERCENTIL_YDISPO_PER #	Percentil según ingreso disponible per cápita del hogar
  ) %>% distinct() 

dim(base_gastos2)


diseno_gastos <- as_survey_design_(.data = base_gastos2, 
                            ids = ~F1_A0_UPM, 
                            probs = ~Factor, 
                            strata = ~F1_A0_ESTRATO,
                            nest = TRUE)
summary(diseno_gastos)

