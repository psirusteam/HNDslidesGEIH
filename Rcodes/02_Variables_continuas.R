#-------------------------------------------------------------------------------
# 1. Configuración Inicial y Librerías
#-------------------------------------------------------------------------------
# Carga de paquetes esenciales para análisis de encuestas
library(survey)         # Funciones básicas para diseño de encuestas
library(srvyr)          # Versión "tidy" del paquete survey (para usar con dplyr)
library(convey)         # Cálculo de medidas de desigualdad (Gini, Lorenz)
library(TeachingSampling) # Funciones para muestreo estadístico
library(printr)         # Mejora la visualización de resultados
library(kableExtra)     # Formateo profesional de tablas para informes

# Función personalizada para formatear tablas
tba <- function(x){
  x %>%  
    kable(digits = 10,                      # Mostrar 10 dígitos decimales
          format.args = list(scientific = FALSE),  # Evitar notación científica
          format = "latex") %>%              # Formato LaTeX para documentos
    kable_styling(full_width = FALSE)       # Ajuste de ancho de columnas
}

#-------------------------------------------------------------------------------
# 2. Carga y Preparación de Datos
#-------------------------------------------------------------------------------

# Cargar datos de la encuesta desde archivo RDS (formato eficiente de R)

encuesta <- readRDS("Slides/Imagenes/02_variable_continua/ENIGH_HND_Pers.rds")

# Configuración para manejar estratos con una sola UPM (Unidad Primaria de Muestreo)

options(survey.lonely.psu = "adjust")  # Ajusta la varianza para estos casos

# Definición del diseño muestral complejo
diseno <- encuesta %>% 
  mutate(
    estrato = haven::as_factor(F1_A0_ESTRATO), # Convertir estrato a factor
    Sexo = F2_A6_P3_SEXO,                     # Variable de sexo
    Area = F1_A0_AREA                         # Variable de área (urbano/rural)
  ) %>% 
  as_survey_design(
    strata = estrato,    # Variable que identifica los estratos
    ids = F1_A0_UPM,     # Unidades Primarias de Muestreo (conglomerados)
    weights = Factor,    # Factores de expansión/pesos muestrales
    nest = TRUE          # Indica que conglomerados están anidados en estratos
  )

#-------------------------------------------------------------------------------
# 3. Análisis Exploratorio
#-------------------------------------------------------------------------------

## 3.1 Histogramas ponderados
# 14 registros con valores menores que cero

# Histograma ponderado del ingreso (YEMPLEO)
svyhist(
  ~ YEMPLEO,                      # Variable a graficar
  design = diseno %>%             # Subconjunto de datos
    filter(YEMPLEO < 40000, YEMPLEO > 0), # Filtro para valores atípicos
  main = "Distribución del Ingreso por Empleo",
  col = "grey80",                 # Color de las barras
  breaks = 50,                    # Número de intervalos
  xlab = "Ingreso",               # Etiqueta eje X
  probability = FALSE             # Mostrar frecuencias absolutas
)

# Comparación entre histograma ponderado y no ponderado
par(mfrow = c(1,2)) # Configurar área gráfica en 1 fila x 2 columnas

# Histograma ponderado
svyhist(~YEMPLEO, diseno %>% filter(YEMPLEO < 40000, YEMPLEO > 0),
        main = "Ponderado", col = "green", breaks = 50)

# Histograma no ponderado (datos crudos)
hist(encuesta$YEMPLEO[encuesta$YEMPLEO < 40000 & encuesta$YEMPLEO > 0],
     main = "Sin ponderar", xlab = "Ingreso",
     col = "red", prob = TRUE, breaks = 50)


# 3.2 Creación de subgrupos para análisis comparativo

# Definición de subgrupos poblacionales
sub_Urbano <- diseno %>% filter(Area == 1) # Población urbana
sub_Rural  <- diseno %>% filter(Area == 2) # Población rural
sub_Mujer  <- diseno %>% filter(Sexo == 2) # Mujeres
sub_Hombre <- diseno %>% filter(Sexo == 1) # Hombres

# Histogramas comparativos por sexo (mayores de 18 años)
par(mfrow = c(1, 2))
svyhist(
  ~ YEMPLEO,
  design = sub_Mujer %>% filter(YEMPLEO < 40000, YEMPLEO > 0, F2_A6_P4_EDAD >= 18),
  main = "Mujeres",
  breaks = 30,
  col = "grey80",
  xlab = "Ingreso"
)

svyhist(
  ~ YEMPLEO,
  design = sub_Hombre %>% filter(YEMPLEO < 40000, YEMPLEO > 0, F2_A6_P4_EDAD >= 18),
  main = "Hombres",
  breaks = 30,
  col = "grey80",
  xlab = "Ingreso"
)

# Boxplots comparativos por área geográfica
par(mfrow = c(1, 2))
svyboxplot(
  YEMPLEO ~ 1,
  sub_Urbano %>% filter(YEMPLEO < 40000, YEMPLEO > 0),
  col = "grey80",
  ylab = "Ingreso",
  xlab = "Urbano"
)

svyboxplot(
  YEMPLEO ~ 1,
  sub_Rural %>% filter(YEMPLEO < 40000, YEMPLEO > 0),
  col = "grey80",
  ylab = "Ingreso",
  xlab = "Rural"
)

#-------------------------------------------------------------------------------
# 4. Estimación de Estadísticos Descriptivos
#-------------------------------------------------------------------------------

## 4.1 Totales poblacionales
# Estimación del total poblacional para YEMPLEO

svytotal( ~ YEMPLEO, diseno, deff = TRUE) %>%  # deff = TRUE calcula efecto de diseño
  data.frame()                                 # Convertir a dataframe


# Intervalo de confianza para el total
confint(svytotal( ~ YEMPLEO, diseno, deff = TRUE))

# Estimación de totales por subgrupos usando cascade
diseno %>%
  mutate(Sexo = haven::as_factor(Sexo)) %>%   # Convertir sexo a factor con etiquetas
  group_by(Sexo) %>%                          # Agrupar por sexo
  cascade(Total = survey_total(YEMPLEO,             # Calcular total poblacional
                               level = 0.95,        # Nivel de confianza
                               vartype = c("se", "ci")),
          # Mostrar error estándar e IC
          .fill = "Total ingreso"                   # Etiqueta para fila de total
          )
          
## 4.2 Medias poblacionales

# Estimación de la media poblacional
svymean(~YEMPLEO, diseno, deff=T) %>% 
  data.frame() 

# Estimación del intervalo de confianza 

confint(svymean (~YEMPLEO, diseno, deff=T)) 

# Estimación de la media poblacional
svymean (~COSTOALQUI, diseno, deff=T, na.rm = TRUE) %>% 
  data.frame() 

# Estimación del intervalo de confianza 
confint(svymean (~COSTOALQUI, diseno, deff=T, na.rm = TRUE))

diseno <- diseno %>%
  mutate(Sexo = haven::as_factor(F2_A6_P3_SEXO))  # Convertir área a factor

# Medias por  sexo
diseno %>% group_by(Sexo) %>%
  cascade(
    Media = survey_mean(
      COSTOALQUI,                   # Variable de gasto
      level = 0.95,                  # Nivel de confianza
      na.rm = TRUE,                  # Excluir valores faltantes
      vartype = c("se", "ci")        # Mostrar error estándar e IC
    ),
    .fill = "Gasto promedio"         # Etiqueta para fila de total
    ) %>%
  arrange(desc(Sexo))  # Ordena la variable.


diseno <- diseno %>%
  mutate(Area = haven::as_factor(F1_A0_AREA))  # Convertir área a factor

# Medias por área geográfica 

diseno %>% group_by(Area) %>%
  cascade(
    Media = survey_mean(
      COSTOALQUI, level = 0.95, na.rm = TRUE,
      vartype =  c("se", "ci")), 
    .fill = "El gasto medio")%>%
  arrange(desc(Area))  


# Medias por área geográfica y sexo

diseno %>%
  mutate(Area = haven::as_factor(F1_A0_AREA)) %>%  # Convertir área a factor
  group_by(Area, Sexo) %>%                         # Agrupar por área y sexo
  cascade(
    Media = survey_mean(
      COSTOALQUI,                   # Variable de gasto
      level = 0.95,                  # Nivel de confianza
      na.rm = TRUE,                  # Excluir valores faltantes
      vartype = c("se", "ci")        # Mostrar error estándar e IC
    ),
    .fill = "Gasto promedio"         # Etiqueta para fila de total
  ) %>%
  arrange(desc(Area), desc(Sexo))    # Ordenar resultados

## Estimación de la varianza por área geográfica

(tab_sd <- diseno %>% group_by(Area) %>% 
   summarise(Sd = sqrt(
     survey_var(
       YEMPLEO,
       level = 0.95,
       vartype =  c("se", "ci"),
     ) )))  

## Estimación de la varianza por área geográfica y sexo 

(tab_sd <- diseno %>% group_by(Area, Sexo) %>%
    summarise(Sd = sqrt(
      survey_var(YEMPLEO,
                 level = 0.95,
                 vartype =  c("se", "ci"),)
    ))) %>% data.frame() 


##  4.3 Estimación de la mediana

diseno %>% filter(COSTOALQUI > 0) %>% 
  summarise(Mediana = 
              survey_median(
                COSTOALQUI, na.rm = TRUE,
                level = 0.95,
                vartype =  c("se", "ci"),
              )) 

## Estimación de la mediana por área geográfica

diseno %>% filter(COSTOALQUI > 0) %>% 
  group_by(Area) %>% 
  summarise(Mediana = 
              survey_median(
                COSTOALQUI,
                level = 0.95,  na.rm = TRUE,
                vartype =  c("se", "ci"),
              )) 

## Estimación de la mediana por sexo

diseno %>% filter(SALARIO_IMPUT_2 > 0) %>% 
  group_by(Sexo) %>% 
  summarise(Mediana = 
              survey_median(
                SALARIO_IMPUT_2,
                level = 0.95, na.rm = TRUE,
                vartype =  c("se", "ci"),
              )) 

# 4.4 Estimación del cuantil 0.5

# Cálculo del cuantil 0.5 (mediana) del salario imputado para toda la población
# con datos válidos

diseno %>%
  filter(!is.na(SALARIO_IMPUT_2)) %>%  # Se excluyen los valores NA
  summarise(
    Q = survey_quantile(
      SALARIO_IMPUT_2,     # Variable de interés: salario imputado
      quantiles = 0.5,      # Cuantil a estimar (mediana)
      level = 0.95,         # Nivel de confianza del 95%
      vartype = c("se", "ci"), # Se estima error estándar y el intervalo de confianza
      interval_type = "score"  # Método de cálculo del IC: score
    )
  )

# Cálculo del cuantil 0.25 del salario imputado por sexo

diseno %>%
  filter(!is.na(SALARIO_IMPUT_2)) %>%  # Exclusión de valores faltantes
  group_by(Sexo) %>%                   # Agrupación por la variable Sexo
  summarise(
    Q = survey_quantile(
      SALARIO_IMPUT_2,
      quantiles = 0.25,     # Primer cuartil (Q1)
      level = 0.95,
      vartype = c("se", "ci"),
      interval_type = "score"
    )
  )

# Cálculo del cuantil 0.25 del ingreso laboral por área (urbano/rural)
diseno %>%
  filter(YEMPLEO > 0) %>%   # Se excluyen casos sin ingreso laboral
  group_by(Area) %>%        # Agrupación por zona geográfica (urbano/rural)
  summarise(
    Q = survey_quantile(
      YEMPLEO,
      quantiles = 0.25,     # Primer cuartil
      level = 0.95,
      vartype = c("se", "ci"),
      interval_type = "score"
    )
  )

## 4.6 Estimación de la razón poblacional  

# Cálculo de la razón promedio entre el costo del alquiler y el ingreso laboral

diseno %>%
  filter(YEMPLEO > 0, COSTOALQUI > 0) %>%  # Se excluyen observaciones con cero o datos faltantes
  summarise(
    Razon = survey_ratio(
      numerator = COSTOALQUI,  # Variable numerador: gasto en alquiler
      denominator = YEMPLEO,   # Variable denominador: ingreso laboral
      level = 0.95,            # Nivel de confianza del 95%
      vartype = c("se", "ci")  # Error estándar e intervalo de confianza
    )
  )

# Estimación de la razón de mujeres a hombres en el total del diseño muestral
diseno %>% summarise(
  Razon = survey_ratio(
    numerator = (Sexo == "2. Mujer"),       # Crea una variable binaria: 1 si es mujer, 0 si no
    denominator = (Sexo == "1. Hombre"),    # Crea una variable binaria: 1 si es hombre, 0 si no
    level = 0.95,                           # Nivel de confianza para el intervalo
    vartype = c("se", "ci")                 # Se solicita el error estándar y el intervalo de confianza
  )
)

# Transformando la variable de sexo en un factor etiquetado en el subconjunto sub_Rural
sub_Rural <- sub_Rural %>%
  mutate(Sexo = haven::as_factor(F2_A6_P3_SEXO))  # Se convierte la variable original en factor con etiquetas

# Estimación la razón mujeres/hombres en la submuestra rural
sub_Rural %>% summarise(
  Razon = survey_ratio(
    numerator = (Sexo == "2. Mujer"),       # Dummy para mujeres
    denominator = (Sexo == "1. Hombre"),    # Dummy para hombres
    level = 0.95,                           # Nivel de confianza del 95%
    vartype = c("se", "ci")                 # Se incluye error estándar e intervalo de confianza
  )
)

# Estimación la razón entre el costo de alquiler y el ingreso de empleo en mujeres ocupadas
sub_Mujer %>% 
  filter(YEMPLEO > 0, COSTOALQUI > 0) %>%   # Se filtran solo observaciones con ingresos y alquiler mayores a cero
  summarise(
    Razon = survey_ratio(
      numerator = COSTOALQUI,              # Variable en el numerador: costo de alquiler
      denominator = YEMPLEO,               # Variable en el denominador: ingreso por empleo
      level = 0.95,                         # Nivel de confianza
      vartype = c("se", "ci")              # Se calcula error estándar e intervalo de confianza
    )
  )

# Estimación de la razón alquiler/empleo en hombres ocupados 
sub_Hombre %>%
  filter(YEMPLEO > 0, COSTOALQUI > 0) %>%  # Filtrado para asegurar valores positivos
  summarise(
    Razon = survey_ratio(
      numerator = COSTOALQUI,            # Numerador: costo del alquiler
      denominator = YEMPLEO,             # Denominador: ingreso por empleo
      level = 0.95,                      # Nivel de confianza
      vartype = c("se", "ci")           # Error estándar e intervalo de confianza
    )
  )

#-------------------------------------------------------------------------------
# 5. Medidas de Desigualdad
#-------------------------------------------------------------------------------
## Índice de Gini y Curva de Lorenz

# Preparar diseño para medidas de desigualdad
library(convey)
diseno_gini <- convey_prep(diseno)  # Requerido por el paquete convey

# Cálculo del índice de Gini para ingresos
svygini(~YEMPLEO, 
        design = diseno_gini %>% filter(YEMPLEO > 0)) %>%  # Excluir ceros
  data.frame() 

svygini( ~COSTOALQUI,
         design = diseno_gini %>% filter(COSTOALQUI > 0)) %>%
  data.frame() 

# Visualización de la curva de Lorenz
svylorenz(~YEMPLEO, 
          design = diseno_gini %>% filter(YEMPLEO > 0),
          seq(0, 1, .05),    # Puntos para evaluar la curva
          alpha = .01)       # Nivel de significancia


svylorenz( ~YEMPLEO, diseno_gini %>% filter(YEMPLEO > 0), 
           seq(0,1,.05), alpha = .01 )

#-------------------------------------------------------------------------------
# 6. Pruebas de Hipótesis y Contrastes
#-------------------------------------------------------------------------------

## Comparación de medias entre grupos

# Se filtra el diseño para eliminar observaciones con ingreso igual a cero
diseno <- diseno %>% filter(YEMPLEO > 0)

# Prueba t para comparar el ingreso entre hombres y mujeres en la muestra total
svyttest(YEMPLEO ~ Sexo, diseno)


# Se filtra la submuestra urbana para eliminar observaciones con ingreso igual a cero
sub_Urbano <- sub_Urbano %>% filter(YEMPLEO > 0)

# Prueba t para comparar el ingreso entre hombres y mujeres en la submuestra urbana
svyttest(YEMPLEO ~ Sexo, sub_Urbano)


# Prueba t restringida a personas mayores de 18 años
svyttest(YEMPLEO ~ Sexo, diseno %>% filter(F2_A6_P4_EDAD > 18))


###  Estimación de ingreso promedio por departamento

# Se crea una nueva variable categórica con los nombres de los departamentos
diseno  <- diseno %>% mutate(dam = haven::as_factor(F1_A0_DEPARTAMENTO))

# Se estima el ingreso promedio por departamento usando el diseño muestral
(prom_dam <- svyby(~YEMPLEO, ~dam , diseno, 
                   svymean, na.rm = TRUE, 
                   covmat = TRUE,            # Se guarda la matriz de covarianza
                   vartype = c("se", "ci"))) # Se solicita el error estándar e IC


# Se extrae una submatriz de covarianza para dos departamentos específicos
vcov(prom_dam)[2:3, 2:3]

# Se calcula manualmente la desviación estándar de la diferencia entre dos medias
# usando la fórmula clásica de la varianza de la diferencia
sqrt(965391 + 878904 - 2*0)


# Se contrasta la diferencia entre dos departamentos
svycontrast(prom_dam,
            list(diff = c(0, 1, -1, rep(0,15)))) %>%
  data.frame()


# =======================
# Cálculo de contrastes
# =======================

# Se construye una matriz de contrastes con 3 filas (uno por comparación)
# y 18 columnas (una por dominio o subgrupo). Los valores son ceros por defecto.
contrastes <- matrix(0, nrow = 3, ncol = 18)

# Definición de contrastes específicos entre departamentos
# Cada fila representa una comparación lineal entre dos dominios.

# Contraste 1: Atlántida (columna 1) vs Colón (columna 2)
contrastes[1, c(1, 2)] <- c(1, -1)

# Contraste 2: Cortés (columna 5) vs Choluteca (columna 6)
contrastes[2, c(5, 6)] <- c(1, -1)

# Contraste 3: Olancho (columna 15) vs Yoro (columna 18)
contrastes[3, c(15, 18)] <- c(1, -1)

# Mostrar matriz de contrastes
contrastes

# Aplicación de los contrastes definidos a los resultados por dominio (prom_dam)
# utilizando la función svycontrast. Se obtienen estimaciones y errores estándar.
svycontrast(prom_dam, list(
  Atlantida_Colón = contrastes[1,],
  Cortés_Choluteca = contrastes[2,],
  Olancho_Yoro = contrastes[3,]
)) %>% data.frame()


# Promedio de YEMPLEO por sexo


# Se calcula el promedio de YEMPLEO (ingreso laboral) por sexo
# con errores estándar, utilizando diseño de encuesta complejo.

prom_sexo <- svyby(
  ~YEMPLEO,         # variable de interés
  ~Sexo,            # variable de agrupación
  diseno,           # objeto del diseño muestral
  svymean,          # función para estimar la media
  na.rm = TRUE,
  covmat = TRUE,    # guardar matriz de varianzas-covarianzas
  vartype = c("se", "ci")  # incluir error estándar e intervalo de confianza
)

# Mostrar resultados por sexo
prom_sexo

# Cálculo del contraste entre sexos: diferencia (Hombres - Mujeres)
svycontrast(prom_sexo, list(
  diff_Sexo = c(1, -1)
)) %>% data.frame()


# Cálculo directo del error estándar del contraste

# Se muestra cómo obtener el error estándar de la diferencia
# utilizando la matriz de varianzas-covarianzas.

vcov(prom_sexo)

# El error estándar del contraste se calcula como:
# sqrt(var_Hombre + var_Mujer - 2*cov(Hombre,Mujer))
sqrt(207159 + 162242 - 2 * 86645)

## Suma total de ingreso (YEMPLEO) por departamento


# Estimación del total de ingreso por departamento (dam)
sum_dam <- svyby(
  ~ YEMPLEO,         # variable de interés
  ~ dam,             # agrupación por departamento
  diseno, 
  svytotal,          # se estima el total poblacional
  na.rm = TRUE,
  covmat = TRUE,     # guarda la matriz de varianzas-covarianzas
  vartype = c("se", "ci")  # incluye errores estándar e IC
)

sum_dam

##  Agregación de totales para subgrupos de departamentos

# Cálculo de la suma total combinada de los primeros 3 departamentos
svycontrast(sum_dam,
            list(
              Agregado = rep(c(1, 0), c(3, 15))  # Suma ponderada: 1 para primeros 3, 0 para el resto
            )) %>% data.frame()


# Se extrae la submatriz 3x3 de varianzas y covarianzas entre los 3 departamentos
vcov(sum_dam)[1:3, 1:3] %>% as.data.frame()

# Cálculo manual del error estándar del total combinado:
sqrt(176679802169900928 + 88553200088489904 + 321009814737339968 -
       2 * 44864888457644600)

## Promedio de ingreso por grupo etario

# Creación de una variable categórica para grupos de edad
diseno <- diseno %>%
  mutate(Edad_cat = cut(
    F2_A6_P4_EDAD,
    c(0, 15, 30, 45, 60, Inf),
    labels = c("0 - 15", "16 - 30", "31 - 45", "46 - 60", "60 +")
  ))

# Estimación del ingreso promedio por grupo de edad (excluyendo menores de 15 años)
prom_edad <- svyby(
  ~ YEMPLEO,
  ~ Edad_cat,
  diseno %>% filter(F2_A6_P4_EDAD > 15),
  svymean,
  na.rm = TRUE,
  covmat = TRUE
)

prom_edad

##  Agregado del ingreso medio para adultos (promedio entre 4 grupos de edad)

# Se estima el promedio combinado de las medias para los 4 grupos etarios intermedios
svycontrast(prom_edad,
            list(
              agregado_edad = c(1/4, 1/4, 1/4, 1/4)  # promedio ponderado simple
            )) %>% data.frame()


# Visualización de la matriz de covarianzas de las medias por edad
vcov(prom_edad)

# Cálculo manual del error estándar del promedio combinado
(1 / 4) * sqrt(
  49065 + 2*39352 + 2*30798 + 2*95779 +
    562921 + 2*71905 + 2*129515 +
    282640 + 2*130462 +
    1131332
)

# Filtra los casos con datos válidos de costo de alquiler
diseno_temp <- diseno %>% filter(COSTOALQUI > 0)

# Calcula la razón del ingreso sobre el costo de alquiler, por sexo
razon_sexo <- svyby(
  ~ YEMPLEO,              # Numerador
  ~ Sexo,                 # Agrupación
  denominator = ~ COSTOALQUI,  # Denominador
  design = diseno_temp,
  FUN = svyratio,
  na.rm = TRUE,
  covmat = TRUE,
  vartype = c("se", "ci")
)

razon_sexo


# Contraste de diferencias entre mujeres y hombres en la razón ingreso/alquiler
svycontrast(razon_sexo,
            list(
              diff_sexo = c(1, -1)
            )) %>% data.frame()



# Visualización de la matriz de varianza-covarianza de la razón
vcov(razon_sexo)

# Cálculo del error estándar de la diferencia entre razones:
# sqrt(var_hombre + var_mujer - 2 * cov(hombre, mujer))
sqrt(39.2 + 33.20 - 2 * (-17.66))
