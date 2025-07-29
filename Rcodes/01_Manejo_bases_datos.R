#-------------------------------------------------------------------------------
# 1. Configuración Inicial y Librerías
#-------------------------------------------------------------------------------

# Limpiar entorno de trabajo - Elimina todos los objetos del entorno global
rm(list = ls())

# Instalar librerías si es necesario (instrucción comentada)
# install.packages(c("dplyr", "readstata13", "survey", "srvyr", "ggplot2", 
#                   "TeachingSampling", "samplesize4surveys", "knitr"))

# Cargar librerías esenciales
library(dplyr)          # Manipulación de datos con sintaxis intuitiva
library(readstata13)    # Para leer archivos .dta de Stata versión 13+
library(survey)         # Análisis de encuestas complejas (diseños muestrales)
library(srvyr)          # Versión "tidy" del paquete survey (integrado con dplyr)
library(ggplot2)        # Creación de gráficos avanzados
library(TeachingSampling) # Funciones para muestreo estadístico
library(samplesize4surveys) # Cálculo de tamaños de muestra para encuestas

# Configurar tema por defecto para gráficos ggplot2
ggplot2::theme_set(theme_bw()) # Establece tema blanco y negro como predeterminado

#-------------------------------------------------------------------------------
# 2. Carga y Exploración de Datos
#-------------------------------------------------------------------------------

# Cargar conjunto de datos "BigCity" del paquete TeachingSampling
data("BigCity", package = "TeachingSampling")

# Guardar datos en formato RDS (formato binario eficiente de R)
saveRDS(BigCity, "Slides/Imagenes/01_ManejoBase/BigCity.rds") 

# Leer datos desde archivo RDS (más rápido que .csv o .dta para grandes conjuntos)
data2 <- readRDS("Slides/Imagenes/01_ManejoBase/BigCity.rds")

# Exploración inicial de la estructura de los datos
cat("Número de registros:", nrow(data2), "\n")  # Conteo de filas
cat("Número de variables:", ncol(data2), "\n")  # Conteo de columnas

# Dimensiones del dataframe (filas x columnas)
dim(data2)

# Abrir visor de datos (interfaz gráfica para explorar)
View(data2)

# Mostrar nombres de las variables
names(data2)

# Estructura detallada del dataframe (tipos de datos, primeras observaciones)
cat("Estructura de los datos:\n")
str(data2)
# Mostrar primeras 5 filas del dataframe
cat("\nPrimeras 5 filas:\n")
head(data2, 5)

#-------------------------------------------------------------------------------
# 3. Transformación de Variables
#-------------------------------------------------------------------------------

# Crear variable "Region" a partir de "Stratum" (extrae números del texto)
Region <- as.numeric(gsub("\\D", "", data2$Stratum))  # \\D coincide con no-dígitos

# Convertir números de región en categorías con etiquetas
data2$Region <- cut(
  Region, 
  breaks = 5,  # Divide en 5 intervalos
  labels = c("Norte", "Sur", "Centro", "Occidente", "Oriente")  # Etiquetas
)

# Crear identificador numérico para regiones (factor ordenado)
data2$IDRegion <- factor(
  data2$Region,
  levels = c("Norte", "Sur", "Centro", "Occidente", "Oriente"),  # Niveles
  labels = c("01", "02", "03", "04", "05")  # Etiquetas numéricas
)

# Crear nombre corto para regiones (factor ordenado)
data2$Nom_corto <- factor(
  data2$Region,
  levels = c("Norte", "Sur", "Centro", "Occidente", "Oriente"),
  labels = c("N", "S", "C", "O", "E")  # Etiquetas de un carácter
)

# Conteo de registros usando dplyr
data2 %>% count()


#-------------------------------------------------------------------------------
# 4. Manipulación de Datos con dplyr
#-------------------------------------------------------------------------------

## 4.1 filter - Filtrar observaciones

# Filtrar por región (dos métodos equivalentes)
dataregion1 <- data2 %>% filter(IDRegion == "01")  # Usando ID numérico
dataregion2 <- data2 %>% filter(Region == "Norte") # Usando nombre de región

# Filtrar por zona (urbana/rural)
dataurbano <- data2 %>% filter(Zone == "Urban")
datarural <- data2 %>% filter(Zone == "Rural")

# Filtrar por ingresos específicos
dataingreso1 <- data2 %>% filter(Income %in% c(50, 100))  # Ingresos bajos
dataingreso2 <- data2 %>% filter(Income %in% c(1000, 2000)) # Ingresos altos

## 4.2 select - Seleccionar columnas

# Seleccionar columnas específicas
datared <- data2 %>% select(HHID, PSU, Region, Stratum)  # Variables geográficas
datablue <- data2 %>% select(PersonID, Age, Sex, Income) # Variables individuales

# Excluir columnas específicas
datagrey <- data2 %>% select(-MaritalST, -IDRegion)  # Elimina estas columnas

## 4.3 arrange - Ordenar datos

# Ordenar por ingreso (ascendente)
datadog <- datablue %>% arrange(Income)
datadog %>% head()  # Mostrar primeras filas

# Ordenar múltiples columnas (sexo y luego edad)
datablue %>% arrange(Sex, Age) %>% head()

# Orden descendente (dos métodos equivalentes)
datablue %>% arrange(desc(Age)) %>% head()  # Usando función desc()
datablue %>% arrange(-Age) %>% head()       # Usando signo negativo

## 4.4 mutate - Crear nuevas variables

# Crear nueva variable (doble del ingreso)
datablue2 <- datablue %>% mutate(Income2 = 2 * Income)

# Crear múltiples variables en cadena
datacat <- datablue %>%
  mutate(
    Income2 = 2 * Income,  # Doble del ingreso
    Income4 = 2 * Income2  # Cuádruple del ingreso original
  )

# Crear categorías de edad (variable ordinal)
data2 <- data2 %>% mutate(
  CatAge = case_when(
    Age <= 5 ~ "0-5",
    Age <= 15 ~ "6-15",
    Age <= 30 ~ "16-30",
    Age <= 45 ~ "31-45",
    Age <= 60 ~ "46-60",
    TRUE ~ "Más de 60"  # Valor por defecto (else)
  ),
  # Convertir a factor ordenado
  CatAge = factor(CatAge, 
                  levels = c("0-5", "6-15", "16-30", "31-45", "46-60", "Más de 60"), 
                  ordered = TRUE)
)

## 4.5 group_by - Resúmenes agrupados

# Conteo por región (ordenado descendente)
data2 %>%
  group_by(Region) %>%
  summarise(n = n()) %>%  # n() cuenta observaciones
  arrange(desc(n))        # Ordena por conteo descendente

# Conteos por otras variables categóricas
data2 %>%
  group_by(Sex) %>%
  summarise(n = n()) %>% arrange(desc(n))

data2 %>%
  group_by(Zone) %>%
  summarise(n = n()) %>% arrange(desc(n))

data2 %>%
  group_by(Employment) %>%
  summarise(n = n()) %>% arrange(desc(n))

#-------------------------------------------------------------------------------
# 5. Muestreo en Dos Etapas Estratificado
#-------------------------------------------------------------------------------

# Crear marco muestral a nivel de UPM (Unidades Primarias de Muestreo)
FrameI <- data2 %>%
  group_by(PSU) %>%  # Agrupar por UPM
  summarise(
    Stratum = unique(Stratum),  # Estrato al que pertenece
    Persons = n(),              # Personas por UPM
    Income = sum(Income),       # Ingreso total
    Expenditure = sum(Expenditure) # Gasto total
  )

# Calcular tamaños de muestra por estrato
sizes <- FrameI %>%
  group_by(Stratum) %>%
  summarise(
    NIh = n(),   # Total de UPM por estrato
    nIh = 2,     # UPM a seleccionar por estrato (fijo=2)
    dI = NIh / nIh  # Factor de expansión (inverso de probabilidad)
  )

# Selección de UPM (etapa 1) usando muestreo estratificado

set.seed(1234)  # Fijar semilla para reproducibilidad
samI <- S.STSI(Stratum, NIh, nIh)  # Función de TeachingSampling
UI <- levels(as.factor(FrameI$PSU))
sampleI <- UI[samI]  # UPM seleccionadas

# Unir datos de UPM seleccionadas con información de tamaños
FrameII <- sizes %>%
  left_join(data2 %>% filter(PSU %in% sampleI), by = "Stratum")

# Selección de hogares (etapa 2) - Muestreo aleatorio simple dentro de UPM
HHdb <- FrameII %>%
  group_by(PSU) %>%
  summarise(Ni = n_distinct(HHID))  # Hogares por UPM

Ni <- as.numeric(HHdb$Ni)
ni <- ceiling(Ni * 0.1)  # Muestra 10% de hogares por UPM (redondeo hacia arriba)

# Selección inicial para la primera UPM
sam = S.SI(Ni[1], ni[1])  # Muestreo aleatorio simple
clusterII = FrameII[which(FrameII$PSU == sampleI[1]), ]
sam.HH <- data.frame(HHID = unique(clusterII$HHID)[sam])

# Unir datos y calcular factores de expansión
clusterHH <- left_join(sam.HH, clusterII, by = "HHID")
clusterHH$dki <- Ni[1] / ni[1]  # Factor etapa 2
clusterHH$dk <- clusterHH$dI * clusterHH$dki  # Factor final (producto de etapas)

# Inicializar muestra final
sam_data = clusterHH

# Selección para las demás UPM (bucle)
set.seed(1234)
for (i in 2:nrow(HHdb)) {
  hogares_UPM <- unique(FrameII$HHID[FrameII$PSU == HHdb$PSU[i]])
  sel_hogares <- sample(hogares_UPM, ni[i])  # Muestra aleatoria simple
  tmp <- FrameII %>%
    filter(PSU == HHdb$PSU[i], HHID %in% sel_hogares) %>%
    mutate(
      dki = HHdb$Ni[i] / ni[i],  # Factor etapa 2
      dk = dI * dki              # Factor final
    )
  sam_data <- bind_rows(sam_data, tmp)  # Acumular resultados
}

# Muestra final
encuesta <- sam_data
cat("\nMuestra final:", nrow(encuesta), "registros")

# Verificar suma de pesos vs tamaño poblacional
sum(encuesta$dk)  # Suma de factores de expansión
nrow(data2)     # Tamaño de la población

#-------------------------------------------------------------------------------
# 6. Diseño Muestral y Calibración
#-------------------------------------------------------------------------------

# Definir diseño muestral complejo usando srvyr
diseno <- encuesta %>%
  as_survey_design(
    strata = Stratum,  # Estratos
    ids = PSU,         # Conglomerados (UPM)
    weights = dk,      # Pesos muestrales
    nest = TRUE        # Conglomerados anidados en estratos
  )

# Calcular totales poblacionales conocidos para calibración
totales <- colSums(model.matrix(~ -1 + Zone + Sex, data = data2))

# Calibrar pesos para ajustar a totales poblacionales conocidos
diseno_cal <- calibrate(
  design = diseno,
  formula = ~ -1 + Zone + Sex,  # Variables para calibración
  population = totales,         # Totales poblacionales
  calfun = "linear"             # Función de calibración lineal
)

# Comparar suma de pesos antes/después de calibración
sum(weights(diseno))     # Suma de pesos originales
sum(weights(diseno_cal)) # Suma de pesos calibrados
nrow(data2)              # Tamaño poblacional real

# Añadir pesos calibrados al dataframe
encuesta$wk <- weights(diseno_cal)

#-------------------------------------------------------------------------------
# 7. Visualización de Resultados
#-------------------------------------------------------------------------------

# Histograma de pesos originales vs calibrados
par(mfrow = c(1, 2))  # Dividir área de gráficos en 1 fila x 2 columnas
hist(encuesta$dk, main = "Pesos Originales (dk)", xlab = "dk", col = "lightblue")
hist(encuesta$wk, main = "Pesos Calibrados (wk)", xlab = "wk", col = "lightgreen")

# Diagrama de dispersión pesos originales vs calibrados
plot(encuesta$dk, encuesta$wk, 
     xlab = "Pesos originales (dk)", 
     ylab = "Pesos calibrados (wk)",
     pch = 19, col = "blue", cex = 0.7)
abline(a = 0, b = 1, col = "red", lwd = 2)  # Línea de 45° (y=x)

# Boxplot de pesos por estrato
boxplot(encuesta$wk ~ encuesta$Stratum, 
        xlab = "Estrato", 
        ylab = "Pesos calibrados (wk)",
        col = "orange",
        main = "Distribución de pesos por estrato")

#-------------------------------------------------------------------------------
# FIN DEL SCRIPT
#-------------------------------------------------------------------------------