############################################################
# ICFES Saber 11 - Limpieza y preparación de datos (2024)
# Autor: German Pava
# Objetivo: generar base limpia y recategorizada (Tolima)
############################################################

# ======================
# Librerías
# ======================
library(tidyverse)
library(janitor)
library(naniar)
library(readr)

# ======================
# Configuración inicial
# ======================
# Establecer WD en la ubicación del script (solo RStudio)
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# ======================
# Carga de datos crudos
# ======================
raw_data <- read_delim(
  file   = "../data/raw/Examen_Saber_11_20242.txt",
  delim  = ";",
  locale = locale(encoding = "UTF-8")
)


# Normalización de nombres
raw_data <- raw_data %>%
  clean_names()

# ======================
# Diagnóstico inicial de valores perdidos
# ======================
head(raw_data)
gg_miss_var(raw_data)
colSums(is.na(raw_data))

# ======================
# Cálculo de edad (fecha oficial de presentación)
# ======================
FECHA_EXAMEN <- as.Date("2024-08-18")  # cambiar si aplica otro año

raw_data <- raw_data %>%
  mutate(
    estu_fechanacimiento = as.Date(estu_fechanacimiento, format = "%d/%m/%Y"),
    estu_edad = floor(
      as.numeric(difftime(FECHA_EXAMEN, estu_fechanacimiento, units = "days")) / 365.25
    )
  )



# ======================
# Variables de interés
# ======================
vars_keep <- c(
  "estu_nacionalidad", "estu_depto_reside", "estu_mcpio_reside",
  "estu_estudiante", "fami_cuartoshogar","fami_personashogar",
  "estu_nse_individual", "estu_nse_establecimiento",
  
  "punt_lectura_critica", "punt_matematicas", "punt_c_naturales",
  "punt_sociales_ciudadanas", "punt_ingles", "punt_global",
  
  "cole_jornada", "cole_naturaleza", "cole_area_ubicacion",
  
  "fami_estratovivienda", "estu_edad", "estu_genero", # "cole_genero",
  "fami_situacioneconomica", "fami_educacionpadre", "fami_educacionmadre",
  
  "fami_comecarnepescadohuevo", # "fami_comelechederivados", "fami_comecerealfrutoslegumbre",
  
  "fami_numlibros", "estu_dedicacionlecturadiaria",
  "estu_dedicacioninternet", "estu_horassemanatrabaja",
  
  "fami_tienecomputador", "fami_tienemotocicleta", "fami_tieneautomovil"
)

df <- raw_data %>%
  select(all_of(vars_keep)) %>%
  mutate(across(where(is.character), as.factor))

rm(raw_data)
gc()

# ======================
# Diagnóstico tras selección de variables
# ======================
summary(df)
gg_miss_var(df)

# ======================
# Filtros básicos
# ======================
df <- df %>%
  filter(
    estu_estudiante == "ESTUDIANTE",
    estu_nacionalidad == "COLOMBIA",
    !cole_jornada %in% c("SABATINA", "NOCHE"),
    estu_edad >= 15 & estu_edad <= 18
  )
df <- df %>%
  filter(estu_depto_reside == "TOLIMA") 

# ======================
# Recategorizaciones
# ======================
# Edad
df <- df %>%
  mutate(
    estu_edad = case_when(
      estu_edad %in% c(15, 16) ~ "Edad.15a16",
      estu_edad %in% c(17, 18) ~ "Edad.17a18"
    ),
    estu_edad = factor(estu_edad, levels = c("Edad.15a16", "Edad.17a18"), ordered = TRUE)
  )
# Tiene Vehiculo
df <- df %>%
  mutate(
    tiene_vehiculo = case_when(
      fami_tienemotocicleta == "Si" | fami_tieneautomovil == "Si" ~ "VEH.Sí",
      fami_tienemotocicleta == "No" & fami_tieneautomovil == "No" ~ "VEH.No",
      TRUE ~ NA_character_
    ),
    tiene_vehiculo = factor(tiene_vehiculo, levels = c("VEH.No", "VEH.Sí"))
  ) %>%
  dplyr::select(-fami_tieneautomovil, -fami_tienemotocicleta)

# Maximo nivel educativo padres
edu_levels <- c(
  "Ninguno", "Primaria incompleta", "Primaria completa",
  "Secundaria (Bachillerato) incompleta", "Secundaria (Bachillerato) completa",
  "Técnica o tecnológica incompleta", "Técnica o tecnológica completa",
  "Educación profesional incompleta", "Educación profesional completa",
  "Postgrado"
)

df <- df %>%
  mutate(
    across(c(fami_educacionpadre, fami_educacionmadre),
           ~na_if(.x, "No sabe")),
    across(c(fami_educacionpadre, fami_educacionmadre),
           ~na_if(.x, "No Aplica")),
    
    fami_educacionpadre = factor(fami_educacionpadre, levels = edu_levels, ordered = TRUE),
    fami_educacionmadre = factor(fami_educacionmadre, levels = edu_levels, ordered = TRUE),
    
    edu_max_padres = pmax(fami_educacionpadre, fami_educacionmadre, na.rm = TRUE)
  ) %>%
  select(-fami_educacionpadre, -fami_educacionmadre)

df <- df %>%
  mutate(
    edu_max_padres = case_when(
      edu_max_padres %in% edu_levels[1:4] ~ "Edu.Primaria",
      edu_max_padres == edu_levels[5]     ~ "Edu.Bachiller",
      edu_max_padres %in% edu_levels[6:8] ~ "Edu.Técnico",
      edu_max_padres %in% edu_levels[9:10]~ "Edu.Profesional"
    ),
    edu_max_padres = factor(
      edu_max_padres,
      levels = c("Edu.Primaria", "Edu.Bachiller", "Edu.Técnico", "Edu.Profesional"),
      ordered = TRUE
    )
  )
# Hacinamiento
map_cuartos <- c("Uno"=1,"Dos"=2,"Tres"=3,"Cuatro"=4,"Cinco"=5,"Seis o mas"=6)
map_personas <- c("1 a 2"=2,"3 a 4"=4,"5 a 6"=6,"7 a 8"=8,"9 o más"=9)

df <- df %>%
  mutate(
    n_cuartos   = map_cuartos[as.character(fami_cuartoshogar)],
    n_personas = map_personas[as.character(fami_personashogar)],
    hacinamiento = if_else(n_personas / n_cuartos >= 3, "HAC.SI", "HAC.NO"),
    hacinamiento = factor(hacinamiento, levels = c("HAC.NO", "HAC.SI"))
  ) %>%
  select(-n_cuartos, -n_personas, -fami_cuartoshogar, -fami_personashogar)


# Recategorización multiples variables desbalanceadas

df <- df %>%
  mutate(
    trabaja_cat = if_else(estu_horassemanatrabaja == "0", "NoTrabaja", "Trabaja")
  ) %>%
  select(-estu_horassemanatrabaja)
df$trabaja_cat <- as.factor(df$trabaja_cat) 


df <- df %>%
  mutate(
    fami_estratovivienda = ifelse(fami_estratovivienda %in% c("Estrato 1", "Sin Estrato"), "Est.1-",
                                  ifelse(fami_estratovivienda == "Estrato 2", "Est.2",
                                         ifelse(fami_estratovivienda == "Estrato 3", "Est.3",
                                                ifelse(fami_estratovivienda %in% c("Estrato 4","Estrato 5", "Estrato 6"), "Est.4+", NA))))
  )
df$fami_estratovivienda <- factor(df$fami_estratovivienda, levels = c( "Est.1-", "Est.2","Est.3", "Est.4+"), ordered = TRUE)

df <- df %>%
  mutate(
    fami_comecarnepescadohuevo = case_when(
      `fami_comecarnepescadohuevo` %in% c("Nunca o rara vez comemos eso", "1 o 2 veces por semana") ~ "Carne.0a2",
      `fami_comecarnepescadohuevo` == "3 a 5 veces por semana" ~ "Carne.3a5",
      `fami_comecarnepescadohuevo` == "Todos o casi todos los días" ~ "Carne.diario",
      TRUE ~ as.character(`fami_comecarnepescadohuevo`)
    ),
    
    fami_numlibros = case_when(
      `fami_numlibros` %in% c("MÁS DE 100 LIBROS", "26 A 100 LIBROS") ~ "Lib.+25 libros",
      `fami_numlibros` == "11 A 25 LIBROS" ~ "Lib.11a25 libros",
      `fami_numlibros` == "0 A 10 LIBROS" ~ "Lib.0a10 libros",
      TRUE ~ as.character(`fami_numlibros`)
    ),
    
    estu_dedicacionlecturadiaria = case_when(
      `estu_dedicacionlecturadiaria` %in% c("No leo por entretenimiento", "30 minutos o menos") ~ "Lee.0a30minutos",
      `estu_dedicacionlecturadiaria` == "Entre 30 y 60 minutos" ~ "Lee.30a60min",
      `estu_dedicacionlecturadiaria` == "Entre 1 y 2 horas" ~ "Lee.1a2horas",
      `estu_dedicacionlecturadiaria` == "Más de 2 horas" ~ "Lee.2horas+",
      TRUE ~ as.character(`estu_dedicacionlecturadiaria`)
    ),
    
    estu_dedicacioninternet = case_when(
      `estu_dedicacioninternet` %in% c("No Navega Internet", "30 minutos o menos") ~ "Net.30Minutos-",
      `estu_dedicacioninternet` == "Entre 30 y 60 minutos" ~ "Net.30a60min",
      `estu_dedicacioninternet` == "Entre 1 y 3 horas" ~ "Net.1a3horas",
      `estu_dedicacioninternet` == "Más de 3 horas" ~ "Net.3horas+",
      TRUE ~ as.character(`estu_dedicacioninternet`)
    ),
    estu_genero = case_when(
      `estu_genero` == "F" ~ "Sex.F",
      `estu_genero` == "M" ~ "Sex.M",
      TRUE ~ as.character(`estu_genero`)
    ),
    fami_situacioneconomica = case_when(
      `fami_situacioneconomica` == "Igual" ~ "Situ.Igual",
      `fami_situacioneconomica` == "Mejor" ~ "Situ.Mejor",
      `fami_situacioneconomica` == "Peor" ~ "Situ.Peor",
      TRUE ~ as.character(`fami_situacioneconomica`)
    ),
    fami_tienecomputador = case_when(
      `fami_tienecomputador` == "No" ~ "Compu.No",
      `fami_tienecomputador` == "Si" ~ "Compu.Si",
      TRUE ~ as.character(`fami_tienecomputador`)
    )
  )

# Puntaje global en cuartiles
cuartil <- function(x) {
  cut(
    x,
    breaks = quantile(x, probs = seq(0,1,0.25), na.rm = TRUE),
    include.lowest = TRUE,
    labels = c("Glo.Bajo","Glo.MBajo","Glo.MAlto","Glo.Alto")
  )
}

df <- df %>%
  mutate(glo = cuartil(punt_global))

# Tipo de colegio
df <- df %>%
  mutate(
    cole_tipo = case_when(
      cole_area_ubicacion == "URBANO" & cole_naturaleza == "OFICIAL"    ~ "Urbano/Oficial",
      cole_area_ubicacion == "URBANO" & cole_naturaleza == "NO OFICIAL" ~ "Urbano/No_Oficial",
      cole_area_ubicacion == "RURAL"  & cole_naturaleza == "OFICIAL"    ~ "Rural/Oficial"
    ),
    cole_tipo = factor(
      cole_tipo,
      levels = c("Rural/Oficial", "Urbano/Oficial", "Urbano/No_Oficial")
    )
  ) %>%
  filter(!is.na(cole_tipo))


# Drop de nas
df <- na.omit(df)
summary(df)

# ======================
# Exportación
# ======================
write.table(
  df,
  file      = "../data/processed/ICFES2024-Tolima.txt",
  sep       = "\t",
  row.names = FALSE,
  quote     = FALSE
)

