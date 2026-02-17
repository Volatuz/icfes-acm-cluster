# =====================================================================
# PRUEBAS ESTADÍSTICAS – ANÁLISIS SABER 11 (TOLIMA, 2024)
# Autor: German Pava
# Este script realiza análisis descriptivos, pruebas de normalidad,
# contrastes no paramétricos, pruebas post hoc y asociaciones mediante
# chi-cuadrado, con los datos limpios: ../data/processed/ICFES2024-Tolima.txt" .
# Ademas de exportar una tabla de pruebas a ../output
# =====================================================================


# ---------------------------------------------------------------------
# Librerías y carga de datos
# ---------------------------------------------------------------------

library(dplyr)       # manipulación de datos
library(ggplot2)     # histogramas
library(rstatix)     # wilcox, kruskal, tamaños de efecto
library(openxlsx)    # exportación a Excel
library(FSA)         # dunnTest
library(DescTools)   # CramerV
library(vcd)         # residuales estandarizados chi-cuadrado


# Establecer el directorio de trabajo en la ubicación del script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Carga de la base de datos
df <- read.table(
  "../data/processed/ICFES2024-Tolima.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = TRUE
)

# Conversión sistemática de variables a factores
df[] <- lapply(df, function(col) {
  if (!is.factor(col)) as.factor(col) else col
})
df$punt_global <- as.numeric(as.character(df$punt_global))
#punt_global a double


# ---------------------------------------------------------------------
# Análisis descriptivo y evaluación de normalidad
# ---------------------------------------------------------------------

# Histograma del puntaje global
hist(
  df$punt_global,
  breaks = 40,
  col = "gray80",
  main = "Distribución del puntaje global",
  xlab = "Puntaje global"
)

# Pruebas de Normalidad
set.seed(420) # Para que el resultado sea reproducible
shapiro.test(sample(df$punt_global, 500))
# Resultado: W = 0.98971, p-value = 0.00142

# Al ser el p-valor < 0.05, se rechaza la hipótesis nula de normalidad; 
# los puntajes globales no siguen una distribución normal perfecta, sugiriendo 
# el uso de pruebas no paramétricas para comparaciones de grupos.


# ---------------------------------------------------------------------
# Pruebas no paramétricas (Wilcoxon(Independientes) / Kruskal–Wallis)
# con estimación de tamaño de efecto
# ---------------------------------------------------------------------


# Wilcoxon rank-sum test (Equivalente a Mann–Whitney; ver ?wilcox.test)
wilcox.test(punt_global ~ cole_area_ubicacion, data = df)

# Tamaño de efecto r para Mann–Whitney (ver ?wilcox_effsize)
wilcox_effsize(punt_global ~ cole_area_ubicacion, data = df)



# Kruskal
kruskal.test(punt_global ~ edu_max_padres, data = df)
kruskal_effsize(punt_global ~ edu_max_padres, data = df)


# --------------------------------
# Tabla de estas pruebas para todas las variables
# --------------------------------
vars_cat <- colnames(df[, 16:30])

analisis_no_param <- function(data, var_cat) {
  
  data <- data %>% filter(!is.na(.data[[var_cat]]))
  k <- n_distinct(data[[var_cat]])
  
  # Resumen descriptivo por categoría
  resumen <- data %>%
    group_by(.data[[var_cat]]) %>%
    summarise(
      n = n(),
      Mediana = median(punt_global),
      Q1 = quantile(punt_global, 0.25),
      Q3 = quantile(punt_global, 0.75),
      .groups = "drop"
    ) %>%
    rename(Categoria = 1)
  
  f <- as.formula(paste("punt_global ~", var_cat))
  
  if (k == 2) {
    # Prueba de Mann–Whitney (Wilcoxon para independientes)
    test <- wilcox.test(f, data = data)
    efecto <- wilcox_effsize(data, f)
    
    resumen %>%
      mutate(
        Variable = var_cat,
        Prueba = "Mann–Whitney",  # Cambio de nombre con el que etiqueta la tabla
        Estadistico = test$statistic,
        gl = NA,
        p_value = test$p.value,
        Tamaño_efecto = efecto$effsize
      )
    
  } else {
    # Prueba de Kruskal–Wallis
    test <- kruskal.test(f, data = data)
    efecto <- kruskal_effsize(data, f)
    
    resumen %>%
      mutate(
        Variable = var_cat,
        Prueba = "Kruskal-Wallis",
        Estadistico = test$statistic,
        gl = test$parameter,
        p_value = test$p.value,
        Tamaño_efecto = efecto$effsize
      )
  }
}

# Consolidación de resultados
tabla_anexo <- bind_rows(
  lapply(vars_cat, function(v) {
    analisis_no_param(df, v)
  })
)

# Exportación de resultados
openxlsx::write.xlsx(
  tabla_anexo,
  file = "../output/Anexo_Kruskal_Mann–Whitney_PuntajeGlobal_Saber11_Tolima.xlsx",
  rowNames = FALSE
)


# ---------------------------------------------------------------------
# Pruebas post hoc (Dunn con corrección de Bonferroni)
# ---------------------------------------------------------------------

# Nivel educativo máximo de los padres
df$edu_max_padres <- as.factor(as.character(df$edu_max_padres))

dunnTest(
  punt_global ~ edu_max_padres,
  data = df,
  method = "bonferroni"
)

# Requiere ejecutar los cluster, que es donde se crea dicha columna
# Comparaciones entre clusters
#kruskal.test(punt_global ~ clust, data = df)
#kruskal_effsize(df, punt_global ~ clust)

#dunnTest(
#  punt_global ~ clust,
#  data = df,
#  method = "bonferroni"
#)


# ---------------------------------------------------------------------
# Pruebas de independencia (Chi-cuadrado) y tamaño de efecto
# ---------------------------------------------------------------------

# Disparidad de recursos tecnológicos
tbl <- table(df$fami_tienecomputador, df$cole_area_ubicacion)
chisq.test(tbl)
chisq.test(tbl)$stdres
CramerV(tbl)

# Capital cultural del hogar
tbl <- table(df$fami_tienecomputador, df$edu_max_padres)
chisq.test(tbl)
chisq.test(tbl)$stdres
CramerV(tbl)

# Género y condición laboral del estudiante
tbl <- table(df$estu_genero, df$trabaja_cat)
chisq.test(tbl)
chisq.test(tbl)$stdres
CramerV(tbl)
