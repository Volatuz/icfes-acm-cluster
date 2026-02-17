
# ---------------------------------------------------------------------
# Librerías y carga de datos
# ---------------------------------------------------------------------
library(WDI)
library(ggplot2)
library(dplyr)
library(corrplot)
library(readxl)
library(openxlsx)
library(lavaan)
library(MASS)
library(RColorBrewer)
library(psych)
library(FactoMineR)
library(factoextra)
library(scales)
library(readr)

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
df$punt_global <- as.numeric(as.character(df$punt_global)) #punt_global a double



## Funcion descriptivos
# Exporta
output.folder <- "../output/"

graficos_descriptivos <- function(dfDesc, folder, fill) {
  require(ggplot2)
  require(dplyr)
  require(grid)
  
  output_file <- paste0(folder, "DescriptivosBy_", fill, "_Tolima.pdf")
  pdf(output_file, height = 5)
  # Clasificación de variables
  columnas_factor <- sapply(dfDesc, is.factor)
  nombres_categoricas <- names(dfDesc)[columnas_factor]
  
  # --- Variables categóricas ---
  for (name in nombres_categoricas) {
    
    # Barras simples
    p5 <- ggplot(dfDesc, aes_string(x = name)) + 
      geom_bar(fill = "darkcyan") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3.5) +
      xlab(name) + ylab("Frecuencia") +
      ggtitle(paste("Distribución de", name)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    p8 <- ggplot(dfDesc, aes_string(x = name, fill = fill)) + 
      geom_bar(position = "dodge") +
      xlab(name) + ylab("Frecuencia") +
      ggtitle(paste("Distribución de", name, "por", fill)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p8)
    
    # Gráfico de barras: media de PUNT_GLOBAL por categoría
    if ("punt_global" %in% names(dfDesc)) {
      resumen <- dfDesc %>%
        group_by(.data[[name]]) %>%
        summarise(Media_Puntaje_Global = mean(as.numeric(as.character(punt_global)), na.rm = TRUE)) %>%
        arrange(Media_Puntaje_Global) %>%
        mutate(Grupo = factor(.data[[name]], levels = .data[[name]]))  # Orden explícito ascendente
      
      p7 <- ggplot(resumen, aes(x = Grupo, y = Media_Puntaje_Global)) +
        geom_col(fill = "steelblue") +
        xlab(name) + ylab("Media PUNT_GLOBAL") +
        ggtitle(paste("Media de PUNT_GLOBAL por", name)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      print(p7)
    }
    
  }
  
  dev.off()
}
graficos_descriptivos(df = df, folder = output.folder, fill = "cole_tipo")
graficos_descriptivos(df = df, folder = output.folder, fill = "edu_max_padres")
#graficos_descriptivos(df = df, folder = output.folder, fill = "cole_area_ubicacion")
#graficos_descriptivos(df = df, folder = output.folder, fill = "estu_nse_individual")
#graficos_descriptivos(df = df, folder = output.folder, fill = "glo")
#graficos_descriptivos(df = df, folder = output.folder, fill = "cole_naturaleza")


# Datos para tabla
tab_edad_genero <- table(df$estu_edad, df$estu_genero)
tab_edad_genero

#### Barras simples de cole_naturaleza
df %>% 
  count(cole_naturaleza) %>% 
  ggplot(aes(
    x = reorder(cole_naturaleza, n),
    y = n,
    fill = cole_naturaleza
  )) +
  geom_col() +
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = NULL,
    y = "Número de estudiantes",
    title = "Naturaleza del establecimiento educativo",
    fill = "Naturaleza del establecimiento"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.98, 0.02),   
    legend.justification = c(1, 0),    
    legend.background = element_rect(fill = "white", color = NA)
  )


#### Barras simples de cole_area_ubicacion
df %>% 
  count(cole_area_ubicacion) %>% 
  ggplot(aes(
    x = reorder(cole_area_ubicacion, n),
    y = n,
    fill = cole_area_ubicacion
  )) +
  geom_col() +
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = NULL,
    y = "Número de estudiantes",
    title = "Área de ubicación del establecimiento",
    fill = "Área"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.98, 0.02),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = NA)
  )


###########
#
########## Violines
df$edu_max_padres <- factor(df$edu_max_padres, 
                            levels = c("Edu.Primaria", "Edu.Bachiller", "Edu.Técnico", "Edu.Profesional"), 
                            ordered = TRUE)

p_violin_box <- ggplot(df, aes(x = edu_max_padres, y = punt_global, fill = edu_max_padres)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1,
               outlier.shape = 16,
               outlier.size  = 1.8,
               outlier.alpha = 0.75,
               fill = "white", color = "black") +
  #geom_hline(yintercept = 257, linetype = "dashed", color = "red", size = 0.7) +
  #geom_hline(yintercept = 300, linetype = "dashed", color = "black", size = 0.7) +
  #   scale_fill_manual(values = c("#BA6B3D", "#BCB4BA", "#855195", "gray")) +
  scale_fill_manual(values = c("#C4682F", "#F4CC65", "#B68B9E", "#C3CBCD")) +
  coord_cartesian(ylim = c(100, 450)) +
  xlab("edu_max_padres") +
  ylab("Puntaje global") +
  
  ggtitle("Distribución de puntajes por la educación maxima alcanzada por los padres.") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

p_violin_box +
  stat_summary(
    fun = function(x) quantile(x, probs = 0.25),
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    hjust = -0.6,
    size = 3,
    color = "black"
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    hjust = -0.6,
    size = 3.2,
    fontface = "bold",
    color = "black"
  ) +
  stat_summary(
    fun = function(x) quantile(x, probs = 0.75),
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    hjust = -0.6,
    size = 3,
    color = "black"
  )


# p_violin_box

## Violin 2

df$cole_area_ubicacion <- factor(df$cole_area_ubicacion, 
                                 levels = c("RURAL", "URBANO"), 
                                 ordered = TRUE)

p_violin_box <- ggplot(df, aes(x = cole_area_ubicacion, y = punt_global, fill = cole_area_ubicacion)) +
  geom_violin(trim = FALSE, alpha = 1) +
  geom_boxplot(width = 0.1,
               outlier.shape = 16,
               outlier.size  = 1.8,
               outlier.alpha = 0.75,
               fill = "white", color = "black") +
  #geom_hline(yintercept = 257, linetype = "dashed", color = "red", size = 0.7) +
  #geom_hline(yintercept = 300, linetype = "dashed", color = "black", size = 0.7) +
  #   scale_fill_manual(values = c("#BA6B3D", "#BCB4BA", "#855195", "gray")) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62")) +
  coord_cartesian(ylim = c(100, 450)) +
  xlab("cole_area_ubicacion") +
  ylab("Puntaje global") +
  
  ggtitle("Distribución de puntajes por la educación maxima alcanzada por los padres.") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

p_violin_box +
  stat_summary(
    fun = function(x) quantile(x, probs = 0.25),
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    hjust = -0.6,
    size = 3,
    color = "black"
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    hjust = -0.6,
    size = 3.2,
    fontface = "bold",
    color = "black"
  ) +
  stat_summary(
    fun = function(x) quantile(x, probs = 0.75),
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    hjust = -0.6,
    size = 3,
    color = "black"
  )

# Violin 3

df$cole_naturaleza <- factor(df$cole_naturaleza, 
                             levels = c("OFICIAL", "NO OFICIAL"), 
                             ordered = TRUE)

p_violin_box <- ggplot(df, aes(x = cole_naturaleza, y = punt_global, fill = cole_naturaleza)) +
  geom_violin(trim = FALSE, alpha = 1) +
  geom_boxplot(width = 0.1,
               outlier.shape = 16,
               outlier.size  = 1.8,
               outlier.alpha = 0.75,
               fill = "white", color = "black") +
  #geom_hline(yintercept = 257, linetype = "dashed", color = "red", size = 0.7) +
  #geom_hline(yintercept = 300, linetype = "dashed", color = "black", size = 0.7) +
  #   scale_fill_manual(values = c("#BA6B3D", "#BCB4BA", "#855195", "gray")) +
  scale_fill_manual(values = c("#377EB8", "#E41A1C")) +
  coord_cartesian(ylim = c(100, 450)) +
  xlab("cole_naturaleza") +
  ylab("Puntaje global") +
  
  ggtitle("Distribución de puntajes por la educación maxima alcanzada por los padres.") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

p_violin_box +
  stat_summary(
    fun = function(x) quantile(x, probs = 0.25),
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    hjust = -0.6,
    size = 3,
    color = "black"
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    hjust = -0.6,
    size = 3.2,
    fontface = "bold",
    color = "black"
  ) +
  stat_summary(
    fun = function(x) quantile(x, probs = 0.75),
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    hjust = -0.6,
    size = 3,
    color = "black"
  )

#graficos de barras dobles
ggplot(df)+
  geom_bar(mapping = aes(x = clust, fill = clust), stat = "count")


ggplot(df, aes(
  x = cole_area_ubicacion,
  fill = fami_tienecomputador
)) +
  geom_bar(position = "fill") +
  #coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Área de ubicación del establecimiento",
    y = "Proporción",
    fill = "Tiene computador"
  ) +
  theme_minimal()+ 
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

ggplot(df, aes(
  x = edu_max_padres,
  fill = fami_tienecomputador
)) +
  geom_bar(position = "fill") +
  #coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Educación Maxima Alcanzada por los Padres",
    y = "Proporción",
    fill = "Tiene computador"
  ) +
  theme_minimal()+ 
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )


#barras apiladas
#edu_max_padres
#fami_tienecomputador
#glo

ggplot(df)+
  geom_bar(mapping = aes(x = clust, fill = glo), position="fill")+
  coord_flip() +
  labs(x = NULL) +
  labs(y = NULL) +
  scale_y_reverse() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal")


ggplot(df)+
  geom_bar(mapping = aes(x = clust, fill = fami_tienecomputador), position="fill")

ggplot(df)+
  geom_bar(mapping = aes(x = clust, fill = glo), position="fill")


ggplot(df)+
  geom_bar(mapping = aes(x = clust, fill = fami_tienecomputador), position="fill")
ggplot(df)+
  geom_bar(mapping = aes(x = clust, fill = cole_tipo), position="fill")



ggplot(df)+
  geom_bar(mapping = aes(x = clust, fill = edu_max_padres), position="dodge")


#####
######


df$clust <- as.factor(df$clust) 
#df$punt_global <- as.double(df$punt_global)

p_violin_box2 <- ggplot(df, aes(x = clust, y = punt_global, fill = clust)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1,
               outlier.shape = 16,
               outlier.size  = 1.8,
               outlier.alpha = 0.75,
               fill = "white", color = "black") +
  #geom_hline(yintercept = 257, linetype = "dashed", color = "red", size = 0.7) +
  #geom_hline(yintercept = 300, linetype = "dashed", color = "black", size = 0.7) +
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) +
  xlab("Cluster") +
  ylab("Puntaje global") +
  ggtitle("Distribución de puntajes por Cluster.") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)   # <-- centra el título
  )

p_violin_box2 +
  stat_summary(
    fun = function(x) quantile(x, probs = 0.25),
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    hjust = -0.6,
    size = 3,
    color = "black"
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    hjust = -0.6,
    size = 3.2,
    fontface = "bold",
    color = "black"
  ) +
  stat_summary(
    fun = function(x) quantile(x, probs = 0.75),
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    hjust = -0.6,
    size = 3,
    color = "black"
  )


p_violin_box2


df %>%
  count(clust) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ggplot(aes(x = 2, y = n, fill = factor(clust))) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(
    aes(label = paste0(n, "\n", pct, "%")),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  labs(fill = "Cluster") +
  theme_void()

