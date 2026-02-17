##############################
# Librerías y carga de datos
##############################
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(corrplot)
library(RColorBrewer)
library(psych)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(arules)
library(arulesViz)
library(igraph)


# Forzar directorio de trabajo a la ubicación del script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

##############################
# Carga de datos
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

# Selección de columnas de interés para análisis
df_sample <- df[,14:29]
summary(df_sample)

# Filtros opcionales
# df_sample <- df_sample %>% filter(cole_area_ubicacion == "RURAL")
# df_sample <- df_sample %>% filter(cole_naturaleza == "OFICIAL")

##############################
# Análisis de correspondencias múltiples (ACM / MCA)
##############################
ACM <- MCA(X = df_sample, graph = FALSE, ncp = 9)
ACM
summary(ACM)

# Contribuciones absolutas de cada variable en 5 dimensiones
p1 <- fviz_contrib(ACM, choice = "var", axes = 1, top = 15)
p2 <- fviz_contrib(ACM, choice = "var", axes = 2, top = 15)
p3 <- fviz_contrib(ACM, choice = "var", axes = 3, top = 15)
p4 <- fviz_contrib(ACM, choice = "var", axes = 4, top = 15)

grid.arrange(p1, p2, p3, p4, ncol = 2)

fviz_screeplot(ACM,
               labelsize = 0.1,
               addlabels = TRUE,
               barfill = "slategray1",
               line = 2.0)

corrplot(ACM$var$contrib, is.corr = FALSE)

# Contribución total por categoría (primeros 3 ejes)
contrib_total <- rowSums(ACM$var$contrib[, 1:3])
cos2_total <- rowSums(ACM$var$cos2[, 1:3])

df_contrib <- data.frame(
  categoria = names(contrib_total),
  contrib   = contrib_total,
  cos2      = cos2_total
)

# Detectar categorías poco informativas
df_contrib <- df_contrib %>%
  arrange(contrib) %>%
  mutate(flag_bajo_aporte = contrib < 1 & cos2 < 0.1)

# Revisar categorías irrelevantes
head(df_contrib[df_contrib$flag_bajo_aporte == TRUE, ])

##############################
# Función: Biplot MCA
##############################
graficar_biplot_mca <- function(data_col = NULL, axes = c(1,2), palette = "Set2") {
  
  data_name <- if (!is.null(data_col)) {
    expr <- deparse(substitute(data_col))
    expr <- sub(".*\\$", "", expr)
    expr <- sub("\\)$", "", expr)
    expr
  }
  
  titulo <- paste0("Plano ", axes[1], "-", axes[2], " del ACM - ", data_name)
  
  if (is.null(data_col)) {
    fviz_mca_biplot(
      X = ACM,
      axes = axes,
      title = paste("Plano", paste(axes, collapse = "-"), "del ACM"),
      labelsize = 5,
      repel = TRUE,
      col.var = "cos2",
      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
      label = "var",
      invisible = "ind",
      legend.title = "Categorias"
    )
  } else {
    fviz_mca_biplot(
      X = ACM,
      palette = palette,
      col.ind = data_col,
      axes = axes,
      title = titulo,
      title.orientation = "center",
      labelsize = 5,
      repel = TRUE,
      col.var = "gray0",
      label = "var",
      addEllipses = FALSE,
      fill.ind = data_col,
      alpha.ind = 0.6
    ) +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 14)
      ) +
      labs(
        x = paste0("Dim ", axes[1]),
        y = paste0("Dim ", axes[2]),
        color = paste0(data_name, ":")
      )
  }
}

##############################
# Graficar biplots ACM
##############################
graficar_biplot_mca(axes = c(1,2))
graficar_biplot_mca(df$glo, palette = c("#FB9A99", "#FDBF6F", "#B2DF8A", "#A6CEE3"))
graficar_biplot_mca(as.factor(df$estu_nse_individual), palette = "Accent")
graficar_biplot_mca(as.factor(df$cole_tipo), palette = "Set3")
graficar_biplot_mca(df_sample$cole_naturaleza, palette = c("green", "blue"))

##############################
# Cluster jerárquico + K-means (HCPC)
##############################
res.hcpc <- HCPC(ACM, nb.clust = -1, graph = TRUE)

fviz_cluster(res.hcpc,
             repel = FALSE,
             show_labels = FALSE,
             show.clust.cent = FALSE,
             palette = "Set2",
             ggtheme = theme_minimal(),
             main = "Factor map | Clusters",
             axes = c(1,2))

# Revisar descripciones de clusters
print(res.hcpc$desc.var)
print(res.hcpc$desc.axes)
head(res.hcpc$desc.var)

# Agregar columna de cluster al dataframe
df$clust <- res.hcpc$data.clust$clust

# Graficar biplots por cluster
graficar_biplot_mca(as.factor(df$clust), axes = c(1,2), palette = "Set4")
graficar_biplot_mca(as.factor(df$clust), axes = c(1,3), palette = "Set4")

##############################
# Análisis de reglas de asociación por cluster
##############################
colors <- c("#F8766D", "#00BA38", "#619CFF")

# Filtrar cada cluster
df_1 <- df %>% filter(clust == "1")
df_2 <- df %>% filter(clust == "2")
df_3 <- df %>% filter(clust == "3")

# Seleccionar columnas categóricas para transacciones
df_1 <- df_1[,14:29]
df_2 <- df_2[,14:29]
df_3 <- df_3[,14:29]

trx <- df_1
#trx$hacinamiento <- NULL
#trx$estu_genero <- NULL

# Convertir a transacciones
trx <- as(trx, "transactions")
itemLabels(trx) <- gsub("^.*=", "", itemLabels(trx))

# Resumen de transacciones
summary(trx)
itemFrequency(trx, type = "absolute")[1:20]

# Graficar categorías más frecuentes
itemFrequencyPlot(trx, topN = 20, 
                  type = "absolute", 
                  col = colors[1],
                  border = NA,
                  main = "Características más Frecuentes del Cluster 3")

##############################
# Generación de reglas de asociación
##############################
reglas <- apriori(
  trx,
  parameter = list(support = 0.01, confidence = 0.7, minlen = 2, maxlen = 4),
  appearance = list(rhs = c("Glo.Alto", "Glo.Bajo", 
                            "Glo.MBajo", "Glo.MAlto"), default = "lhs")
)

reglas
arules::inspect(head(sort(reglas, by = "lift"), 20))

##############################
# Visualización de reglas
##############################
subrules <- head(sort(reglas, by = "lift"), 10)
set.seed(420)

# Gráfico coordenadas paralelas
plot(subrules, method = "paracoord", control = list(reorder = TRUE))

# Gráfico de red con igraph
plot(subrules, method = "graph",
     engine = "igraph",
     control = list(layout = with_gem()),
     main = "")

# Alternativa: visNetwork
# plot(head(subrules, 10), method = "graph", engine = "visNetwork")
