
#INSTALAR LIBRERÍAS
# install.packages(c("igraph", "ggraph"))

library(igraph)
library(ggraph)
library(ggplot2)
library(grid) 
library(bnlearn)

# 1. Cargar datos y revivir el objeto
df <- readRDS("data/processed/enmt_clean.rds")
datos_bn <- na.omit(df)
datos_bn[] <- lapply(datos_bn, function(x) if (is.factor(x)) factor(x) else x)
dag_hc <- hc(datos_bn)

# 2. Convertir a grafo
grafo_hc <- bnlearn::as.igraph(dag_hc)

# 3. Diseñar el gráfico (Versión Pro y Proporcional)
plot_hc <- ggraph(grafo_hc, layout = 'sugiyama') +
  geom_edge_link(
    arrow = arrow(length = unit(5, 'mm'), type = "closed"), 
    end_cap = circle(10, 'mm'), # Alejamos la flecha para que no pise el nodo grande
    color = "#8da0cb", 
    width = 1.2
  ) +
  geom_node_point(size = 24, color = "#fc8d62", alpha = 0.95) + # Círculos mucho más grandes
  geom_node_text(aes(label = name), repel = TRUE, size = 6.5, fontface = "bold", color = "#222222") + # Texto más grande
  theme_void() +
  ggtitle("Estructura Óptima (Hill-Climbing)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22, margin = margin(b = 25)),
    plot.background = element_rect(fill = "white", color = NA), # Fondo 100% blanco
    panel.background = element_rect(fill = "white", color = NA)
  )

# 4. Mostrar en pantalla y guardar en disco
print(plot_hc)
ggsave("article/dag_hc_pro.png", plot = plot_hc, width = 12, height = 8, dpi = 300)


