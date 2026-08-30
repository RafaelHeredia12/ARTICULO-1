# ============================================================
# 03_dags_comparacion.R
# ============================================================
# Objetivo: Proponer 3 DAGs teóricos, ejecutar Hill-Climbing,
# evaluar bondad de ajuste (BIC) y exportar las redes.
# ============================================================

library(bnlearn)
library(Rgraphviz)

# 1. Cargar datos
df <- readRDS("data/processed/enmt_clean.rds")
datos_bn <- na.omit(df)
datos_bn[] <- lapply(datos_bn, function(x) if (is.factor(x)) factor(x) else x)
nodos <- names(datos_bn)

# 2. Definir los 3 DAGs Teóricos
# DAG 1: Socioeconómico (mismo que usamos para inferencia)
dag1 <- empty.graph(nodos)
arcs(dag1) <- matrix(c(
  "edad_1", "escol", "sexo", "escol", "escol", "cond_act", 
  "escol", "ing_fam", "cond_act", "ing_fam", "ing_fam", "p6", 
  "Tam_loc_bin", "usa_tractor", "Tam_loc_bin", "medio_principal", 
  "ing_fam", "medio_principal", "dura1_cat", "medio_principal", 
  "p6", "p15_3", "sexo", "p15_3", "p6", "victima_delito"
), ncol = 2, byrow = TRUE)

# DAG 2: Infraestructura (Tamaño de localidad como raíz principal)
dag2 <- empty.graph(nodos)
arcs(dag2) <- matrix(c(
  "Tam_loc_bin", "medio_principal", "Tam_loc_bin", "usa_tractor",
  "Tam_loc_bin", "ing_fam", "ing_fam", "p6", "p6", "dura1_cat",
  "dura1_cat", "medio_principal", "p6", "victima_delito",
  "sexo", "p15_3", "p6", "p15_3", "edad_1", "cond_act", "escol", "cond_act"
), ncol = 2, byrow = TRUE)

# DAG 3: Riesgo (Enfoque en delitos y claxon)
dag3 <- empty.graph(nodos)
arcs(dag3) <- matrix(c(
  "p6", "victima_delito", "medio_principal", "victima_delito",
  "Tam_loc_bin", "victima_delito", "p6", "p15_3", "dura1_cat", "p15_3",
  "ing_fam", "p6", "ing_fam", "medio_principal", "edad_1", "escol",
  "sexo", "medio_principal", "escol", "ing_fam", "Tam_loc_bin", "usa_tractor"
), ncol = 2, byrow = TRUE)

# 3. Descubrimiento Algorítmico (Hill-Climbing)
dag_hc <- hc(datos_bn)

# 4. Evaluación y Comparación (Score BIC)
score1 <- score(dag1, data = datos_bn, type = "bic")
score2 <- score(dag2, data = datos_bn, type = "bic")
score3 <- score(dag3, data = datos_bn, type = "bic")
score_hc <- score(dag_hc, data = datos_bn, type = "bic")

scores_df <- data.frame(
  Modelo = c("DAG 1 (Socioeconomico)", "DAG 2 (Infraestructura)", "DAG 3 (Riesgo)", "Hill-Climbing"),
  Score_BIC = c(score1, score2, score3, score_hc)
)

# Exportar tabla comparativa
write.csv(scores_df, "data/processed/comparacion_dags.csv", row.names = FALSE)

# 5. Exportar los 3 DAGs restantes como imágenes
png("article/dag2_infraestructura.png", width = 800, height = 600)
graphviz.plot(dag2, shape = "ellipse", main = "DAG 2: Modelo de Infraestructura")
dev.off()

png("article/dag3_riesgo.png", width = 800, height = 600)
graphviz.plot(dag3, shape = "ellipse", main = "DAG 3: Modelo de Riesgo")
dev.off()

png("article/dag_hc.png", width = 800, height = 600)
graphviz.plot(dag_hc, shape = "ellipse", main = "Algoritmo Hill-Climbing")
dev.off()
