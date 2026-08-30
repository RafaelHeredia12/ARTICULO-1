# ============================================================
# 03_dags_comparacion.R (CORREGIDO)
# ============================================================
# Objetivo: Proponer 3 DAGs teóricos, ejecutar Hill-Climbing,
# evaluar bondad de ajuste (BIC) y exportar las redes.
# ============================================================

library(bnlearn)
library(Rgraphviz)

df <- readRDS("data/processed/enmt_clean.rds")
datos_bn <- na.omit(df)
datos_bn[] <- lapply(datos_bn, function(x) if (is.factor(x)) factor(x) else x)
nodos <- names(datos_bn)

# DAG 1: Socioeconómico
dag1 <- empty.graph(nodos)
arcs(dag1) <- matrix(c(
  "edad_1", "escol", "sexo", "escol", "escol", "cond_act", 
  "escol", "ing_fam", "cond_act", "ing_fam", "ing_fam", "p6", 
  "Tam_loc_bin", "usa_tractor", "Tam_loc_bin", "medio_principal", 
  "ing_fam", "medio_principal", "dura1_cat", "medio_principal", 
  "p6", "p15_3", "sexo", "p15_3", "p6", "victima_delito"
), ncol = 2, byrow = TRUE)

# DAG 2: Infraestructura (CORREGIDO: Se conectó cond_act -> ing_fam)
dag2 <- empty.graph(nodos)
arcs(dag2) <- matrix(c(
  "Tam_loc_bin", "medio_principal", "Tam_loc_bin", "usa_tractor",
  "Tam_loc_bin", "ing_fam", "ing_fam", "p6", "p6", "dura1_cat",
  "dura1_cat", "medio_principal", "p6", "victima_delito",
  "sexo", "p15_3", "p6", "p15_3", "edad_1", "cond_act", "escol", "cond_act",
  "cond_act", "ing_fam" 
), ncol = 2, byrow = TRUE)

# DAG 3: Riesgo (CORREGIDO: Se conectó cond_act -> victima_delito)
dag3 <- empty.graph(nodos)
arcs(dag3) <- matrix(c(
  "p6", "victima_delito", "medio_principal", "victima_delito",
  "Tam_loc_bin", "victima_delito", "p6", "p15_3", "dura1_cat", "p15_3",
  "ing_fam", "p6", "ing_fam", "medio_principal", "edad_1", "escol",
  "sexo", "medio_principal", "escol", "ing_fam", "Tam_loc_bin", "usa_tractor",
  "cond_act", "victima_delito"
), ncol = 2, byrow = TRUE)

# Descubrimiento Algorítmico (Hill-Climbing con BLACKLIST)
# Prohíbe que cualquier variable sea causa de 'sexo' o 'edad_1'
bl_sexo <- data.frame(from = nodos[nodos != "sexo"], to = "sexo")
bl_edad <- data.frame(from = nodos[nodos != "edad_1"], to = "edad_1")
blacklist_causal <- rbind(bl_sexo, bl_edad)

dag_hc <- hc(datos_bn, blacklist = blacklist_causal)

# Guardar el DAG HC para usarlo en el script 4 sin recalcular
saveRDS(dag_hc, "data/processed/dag_hc.rds")

# Evaluación y Comparación (Score BIC)
score1 <- score(dag1, data = datos_bn, type = "bic")
score2 <- score(dag2, data = datos_bn, type = "bic")
score3 <- score(dag3, data = datos_bn, type = "bic")
score_hc <- score(dag_hc, data = datos_bn, type = "bic")

scores_df <- data.frame(
  Modelo = c("DAG 1 (Socioeconomico)", "DAG 2 (Infraestructura)", "DAG 3 (Riesgo)", "Hill-Climbing"),
  Score_BIC = c(score1, score2, score3, score_hc)
)

write.csv(scores_df, "data/processed/comparacion_dags.csv", row.names = FALSE)

# Exportar imágenes
png("article/dag2_infraestructura.png", width = 800, height = 600)
graphviz.plot(dag2, shape = "ellipse")
dev.off()

png("article/dag3_riesgo.png", width = 800, height = 600)
graphviz.plot(dag3, shape = "ellipse")
dev.off()

png("article/dag_hc.png", width = 800, height = 600)
graphviz.plot(dag_hc, shape = "ellipse")
dev.off()