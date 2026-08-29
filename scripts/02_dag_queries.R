# ============================================================
# 02_dag_queries.R
# ============================================================
# Objetivo: Construir la Red Bayesiana sobre la hipótesis
# Socioeconómica (DAG 1) y resolver las 4 queries asignadas.
# ============================================================

library(bnlearn)
library(dplyr)
library(Rgraphviz)

# 1. Cargar y preparar datos
df <- readRDS("data/processed/enmt_clean.rds")
datos_bn <- na.omit(df)
datos_bn[] <- lapply(datos_bn, function(x) if (is.factor(x)) factor(x) else x)

# 2. Definir DAG 1 (Teórico Socioeconómico)
dag <- empty.graph(names(datos_bn))
arcs(dag) <- matrix(c(
  "edad_1", "escol", "sexo", "escol", "escol", "cond_act", 
  "escol", "ing_fam", "cond_act", "ing_fam", "ing_fam", "p6", 
  "Tam_loc_bin", "usa_tractor", "Tam_loc_bin", "medio_principal", 
  "ing_fam", "medio_principal", "dura1_cat", "medio_principal", 
  "p6", "p15_3", "sexo", "p15_3", "p6", "victima_delito"
), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))

# 3. Ajuste Bayesiano (iss=10 suaviza el evento raro del tractor)
fitted <- bn.fit(dag, datos_bn, method = "bayes", iss = 10)

# 4. Resolución de Queries
# Q1: Tractor x Localidad (method = "lw" requiere evidencia como lista)
p_tractor_rural <- cpquery(fitted, event = (usa_tractor == "Sí"), evidence = list(Tam_loc_bin = "Menos de 15,000 hab."), method = "lw", n = 200000)
p_tractor_urbano <- cpquery(fitted, event = (usa_tractor == "Sí"), evidence = list(Tam_loc_bin = "15,000 hab. o más"), method = "lw", n = 200000)

# Q2: Claxon x Sexo (con auto)
p_claxon_h <- cpquery(fitted, event = (p15_3 %in% c("Siempre", "Casi siempre")), evidence = list(sexo = "Hombre", p6 = "Sí"), method = "lw", n = 100000)
p_claxon_m <- cpquery(fitted, event = (p15_3 %in% c("Siempre", "Casi siempre")), evidence = list(sexo = "Mujer", p6 = "Sí"), method = "lw", n = 100000)

# Q3: Delitos x Auto propio
test_delito <- ci.test("victima_delito", "p6", data = datos_bn)
p_delito_con <- cpquery(fitted, event = (victima_delito == "Sí"), evidence = list(p6 = "Sí"), method = "lw", n = 100000)
p_delito_sin <- cpquery(fitted, event = (victima_delito == "Sí"), evidence = list(p6 = "No"), method = "lw", n = 100000)

# Q4: Transporte x Nivel Socioeconómico y Tiempo (method = "ls" permite usar %in% en la evidencia)
p_pub_bajo_largo <- cpquery(fitted, event = (medio_principal == "Público masivo"), evidence = (ing_fam %in% c("<1 SM", "1-2 SM") & dura1_cat == "Largo (>40 min)"), method = "ls", n = 1000000)
p_auto_alto_corto <- cpquery(fitted, event = (medio_principal == "Automóvil particular"), evidence = (ing_fam %in% c("4-5 SM", "5-6 SM", ">5 SM") & dura1_cat == "Corto (≤20 min)"), method = "ls", n = 1000000)

# 5. Exportar Resultados y Gráficos
resultados <- data.frame(
  Query = c("Tractor_Rural", "Tractor_Urbano", "Claxon_Hombre", "Claxon_Mujer", "Delito_ConAuto", "Delito_SinAuto", "Pub_BajoLargo", "Auto_AltoCorto"),
  Probabilidad = c(p_tractor_rural, p_tractor_urbano, p_claxon_h, p_claxon_m, p_delito_con, p_delito_sin, p_pub_bajo_largo, p_auto_alto_corto)
)

write.csv(resultados, "data/processed/resultados_queries_DAG1.csv", row.names = FALSE)
print(test_delito) # Imprime en consola para que veas el p-value de Q3

# Crear el PNG de la red
png("article/dag1_teorico.png", width = 800, height = 600)
graphviz.plot(dag, shape = "ellipse", main = "DAG 1: Modelo Socioeconomico Base")
dev.off()