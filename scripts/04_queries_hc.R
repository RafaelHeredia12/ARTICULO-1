# ============================================================
# 04_queries_hc.R
# ============================================================
# Objetivo: Construir la Red Bayesiana sobre la topología óptima
# descubierta por Hill-Climbing y recalcular las 4 queries.
# ============================================================

library(bnlearn)

df <- readRDS("data/processed/enmt_clean.rds")
datos_bn <- na.omit(df)
datos_bn[] <- lapply(datos_bn, function(x) if (is.factor(x)) factor(x) else x)

# Cargar el DAG Hill-Climbing generado en el script 03
dag_hc <- readRDS("data/processed/dag_hc.rds")

# Ajuste Bayesiano
fitted_hc <- bn.fit(dag_hc, datos_bn, method = "bayes", iss = 10)

# Q1: Tractor
p_tractor_rural <- cpquery(fitted_hc, event = (usa_tractor == "Sí"), evidence = list(Tam_loc_bin = "Menos de 15,000 hab."), method = "lw", n = 200000)
p_tractor_urbano <- cpquery(fitted_hc, event = (usa_tractor == "Sí"), evidence = list(Tam_loc_bin = "15,000 hab. o más"), method = "lw", n = 200000)

# Q2: Claxon
p_claxon_h <- cpquery(fitted_hc, event = (p15_3 %in% c("Siempre", "Casi siempre")), evidence = list(sexo = "Hombre", p6 = "Sí"), method = "lw", n = 100000)
p_claxon_m <- cpquery(fitted_hc, event = (p15_3 %in% c("Siempre", "Casi siempre")), evidence = list(sexo = "Mujer", p6 = "Sí"), method = "lw", n = 100000)

# Q3: Delitos (Prueba estadística)
test_delito <- ci.test("victima_delito", "p6", data = datos_bn)
p_delito_con <- cpquery(fitted_hc, event = (victima_delito == "Sí"), evidence = list(p6 = "Sí"), method = "lw", n = 100000)
p_delito_sin <- cpquery(fitted_hc, event = (victima_delito == "Sí"), evidence = list(p6 = "No"), method = "lw", n = 100000)

# Q4: Socioeconómico y Tiempo
p_pub_bajo_largo <- cpquery(fitted_hc, event = (medio_principal == "Público masivo"), evidence = (ing_fam %in% c("<1 SM", "1-2 SM") & dura1_cat == "Largo (>40 min)"), method = "ls", n = 1000000)
p_auto_alto_corto <- cpquery(fitted_hc, event = (medio_principal == "Automóvil particular"), evidence = (ing_fam %in% c("4-5 SM", "5-6 SM", ">5 SM") & dura1_cat == "Corto (≤20 min)"), method = "ls", n = 1000000)

resultados_hc <- data.frame(
  Query = c("Tractor_Rural", "Tractor_Urbano", "Claxon_Hombre", "Claxon_Mujer", "Delito_ConAuto", "Delito_SinAuto", "Pub_BajoLargo", "Auto_AltoCorto"),
  Probabilidad = c(p_tractor_rural, p_tractor_urbano, p_claxon_h, p_claxon_m, p_delito_con, p_delito_sin, p_pub_bajo_largo, p_auto_alto_corto)
)

write.csv(resultados_hc, "data/processed/resultados_queries_hc.csv", row.names = FALSE)