# ============================================================
# 04_queries_hc.R
# ============================================================
# Objetivo: Construir la Red Bayesiana sobre la topología óptima
# descubierta por Hill-Climbing y recalcular las 4 queries.
# ============================================================

library(bnlearn)
library(dplyr)

# 1. Cargar y preparar datos
df <- readRDS("data/processed/enmt_clean.rds")
datos_bn <- na.omit(df)
datos_bn[] <- lapply(datos_bn, function(x) if (is.factor(x)) factor(x) else x)

# 2. Descubrimiento Algorítmico (Hill-Climbing)
# Aquí la máquina define las flechas basándose 100% en los datos
dag_hc <- hc(datos_bn)

# 3. Ajuste Bayesiano sobre la red óptima
fitted_hc <- bn.fit(dag_hc, datos_bn, method = "bayes", iss = 10)

# 4. Resolución de Queries con el nuevo modelo
# Q1: Tractor x Localidad (method = "lw" requiere evidencia como lista)
p_tractor_rural_hc <- cpquery(fitted_hc, event = (usa_tractor == "Sí"), evidence = list(Tam_loc_bin = "Menos de 15,000 hab."), method = "lw", n = 200000)
p_tractor_urbano_hc <- cpquery(fitted_hc, event = (usa_tractor == "Sí"), evidence = list(Tam_loc_bin = "15,000 hab. o más"), method = "lw", n = 200000)

# Q2: Claxon x Sexo (con auto)
p_claxon_h_hc <- cpquery(fitted_hc, event = (p15_3 %in% c("Siempre", "Casi siempre")), evidence = list(sexo = "Hombre", p6 = "Sí"), method = "lw", n = 100000)
p_claxon_m_hc <- cpquery(fitted_hc, event = (p15_3 %in% c("Siempre", "Casi siempre")), evidence = list(sexo = "Mujer", p6 = "Sí"), method = "lw", n = 100000)

# Q3: Delitos x Auto propio
p_delito_con_hc <- cpquery(fitted_hc, event = (victima_delito == "Sí"), evidence = list(p6 = "Sí"), method = "lw", n = 100000)
p_delito_sin_hc <- cpquery(fitted_hc, event = (victima_delito == "Sí"), evidence = list(p6 = "No"), method = "lw", n = 100000)

# Q4: Transporte x Nivel Socioeconómico y Tiempo
p_pub_bajo_largo_hc <- cpquery(fitted_hc, event = (medio_principal == "Público masivo"), evidence = (ing_fam %in% c("<1 SM", "1-2 SM") & dura1_cat == "Largo (>40 min)"), method = "ls", n = 1000000)
p_auto_alto_corto_hc <- cpquery(fitted_hc, event = (medio_principal == "Automóvil particular"), evidence = (ing_fam %in% c("4-5 SM", "5-6 SM", ">5 SM") & dura1_cat == "Corto (≤20 min)"), method = "ls", n = 1000000)

# 5. Exportar Resultados del modelo Hill-Climbing
resultados_hc <- data.frame(
  Query = c("Tractor_Rural", "Tractor_Urbano", "Claxon_Hombre", "Claxon_Mujer", "Delito_ConAuto", "Delito_SinAuto", "Pub_BajoLargo", "Auto_AltoCorto"),
  Probabilidad_HC = c(p_tractor_rural_hc, p_tractor_urbano_hc, p_claxon_h_hc, p_claxon_m_hc, p_delito_con_hc, p_delito_sin_hc, p_pub_bajo_largo_hc, p_auto_alto_corto_hc)
)

write.csv(resultados_hc, "data/processed/resultados_queries_hc.csv", row.names = FALSE)
print("Resultados usando Hill-Climbing generados con éxito:")
print(resultados_hc)
