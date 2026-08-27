# ============================================================
# Preprocesamiento ENMT-UNAM para Redes Bayesianas Multinomiales
# Grupo 101 - Análisis de métodos de razonamiento e incertidumbre
# AD 2026 - Dr. Javier E. Garrido Guillén
# ============================================================
# Este script:
#   1. Carga los datos crudos (Latin-1)
#   2. Limpia los códigos de NS/NC (varían por variable)
#   3. Construye las variables derivadas necesarias para las
#      4 queries asignadas y los 3 DAGs
#   4. Guarda un dataset limpio en data/processed/
# ============================================================

# ============================================================
# 01_preprocesamiento.R
# ============================================================
library(dplyr)

raw <- read.csv("data/raw/enmt_unam.csv", fileEncoding = "latin1", stringsAsFactors = FALSE)

clean_na <- function(x, codes) {
  x[x %in% codes] <- NA
  x
}

df <- raw

# Sociodemográficas
df$sexo <- clean_na(df$sexo, c(8, 9))
df$sexo <- factor(df$sexo, levels = c(1, 2), labels = c("Hombre", "Mujer"))

df$edad_1 <- clean_na(df$edad_1, c(-1))
df$edad_1 <- factor(df$edad_1, levels = 1:7, labels = c("Menos de 15", "15 a 24", "25 a 34", "35 a 44", "45 a 54", "55 a 64", "65 y más"), ordered = TRUE)

df$escol <- clean_na(df$escol, c(8, 9))
df$escol <- factor(df$escol, levels = 1:5, labels = c("Ninguna", "Primaria", "Secundaria", "Preparatoria/Bachillerato", "Universidad/Posgrado"), ordered = TRUE)

df$cond_act <- clean_na(df$cond_act, c(-1))
df$cond_act <- factor(df$cond_act, levels = c(1, 2), labels = c("Sí trabaja", "No trabaja"))

df$ing_fam <- clean_na(df$ing_fam, c(1, 988888, 999999, 0))
df$ing_fam <- factor(df$ing_fam, levels = 2:8, labels = c("<1 SM", "1-2 SM", "2-3 SM", "3-4 SM", "4-5 SM", "5-6 SM", ">5 SM"), ordered = TRUE)

df$Tam_loc_bin <- factor(ifelse(df$Tam_loc %in% c(3, 4), "Menos de 15,000 hab.", "15,000 hab. o más"))

# Uso de transporte (p1a_1 a p1a_22)
p1a_cols <- paste0("p1a_", 1:22)
for (col in p1a_cols) { df[[col]] <- clean_na(df[[col]], c(8, 9)) }

categoria_modo <- c(
  p1a_1="Público masivo", p1a_2="Público masivo", p1a_3="Público masivo", 
  p1a_4="Público masivo", p1a_5="Público masivo", p1a_6="Público masivo", 
  p1a_7="Público masivo", p1a_8="Taxi/Plataforma", p1a_9="Taxi/Plataforma", 
  p1a_10="Público masivo", p1a_11="Otro", p1a_12="Automóvil particular", 
  p1a_13="Otro", p1a_14="Otro", p1a_15="No motorizado", p1a_16="No motorizado", 
  p1a_17="No motorizado", p1a_18="No motorizado", p1a_19="No motorizado", 
  p1a_20="Otro", p1a_21="Otro", p1a_22="Otro"
)

score_matrix <- sapply(p1a_cols, function(col) { ifelse(df[[col]] == 1, 2, ifelse(df[[col]] == 2, 1, 0)) })
score_matrix[is.na(score_matrix)] <- 0
medio_idx <- apply(score_matrix, 1, function(row) { if (all(row == 0)) return(NA); which.max(row) })
df$medio_principal <- factor(ifelse(is.na(medio_idx), NA, categoria_modo[medio_idx]), levels = c("Automóvil particular", "Público masivo", "Taxi/Plataforma", "No motorizado", "Otro"))

df$usa_tractor <- factor(ifelse(df$p1a_13 %in% c(1, 2), "Sí", "No"), levels = c("No", "Sí"))
df$usa_tractor[is.na(df$p1a_13)] <- NA

df$dura1_cat <- cut(df$dura1, breaks = c(-Inf, 20, 40, Inf), labels = c("Corto (≤20 min)", "Medio (21-40 min)", "Largo (>40 min)"), right = TRUE)

df$p6 <- clean_na(df$p6, c(98, 99))
df$p6 <- factor(df$p6, levels = c(1, 2), labels = c("Sí", "No"))

# Claxon: Mantener 'No aplica' para no perder a la población sin auto
df$p15_3 <- clean_na(df$p15_3, c(99))
df$p15_3 <- factor(
  ifelse(df$p15_3 %in% c(-1, 97), "No aplica", df$p15_3),
  levels = c("1", "2", "3", "4", "No aplica"),
  labels = c("Siempre", "Casi siempre", "Casi nunca", "Nunca", "No aplica")
)

# Incidencia delictiva agregada
delito_cols <- paste0("p25_1_", 1:7)
for (col in delito_cols) { df[[col]] <- clean_na(df[[col]], c(98, 99)) }
df$victima_delito <- factor(apply(df[delito_cols], 1, function(row) { if (all(is.na(row))) return(NA); any(row == 1, na.rm = TRUE) }), levels = c(FALSE, TRUE), labels = c("No", "Sí"))

# Guardar
df_final <- df %>% select(sexo, edad_1, escol, cond_act, ing_fam, Tam_loc_bin, medio_principal, dura1_cat, p6, p15_3, victima_delito, usa_tractor)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(df_final, "data/processed/enmt_clean.rds")
write.csv(df_final, "data/processed/enmt_clean.csv", row.names = FALSE, na = "")