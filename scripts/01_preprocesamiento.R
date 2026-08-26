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

library(dplyr)

# ---- 1. Cargar datos crudos ---------------------------------------
raw <- read.csv(
  "data/raw/enmt_unam.csv",
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)

cat("Dimensiones originales:", dim(raw), "\n")

# ---- 2. Función auxiliar para limpiar códigos de missing -----------
# Convierte cualquier código de la lista `codes` a NA
clean_na <- function(x, codes) {
  x[x %in% codes] <- NA
  x
}

df <- raw

# ---- 3. Variables sociodemográficas --------------------------------

df$sexo <- clean_na(df$sexo, c(8, 9))
df$sexo <- factor(df$sexo, levels = c(1, 2), labels = c("Hombre", "Mujer"))

df$edad_1 <- clean_na(df$edad_1, c(-1))
df$edad_1 <- factor(
  df$edad_1, levels = 1:7,
  labels = c("Menos de 15", "15 a 24", "25 a 34", "35 a 44",
             "45 a 54", "55 a 64", "65 y más"),
  ordered = TRUE
)

df$escol <- clean_na(df$escol, c(8, 9))
df$escol <- factor(
  df$escol, levels = 1:5,
  labels = c("Ninguna", "Primaria", "Secundaria",
             "Preparatoria/Bachillerato", "Universidad/Posgrado"),
  ordered = TRUE
)

df$cond_act <- clean_na(df$cond_act, c(-1))
df$cond_act <- factor(df$cond_act, levels = c(1, 2),
                      labels = c("Sí trabaja", "No trabaja"))

# ing_fam: 1 = NS/NC, 988888 = NS, 999999 = NC, 0 = código no documentado
# (probablemente missing del sistema SPSS) -> todos a NA
df$ing_fam <- clean_na(df$ing_fam, c(1, 988888, 999999, 0))
df$ing_fam <- factor(
  df$ing_fam, levels = 2:8,
  labels = c("<1 SM", "1-2 SM", "2-3 SM", "3-4 SM",
             "4-5 SM", "5-6 SM", ">5 SM"),
  ordered = TRUE
)

df$Region <- factor(df$Region, levels = 1:4,
                    labels = c("Centro", "Metropolitana", "Norte", "Sur"))

# Tamaño de localidad: versión completa (4 categorías) y binaria
# (la binaria es la que usa la query 1, corte exacto en 15,000 hab.)
df$Tam_loc_cat <- factor(
  df$Tam_loc, levels = 1:4,
  labels = c("100000+", "15000-99999", "2500-14999", "1-2499")
)
df$Tam_loc_bin <- factor(
  ifelse(df$Tam_loc %in% c(3, 4), "Menos de 15,000 hab.", "15,000 hab. o más")
)

# ---- 4. Percepción del transporte público (p17_*) -------------------
# Ya son binarias, sin códigos NS/NC documentados en el diccionario
df$p17_1 <- factor(df$p17_1, levels = c(1, 2), labels = c("Eficiente", "Ineficiente"))
df$p17_4 <- factor(df$p17_4, levels = c(1, 2), labels = c("Seguro", "Inseguro"))

# ---- 5. Uso de transporte (p1a_1 a p1a_22) ---------------------------
p1a_cols <- paste0("p1a_", 1:22)
for (col in p1a_cols) {
  df[[col]] <- clean_na(df[[col]], c(8, 9))
}

# Categoría de política pública para cada uno de los 22 modos
categoria_modo <- c(
  p1a_1  = "Público masivo",       # Tren
  p1a_2  = "Público masivo",       # Tren urbano (metro, suburbano, ligero)
  p1a_3  = "Público masivo",       # Transporte eléctrico
  p1a_4  = "Público masivo",       # Camión/microbús
  p1a_5  = "Público masivo",       # Colectivo
  p1a_6  = "Público masivo",       # Autobús foráneo
  p1a_7  = "Público masivo",       # BRT
  p1a_8  = "Taxi/Plataforma",      # Taxi
  p1a_9  = "Taxi/Plataforma",      # Bicitaxi/mototaxi
  p1a_10 = "Público masivo",       # Transporte escolar/personal
  p1a_11 = "Otro",                 # Avión
  p1a_12 = "Automóvil particular", # Automóvil particular
  p1a_13 = "Otro",                 # Tractor
  p1a_14 = "Otro",                 # Tráiler
  p1a_15 = "No motorizado",        # Motocicleta/cuatrimoto
  p1a_16 = "No motorizado",        # Bicicleta/triciclo
  p1a_17 = "No motorizado",        # Patines/patineta
  p1a_18 = "No motorizado",        # Tracción animal
  p1a_19 = "No motorizado",        # Animal
  p1a_20 = "Otro",                 # Helicóptero
  p1a_21 = "Otro",                 # Embarcación mayor
  p1a_22 = "Otro"                  # Embarcación menor
)

# ---- 6. Construir medio_principal ------------------------------------
# Score: Cotidianamente = 2, Ocasionalmente = 1, Nunca/NA = 0
# Se elige la categoría con score máximo (regla de desempate para
# quienes marcaron varios modos como uso cotidiano)
score_matrix <- sapply(p1a_cols, function(col) {
  ifelse(df[[col]] == 1, 2, ifelse(df[[col]] == 2, 1, 0))
})
score_matrix[is.na(score_matrix)] <- 0

medio_idx <- apply(score_matrix, 1, function(row) {
  if (all(row == 0)) return(NA)   # nadie marcó ningún modo -> NA
  which.max(row)
})

df$medio_principal <- ifelse(
  is.na(medio_idx), NA,
  categoria_modo[medio_idx]
)
df$medio_principal <- factor(
  df$medio_principal,
  levels = c("Automóvil particular", "Público masivo",
             "Taxi/Plataforma", "No motorizado", "Otro")
)

# ---- 7. Tiempo de viaje (dura1) ---------------------------------------
# dura1 es la única duración con datos completos para los 1191
# encuestados; dura2-4 corresponden a viajes adicionales que la
# mayoría no realizó, así que no se usan.
df$dura1_cat <- cut(
  df$dura1,
  breaks = c(-Inf, 20, 40, Inf),
  labels = c("Corto (≤20 min)", "Medio (21-40 min)", "Largo (>40 min)"),
  right = TRUE
)

# ---- 8. Auto propio y percepción de contaminación (p6, p19) ----------
df$p6 <- clean_na(df$p6, c(98, 99))
df$p6 <- factor(df$p6, levels = c(1, 2), labels = c("Sí", "No"))

df$p19 <- clean_na(df$p19, c(98, 99))
df$p19 <- factor(
  df$p19, levels = 1:4,
  labels = c("No contamina", "Contamina poco",
             "Contamina algo", "Contamina mucho"),
  ordered = TRUE
)

# ---- 9. Uso excesivo del claxon (p15_3) -------------------------------
# -1 y 97 = "no aplica" (no tiene auto), NO es lo mismo que "Nunca"
df$p15_3 <- clean_na(df$p15_3, c(-1, 97, 98, 99))
df$p15_3 <- factor(
  df$p15_3, levels = 1:4,
  labels = c("Siempre", "Casi siempre", "Casi nunca", "Nunca"),
  ordered = TRUE
)

# ---- 10. Incidencia delictiva agregada (p25_1_1 a p25_1_7) -----------
# Cada delito individual es muy raro por separado (máx. 92 casos);
# se agrega en una sola binaria: ¿fue víctima de algún delito?
delito_cols <- paste0("p25_1_", 1:7)
for (col in delito_cols) {
  df[[col]] <- clean_na(df[[col]], c(98, 99))
}
df$victima_delito <- apply(df[delito_cols], 1, function(row) {
  if (all(is.na(row))) return(NA)
  any(row == 1, na.rm = TRUE)
})
df$victima_delito <- factor(df$victima_delito, levels = c(FALSE, TRUE),
                            labels = c("No", "Sí"))

# ---- 11. Selección final de variables ---------------------------------
df_final <- df %>%
  select(
    folio, sexo, edad_1, escol, cond_act, ing_fam, Region,
    Tam_loc_cat, Tam_loc_bin,
    p17_1, p17_4,
    medio_principal, dura1_cat,
    p6, p19, p15_3,
    victima_delito
  )

# ---- 12. Guardar dataset limpio ----------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(df_final, "data/processed/enmt_clean.rds")
write.csv(df_final, "data/processed/enmt_clean.csv", row.names = FALSE, na = "")

# ---- 13. Resumen de verificación ----------------------------------------
cat("\nDimensiones finales:", dim(df_final), "\n\n")
cat("Resumen de variables limpias:\n")
summary(df_final)

cat("\n% de NA por variable:\n")
sapply(df_final, function(x) round(100 * mean(is.na(x)), 1))
