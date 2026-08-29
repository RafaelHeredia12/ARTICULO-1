# ARTICULO-1

# Análisis Probabilístico del Transporte en México mediante Redes Bayesianas

Este repositorio contiene el código fuente, preprocesamiento de datos, modelos probabilísticos y el artículo científico desarrollado para evaluar los patrones de movilidad, victimización y comportamiento de los usuarios en México a partir de la **Encuesta Nacional de Movilidad y Transporte (ENMT)** de la UNAM.

---

## 📌 Estructura del Repositorio

```text
.
├── 01_preprocesamiento.R      # Limpieza de microdatos, imputación estructural y feature engineering
├── 02_dag_queries.R           # Ajuste de Red Bayesiana, inferencia estocástica (Queries 1 a 4) y prueba G²
├── 03_dags_comparacion.R      # Construcción de 3 DAGs teóricos, Hill-Climbing y evaluación con Score BIC
├── ARTICULO1_RBM.qmd          # Artículo científico en formato Quarto (HTML / PDF)
├── ARTICULO1_RBM.html         # Versión compilada interactiva del artículo
├── data/
│   ├── raw/                   # Microdatos originales de la ENMT (enmt_unam.csv)
│   └── processed/             # Datos limpios y tablas de resultados (enmt_clean.rds, resultados_queries.csv)
└── article/                   # Gráficos de las redes exportados (dag1_teorico.png, dag_hc.png, etc.)