# Análisis Probabilístico del Transporte en México mediante Redes Bayesianas

Este repositorio contiene el código fuente, preprocesamiento de datos, modelos probabilísticos y el artículo científico desarrollado para evaluar los patrones de movilidad, victimización y comportamiento de los usuarios en México a partir de los microdatos de la **Encuesta Nacional de Movilidad y Transporte (ENMT)** de la UNAM.

---

## 📌 Estructura del Repositorio

```text
.
├── article/                               # Visualizaciones exportadas de las redes bayesianas
│   ├── dag1_teorico.png                   # Representación gráfica del DAG Socioeconómico
│   ├── dag2_infraestructura.png            # Representación gráfica del DAG de Infraestructura
│   ├── dag3_riesgo.png                     # Representación gráfica del DAG de Riesgo y Comportamiento
│   └── dag_hc.png                         # Red óptima aprendida algorítmicamente (Hill-Climbing)
├── data/                                  # Gestión de datos del proyecto
│   ├── processed/                         # Datos procesados y tablas de resultados
│   │   ├── comparacion_dags.csv           # Métrica BIC y ranking comparativo de los 4 DAGs
│   │   ├── enmt_clean.csv                 # Dataset recodificado y limpio (formato CSV)
│   │   ├── enmt_clean.rds                 # Dataset limpio preservando tipos de datos en R (formato RDS)
│   │   ├── resultados_queries_DAG1.csv    # Inferencia de probabilidades estocásticas sobre DAG 1
│   │   └── resultados_queries_hc.csv      # Inferencia de probabilidades estocásticas sobre DAG HC
│   └── raw/                               # Microdatos originales sin alterar
│       ├── diccionario_enmt.xls           # Metadatos y definición de variables de la encuesta
│       └── enmt_unam.csv                  # Base de datos cruda de la ENMT
├── notebooks/                             # Publicación y reporte técnico
│   ├── ARTICULO1_RBM.html                 # Reporte final compilado interactivo (Web)
│   └── ARTICULO1_RBM.qmd                  # Código fuente del artículo científico en Quarto
├── scripts/                               # Código modular en R para el pipeline de análisis
│   ├── 01_preprocesamiento.R              # Limpieza, imputación de 'No Aplica' y creación de variables
│   ├── 02_dag_queries.R                   # Inferencia bayesiana base y prueba de independencia G²
│   ├── 03_dags_comparacion.R              # Construcción de DAGs teóricos, Hill-Climbing y score BIC
│   └── 04_queries_hc.R                    # Inferencia estocástica (Monte Carlo) en la red óptima
├── .gitignore                             # Archivos excluidos del control de versiones
├── ARTICULO-1.Rproj                       # Archivo de configuración del entorno RStudio
└── README.md                              # Documentación principal del repositorio