# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
library(stringr)

# 1. Filtrado y preparación de datos
datos_tipo_academia <- datos %>%
  # Quitamos los países que no tienen datos en estas variables
  filter(!is.na(tipo_academia_es), !is.na(GIRAI))

# 2. Generación del gráfico de Boxplots Horizontales
grafico_boxplot_tipo_academia <- ggplot(datos_tipo_academia, 
       aes(x = GIRAI, y = reorder(str_wrap(tipo_academia_es, width = 30), GIRAI, FUN = median))) +
  
  # Cajas ordenadas por mediana, usando el color asignado a la academia
  geom_boxplot(fill = "seagreen", alpha = 0.6, outlier.shape = NA) +
  
  # Capa de dispersión para visibilizar el tamaño de la muestra (n) por categoría
  geom_jitter(color = "gray30", alpha = 0.4, size = 1.5, height = 0.1) +
  
  theme_minimal() +
  
  labs(
    title = "Desempeño del Índice GIRAI según el Tipo de Iniciativa Académica",
    subtitle = "Análisis bivariado ordenado por la mediana del puntaje global",
    x = "Puntaje Global GIRAI (0 - 100)",
    y = NULL
  ) +
  
  theme(
    # Etiquetas compactas y alineadas a la izquierda para nombres largos
    axis.text.y = element_text(size = 9, color = "black", lineheight = 0.8, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank() 
  )

# 3. Imprimimos el gráfico
print(grafico_boxplot_tipo_academia)
