# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
library(stringr)

# 1. Filtrado y preparación de datos
datos_filtrados <- datos %>%
  filter(!is.na(tipo_privado_es), !is.na(GIRAI))

# 2. Generación del gráfico de Boxplots Horizontales
# Ordenamos las categorías en el eje Y según la mediana del índice GIRAI
grafico_boxplot_tipo <- ggplot(datos_filtrados, 
       aes(x = GIRAI, y = reorder(str_wrap(tipo_privado_es, width = 30), GIRAI, FUN = median))) +
  
  # Dibujamos las cajas (ocultamos los outliers por defecto para que no se dupliquen con el jitter)
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.shape = NA) +
  
  # Superponemos los puntos con un leve "temblor" para ver la densidad de países
  geom_jitter(color = "gray30", alpha = 0.4, size = 1.5, height = 0.1) +
  
  theme_minimal() +
  
  labs(
    title = "Desempeño del Índice GIRAI según el Tipo de Iniciativa Privada",
    subtitle = "Análisis bivariado ordenado por la mediana del puntaje global",
    x = "Puntaje Global GIRAI (0 - 100)",
    y = NULL
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank() # Limpiamos líneas horizontales para resaltar las cajas
  )

# 3. Imprimimos el gráfico en la pestaña Plots
print(grafico_boxplot_tipo)
