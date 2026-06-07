# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Preparación de los datos
datos_bastones_ane <- datos %>%
  # Filtramos los valores faltantes para evitar distorsiones
  filter(!is.na(areas_ane)) %>%
  # Contamos cuántos países tienen 0 áreas, 1 área, 2 áreas, etc.
  count(areas_ane)

# 2. Generación del gráfico de bastones
grafico_bastones_ane <- ggplot(datos_bastones_ane, aes(x = factor(areas_ane), y = n)) +
  
  # width = 0.3 afina la geometría para que visualmente sea un "bastón" y no una barra gruesa
  geom_col(fill = "steelblue", width = 0.3, alpha = 0.9) +
  
  # Agregamos la frecuencia absoluta (n) en la punta de cada bastón
  geom_text(aes(label = n), vjust = -0.8, size = 3.5, fontface = "bold") +
  
  theme_minimal() +
  
  # Expandimos el techo del gráfico un 15% para que los números no choquen con el borde
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title = "Distribución de Intervención de Actores No Estatales",
    subtitle = "Frecuencia de países según cantidad de áreas temáticas abordadas",
    x = "Cantidad de Áreas de Intervención (areas_ane)",
    y = "Frecuencia (Cantidad de Países)"
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # Limpiamos las líneas verticales para mayor nitidez
    axis.text = element_text(size = 11, color = "black")
  )

# 3. Imprimimos el gráfico
print(grafico_bastones_ane)
