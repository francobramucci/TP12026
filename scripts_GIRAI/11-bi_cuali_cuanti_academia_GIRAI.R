# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Filtrado y preparación de datos
datos_academia_girai <- datos %>%
  filter(!is.na(academia), !is.na(GIRAI))

# 2. Generación del gráfico de Boxplots
grafico_boxplot_academia <- ggplot(datos_academia_girai, aes(x = academia, y = GIRAI, fill = academia)) +
  
  # Construimos las cajas, ocultando los outliers por defecto para que no se dupliquen
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  
  # Superponemos los datos reales con un leve temblor horizontal (width = 0.2)
  geom_jitter(color = "gray30", alpha = 0.5, size = 1.5, width = 0.2) +
  
  # Aplicamos la paleta cromática coherente con el análisis previo
  scale_fill_manual(values = c("No" = "tomato", "Sí" = "seagreen")) +
  
  theme_minimal() +
  
  labs(
    title = "Impacto del Sector Académico en el Desempeño Global de IA",
    subtitle = "Distribución del puntaje GIRAI según la participación de la academia",
    x = "Participación Académica",
    y = "Puntaje Global GIRAI (0 - 100)"
  ) +
  
  theme(
    legend.position = "none", # Ocultamos la leyenda porque el eje X ya lo explica
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank() # Limpiamos el fondo para resaltar la distribución
  )

# 3. Imprimimos el gráfico
print(grafico_boxplot_academia)
