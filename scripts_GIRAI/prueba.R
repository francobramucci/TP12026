library(dplyr)
library(ggplot2)

# Filtramos los países que sí tienen un dato válido en la columna 'academia'
datos_analisis <- datos %>%
  filter(!is.na(academia))

# Generamos el gráfico
grafico_academia <- ggplot(datos_analisis, aes(x = academia, y = GIRAI, fill = academia)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Impacto de la academia en el Índice de IA Responsable",
    subtitle = "Distribución del puntaje GIRAI según la presencia de la academia",
    x = "¿Hay presencia de academia en IA?",
    y = "Puntaje Global GIRAI (0-100)"
  ) +
  theme(legend.position = "none") # Quitamos la leyenda porque el eje X ya lo explica

# Mostramos el gráfico
print(grafico_academia)
