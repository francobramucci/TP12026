library(dplyr)
library(ggplot2)

# Filtramos los países que sí tienen un dato válido en la columna 'privado'
datos_analisis <- datos %>%
  filter(!is.na(privado))

# Generamos el gráfico
grafico_privado <- ggplot(datos_analisis, aes(x = privado, y = GIRAI, fill = privado)) +
  # 1. Dibujamos el boxplot, pero lo hacemos un poco transparente
  geom_boxplot(alpha = 0.5, outlier.shape = NA) + 
  
  # 2. Agregamos los puntos individuales con 'geom_jitter'
  # width = 0.2 hace que los puntos no formen una línea recta vertical, sino que se dispersen un poco
  geom_jitter(width = 0.2, alpha = 0.8, color = "darkgray") +
  
  theme_minimal() +
  labs(
    title = "Impacto del Sector Privado en el Índice de IA",
    subtitle = "Los puntos grises representan cada país individual",
    x = "¿Hay actores del sector privado trabajando en IA?",
    y = "Puntaje Global GIRAI"
  ) +
  theme(legend.position = "none") # Quitamos la leyenda porque el eje X ya lo explica

# Mostramos el gráfico
print(grafico_privado)
