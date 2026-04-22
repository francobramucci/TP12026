# Cargar librerías
library(dplyr)
library(ggplot2)

# 1. Calculamos la media y la mediana para agregarlas al gráfico (Data Wrangling)
estadisticas_girai <- datos %>%
  summarise(
    media = mean(GIRAI, na.rm = TRUE),
    mediana = median(GIRAI, na.rm = TRUE)
  )

media_global <- estadisticas_girai$media
mediana_global <- estadisticas_girai$mediana

# 2. Generamos el Histograma
grafico_girai_general <- ggplot(datos, aes(x = GIRAI)) +
  # Usamos bins = 12 para dividir los 100 puntos posibles en 12 barras
  geom_histogram(fill = "steelblue", color = "white", bins = 12, alpha = 0.8) +
  
  # EL TOQUE PROFESIONAL: Una línea punteada roja marcando la mediana mundial
  geom_vline(xintercept = mediana_global, linetype = "dashed", color = "tomato", linewidth = 1) +
  
  # Agregamos un texto dinámico para que diga el valor exacto de la mediana
  annotate("text", x = mediana_global + 3, y = 12, 
           label = paste("Mediana Mundial:", round(mediana_global, 1)), 
           color = "tomato", fontface = "bold", hjust = 0) +
  
  theme_minimal() +
  
  # Ajustamos los ejes para que vayan del 0 al 100 (ya que es un índice)
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  
  labs(
    title = "Distribución Global del Índice GIRAI",
    subtitle = "Análisis univariado del puntaje general de IA Responsable (n = 138 países)",
    x = "Puntaje GIRAI (0 - 100)",
    y = "Cantidad de Países"
  ) +
  theme(
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank()
  )

# 3. Imprimimos el gráfico
print(grafico_girai_general)