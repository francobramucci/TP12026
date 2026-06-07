# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Filtrado y preparación de los datos
datos_ddhh_girai <- datos %>%
  filter(!is.na(ddhh), !is.na(GIRAI))

# 2. Generación del gráfico de dispersión con línea de tendencia
grafico_dispersion_ddhh <- ggplot(datos_ddhh_girai, aes(x = ag, y = ane)) +
  
  # Dibujamos los puntos con algo de transparencia (alpha) para ver dónde se superponen
  geom_point(color = "steelblue", alpha = 0.6, size = 3) +
  
  # EL TOQUE PROFESIONAL: Agregamos la recta de regresión lineal y el sombreado de error estándar
  geom_smooth(method = "lm", color = "tomato", linetype = "dashed", fill = "gray80", alpha = 0.5) +
  
  theme_minimal() +
  
  labs(
    title = "Correlación: Derechos Humanos vs. Desempeño Global en IA",
    subtitle = "Análisis de dispersión con ajuste de regresión lineal (IC 95%)",
    x = "Puntaje en Dimensión de Derechos Humanos (ddhh)",
    y = "Puntaje Global GIRAI (0 - 100)"
  ) +
  
  theme(
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# 3. Imprimir el gráfico
print(grafico_dispersion_ddhh)
