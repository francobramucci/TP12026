# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Limpieza preventiva
# Es buena práctica quitar los datos faltantes antes de hacer un gráfico de dispersión
datos_dispersion <- datos %>%
  filter(!is.na(areas_ane), !is.na(GIRAI))

# 2. Generación del Gráfico de Dispersión
grafico_dispersion_ane <- ggplot(datos_dispersion, aes(x = areas_ane, y = GIRAI)) +
  
  # Dibujamos los puntos. 
  # alpha = 0.6 los hace semi-transparentes para ver si hay muchos puntos superpuestos
  geom_point(color = "steelblue", alpha = 0.6, size = 3) +
  
  # Agregamos la línea de tendencia matemática (Linear Model = "lm")
  geom_smooth(method = "lm", color = "tomato", linetype = "dashed", se = FALSE, linewidth = 1) +
  
  theme_minimal() +
  
  labs(
    title = "Impacto de Actores No Estatales en el Puntaje General",
    subtitle = "Relación entre cantidad de áreas de intervención y el Índice GIRAI",
    x = "Cantidad de Áreas Temáticas (areas_ane)",
    y = "Puntaje Global GIRAI (0 - 100)"
  ) +
  
  theme(
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank() # Limpiamos la cuadrícula de fondo
  )

# 3. Imprimimos el gráfico
print(grafico_dispersion_ane)
