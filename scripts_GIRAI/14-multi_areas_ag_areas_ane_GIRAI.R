# Cargar librerías
library(dplyr)
library(ggplot2)

# 1. Filtramos datos válidos
datos_ecosistema <- datos %>%
  filter(!is.na(areas_ane), !is.na(areas_ag), !is.na(GIRAI))

# 2. Generamos el gráfico de dispersión con Jitter y línea de paridad
grafico_ecosistema <- ggplot(datos_ecosistema, aes(x = areas_ane, y = areas_ag, color = GIRAI)) +
  
  # Usamos jitter en lugar de point para evitar superposición de números enteros
  geom_jitter(width = 0.3, height = 0.3, size = 3, alpha = 0.8) +
  
  # LA CLAVE DEL ANÁLISIS: Una línea diagonal exacta (y = x)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = 1) +
  
  # Una escala de colores continua que va de rojo (bajo GIRAI) a verde/azul (alto GIRAI)
  scale_color_viridis_c(option = "plasma", name = "Puntaje GIRAI") +
  
  theme_minimal() +
  labs(
    title = "Ecosistema de IA: Liderazgo Estatal vs. No Estatal",
    subtitle = "Intersección de áreas de trabajo y su impacto en el puntaje global",
    x = "Presencia de Actores No Estatales (areas_ane)",
    y = "Presencia de Actores Gubernamentales (areas_ag)"
  ) +
  
  # Agregamos anotaciones de texto para explicar los cuadrantes a tu jefe
  annotate("text", x = 1, y = max(datos_ecosistema$areas_ag) - 1, 
           label = "Dominio\nEstatal", color = "gray30", fontface = "italic") +
  annotate("text", x = max(datos_ecosistema$areas_ane) - 2, y = 1, 
           label = "Dominio\nNo Estatal", color = "gray30", fontface = "italic") +
  
  theme(legend.position = "right")

# 3. Imprimir el gráfico
print(grafico_ecosistema)