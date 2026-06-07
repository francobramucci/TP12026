# Cargar librerías
library(dplyr)
library(ggplot2)

# 1. Calculo la mediana para agregarla al gráfico
estadisticas_girai <- datos %>%
  summarise(
    mediana = median(GIRAI, na.rm = TRUE)
  )

mediana_global <- estadisticas_girai$mediana

# 2. Genero el Histograma
grafico_girai_general <- ggplot(datos) +
  aes(x = GIRAI) +
  
  # Divido en 12 barras
  geom_histogram(fill = "steelblue", color = "white", bins = 12, alpha = 0.8) +
  
  # Marco la mediana mundial
  geom_vline(xintercept = mediana_global, linetype = "dashed", color = "tomato", linewidth = 1) +
  
  # Indico valor de la mediana global
  annotate("text", x = mediana_global + 3, y = 30, 
           label = paste("Mediana Mundial:", round(mediana_global, 1)), 
           color = "tomato", fontface = "bold", hjust = 0) +
  
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(xlim = c(0, 100)) +
  
  # Temas
  theme_minimal() +
  theme(
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank(),
    
    # Líneas de eje
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Formato de la fuente
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15))
  ) +
  
  # Etiquetas
  labs(
    title = "Distribución Global del Índice GIRAI",
    x = "GIRAI (0-100)",
    y = "Cantidad de Países",
    # Caption de la fuente
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# Imprimo el gráfico
print(grafico_girai_general)

# Calculo percentil para el valor 1
percentil_menor_1 <- datos %>%
  summarise(
    porcentaje = mean(GIRAI < 1) * 100
  ) %>%
  # Extraigo el número exacto
  pull(porcentaje)

# Imprimo el resultado con un texto descriptivo
print(paste0("El ", round(percentil_menor_1, 1), "% de los países tiene un puntaje GIRAI menor a 1."))

# Imprimimos el gráfico en RStudio (opcional, solo para verlo)
print(grafico_girai_general)

# Exportación parametrizada de alta calidad
ggsave(
  filename = "scripts_GIRAI/exports/uni_continua_GIRAI.png", # O ".pdf"
  plot = grafico_girai_general,         # El nombre de la variable de tu gráfico
  width = 6.5,                          # Ancho en pulgadas
  height = 4.2,                         # Alto en pulgadas
  units = "in",                         # Unidad: "in" (inches/pulgadas)
  dpi = 300                             # Resolución de imprenta
)


