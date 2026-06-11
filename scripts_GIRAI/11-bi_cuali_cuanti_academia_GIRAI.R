# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Filtrado y preparación de datos
datos_academia_girai <- datos %>%
  filter(!is.na(academia), !is.na(GIRAI))

# 2. Cálculo e impresión de estadísticas descriptivas por grupo
estadisticas_academia <- datos_academia_girai %>%
  group_by(academia) %>%
  summarise(
    Promedio = round(mean(GIRAI, na.rm = TRUE), 1),
    Q1 = quantile(GIRAI, 0.25, na.rm = TRUE),
    Mediana = median(GIRAI, na.rm = TRUE),
    Q3 = quantile(GIRAI, 0.75, na.rm = TRUE)
  ) %>%
  # Ordenamos la tabla por la mediana de mayor a menor
  arrange(desc(Mediana))

# Imprimimos la tabla en la consola
print("Estadísticas de GIRAI según participación del sector académico:")
print(estadisticas_academia)

# 3. Generación del gráfico de Boxplots
grafico_boxplot_academia <- ggplot(datos_academia_girai) +
  
  # Capa de estéticas independiente
  aes(x = academia, y = GIRAI, fill = academia) +
  
  # Construimos las cajas, ocultando los outliers por defecto para que no se dupliquen
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  
  # Superponemos los datos reales con un leve temblor horizontal (width = 0.2)
  geom_jitter(color = "gray30", alpha = 0.5, size = 1.5, width = 0.2) +
  
  # Aplicamos la paleta cromática coherente con el análisis previo
  scale_fill_manual(values = c("No" = "tomato", "Sí" = "seagreen")) +
  
  # Temas base
  theme_minimal() +
  
  # Personalización del tema estandarizada
  theme(
    # Título centrado respecto a la imagen completa
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 20)),
    
    legend.position = "none", # Ocultamos la leyenda porque el eje X ya lo explica
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # Limpiamos el fondo para resaltar la distribución
    
    # Forzamos la visibilidad de las líneas de los ejes
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Fuente anclada al lienzo completo, a la izquierda (hjust = 0) y con sangría (l = 15)
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15, l = 15))
  ) +
  
  # Etiquetas: Título único descriptivo y fuente
  labs(
    title = "Distribución del puntaje GIRAI según la participación de la academia en IA",
    x = "Participación académica en IA",
    y = "GIRAI (0 - 100)",
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 4. Imprimimos el gráfico en consola
print(grafico_boxplot_academia)

# 5. Exportación parametrizada con ruta y nomenclatura estricta
ggsave(
  filename = "scripts_GIRAI/exports/bi_cuali_cuanti_academia_GIRAI.png",
  plot = grafico_boxplot_academia,
  width = 6.5,
  height = 4.2,
  units = "in",
  dpi = 300
)