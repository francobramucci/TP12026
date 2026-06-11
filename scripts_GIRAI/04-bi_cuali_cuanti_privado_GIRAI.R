# Cargar librerías
library(dplyr)
library(ggplot2)

# 1. Preparación de los datos
datos_analisis <- datos %>%
  # Filtramos NAs en ambas variables para evitar el warning de ggplot
  filter(!is.na(privado), !is.na(GIRAI))

# 2. Generación del gráfico de diagrama de caja (boxplot) con Jitter y Ejes
grafico_privado <- ggplot(datos_analisis) +
  
  # Capa de estéticas independiente
  aes(x = privado, y = GIRAI, fill = privado) +
  
  # Construcción de la geometría
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  
  # Capa de dispersión (Jitter) superpuesta
  geom_jitter(color = "gray30", alpha = 0.5, size = 1.5, width = 0.2) +
  
  # Mantenemos la consistencia cromática para el sector privado
  scale_fill_manual(values = c("No" = "tomato", "Sí" = "royalblue")) +
  
  # Temas base
  theme_minimal() +
  
  # Personalización del tema (título SIN negrita, alineado al centro)
  theme(
    # Título centrado respecto a la imagen completa
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 20)),
    
    legend.position = "none", # Ocultamos leyenda porque el eje X ya lo explica
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Fuente anclada al lienzo completo, a la izquierda (hjust = 0) y con sangría (l = 15)
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15, l = 15))
  ) +
  
  # Etiquetas: Único título descriptivo y fuente agregada
  labs(
    title = "Distribución de GIRAI según presencia del sector privado en IA",
    x = "Presencia del sector privado en IA",
    y = "GIRAI (0 - 100)",
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 3. Calcular e imprimir estadísticas en consola
estadisticas_privado <- datos_analisis %>%
  group_by(privado) %>%
  summarise(
    Q1 = quantile(GIRAI, 0.25, na.rm = TRUE),
    Mediana = median(GIRAI, na.rm = TRUE),
    Q3 = quantile(GIRAI, 0.75, na.rm = TRUE)
  )

print(estadisticas_privado)

# 4. Imprimir el gráfico en consola
print(grafico_privado)

# 5. Exportación parametrizada con ruta y nomenclatura estricta
ggsave(
  filename = "scripts_GIRAI/exports/bi_cuali_cuanti_privado_GIRAI.png",
  plot = grafico_privado,
  width = 6.5,
  height = 4.2,
  units = "in",
  dpi = 300
)