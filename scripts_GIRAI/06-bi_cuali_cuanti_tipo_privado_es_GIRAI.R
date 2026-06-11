# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)
library(stringr)

# 1. Filtrado y preparación de datos
datos_filtrados <- datos %>%
  filter(!is.na(tipo_privado_es), !is.na(GIRAI))

# 2. Cálculo e impresión de estadísticas descriptivas por grupo
estadisticas_tipo_privado <- datos_filtrados %>%
  group_by(tipo_privado_es) %>%
  summarise(
    Promedio = round(mean(GIRAI, na.rm = TRUE), 1), # Nuevo cálculo agregado
    Q1 = quantile(GIRAI, 0.25, na.rm = TRUE),
    Mediana = median(GIRAI, na.rm = TRUE),
    Q3 = quantile(GIRAI, 0.75, na.rm = TRUE)
  ) %>%
  # Ordenamos la tabla por la mediana de mayor a menor para facilitar la lectura
  arrange(desc(Mediana))

# Imprimimos la tabla en la consola
print("Estadísticas de GIRAI según tipo de iniciativa privada:")
print(estadisticas_tipo_privado)

# 3. Generación del gráfico de Boxplots Horizontales
grafico_boxplot_tipo <- ggplot(datos_filtrados) +
  
  # Capa de estéticas independiente (ordenamos categorías por la mediana)
  aes(x = GIRAI, y = reorder(str_wrap(tipo_privado_es, width = 30), GIRAI, FUN = median)) +
  
  # Dibujamos las cajas (ocultamos los outliers por defecto para que no se dupliquen con el jitter)
  geom_boxplot(fill = "royalblue", alpha = 0.6, outlier.shape = NA) +
  
  # Superponemos los puntos con un leve "temblor" vertical para ver la densidad de países
  geom_jitter(color = "gray30", alpha = 0.4, size = 1.5, height = 0.1) +
  
  # Temas base
  theme_minimal() +
  
  # Personalización del tema estandarizada
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 20)),
    
    # Texto de categorías y ejes
    axis.text.y = element_text(size = 9, color = "black", lineheight = 0.8),
    axis.text.x = element_text(size = 11, color = "black"),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), # Limpiamos líneas horizontales para resaltar las cajas
    
    # Forzamos la visibilidad de las líneas de los ejes
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Estilización simple de la fuente
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15))
  ) +
  
  # Etiquetas: Único título descriptivo y fuente
  labs(
    title = "Desempeño del índice GIRAI según \n el tipo de iniciativa privada más frecuente en IA",
    x = "Puntaje Global GIRAI (0 - 100)",
    y = NULL,
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 4. Imprimimos el gráfico en consola
print(grafico_boxplot_tipo)

# 5. Exportación parametrizada con ruta y nomenclatura estricta
ggsave(
  filename = "scripts_GIRAI/exports/bi_cuali_cuanti_tipo_privado_es_GIRAI.png",
  plot = grafico_boxplot_tipo,
  width = 6.5,
  height = 4.2,
  units = "in",
  dpi = 300
)