# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Preparación de los datos para el gráfico
datos_bastones_ag <- datos %>%
  # Filtramos los valores faltantes para evitar distorsiones
  filter(!is.na(areas_ag)) %>%
  # Contamos cuántos países tienen 0 áreas, 1 área, 2 áreas, etc.
  count(areas_ag) %>%
  # CAMBIO AQUÍ: Calculamos el porcentaje relativo al total de países válidos
  mutate(porcentaje = round((n / sum(n)) * 100, 1))

# 2. Cálculo e impresión de estadísticas descriptivas
estadisticas_areas_ag <- datos %>%
  # Filtramos NAs de la variable original
  filter(!is.na(areas_ag)) %>%
  summarise(
    Promedio = round(mean(areas_ag, na.rm = TRUE), 1),
    Q1 = quantile(areas_ag, 0.25, na.rm = TRUE),
    Mediana = median(areas_ag, na.rm = TRUE),
    Q3 = quantile(areas_ag, 0.75, na.rm = TRUE)
  )

# Imprimimos los resultados en la consola
print("Estadísticas de la cantidad de áreas de intervención gubernamental (areas_ag):")
print(estadisticas_areas_ag)

# 3. Generación del gráfico de bastones
grafico_bastones_ag <- ggplot(datos_bastones_ag) +
  
  # CAMBIO AQUÍ: Usamos 'porcentaje' en el eje Y
  aes(x = factor(areas_ag), y = porcentaje) +
  
  # width = 0.3 afina la geometría para que visualmente sea un "bastón"
  geom_col(fill = "indianred1", width = 0.3, alpha = 0.9) +
  
  # Expandimos el techo del gráfico un 15% para que los números no choquen con el borde
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  # Temas base
  theme_minimal() +
  
  # Personalización del tema estandarizada
  theme(
    # Título centrado respecto a la imagen completa
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 20)),
    
    # Ejes
    axis.text = element_text(size = 11, color = "black"),
    
    # Limpiamos las líneas para mayor nitidez
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    
    # Forzamos la visibilidad de las líneas de los ejes
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Fuente anclada al lienzo completo, a la izquierda (hjust = 0) y con sangría (l = 15)
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15, l = 15))
  ) +
  
  # Etiquetas: Título actualizado (Distribución) y eje Y corregido
  labs(
    title = "Distribución de países según cantidad de áreas temáticas \n con intervención gubernamental",
    x = "Cantidad de áreas con intervención gubernamental",
    y = "Porcentaje de países (%)", # Etiqueta actualizada
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 4. Imprimimos el gráfico en consola
print(grafico_bastones_ag)

# 5. Exportación parametrizada con ruta y nomenclatura estricta
ggsave(
  filename = "scripts_GIRAI/exports/uni_discreta_areas_ag.png",
  plot = grafico_bastones_ag,
  width = 6.5,
  height = 4.2,
  units = "in",
  dpi = 300
)