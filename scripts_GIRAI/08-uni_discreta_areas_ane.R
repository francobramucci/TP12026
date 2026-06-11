# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Preparación de los datos para el gráfico
datos_bastones_ane <- datos %>%
  # Filtramos los valores faltantes para evitar distorsiones
  filter(!is.na(areas_ane)) %>%
  # Contamos cuántos países tienen 0 áreas, 1 área, 2 áreas, etc.
  count(areas_ane)

# 2. Cálculo e impresión de estadísticas descriptivas
estadisticas_areas_ane <- datos %>%
  # Filtramos NAs de la variable original
  filter(!is.na(areas_ane)) %>%
  summarise(
    Q1 = quantile(areas_ane, 0.25, na.rm = TRUE),
    Mediana = median(areas_ane, na.rm = TRUE),
    Q3 = quantile(areas_ane, 0.75, na.rm = TRUE)
  )

# Imprimimos los resultados en la consola
print("Cuartiles y mediana de la cantidad de áreas de intervención (areas_ane):")
print(estadisticas_areas_ane)

# 3. Generación del gráfico de bastones
grafico_bastones_ane <- ggplot(datos_bastones_ane) +
  
  # Capa de estéticas independiente
  aes(x = factor(areas_ane), y = n) +
  
  # width = 0.3 afina la geometría para que visualmente sea un "bastón"
  geom_col(fill = "slateblue", width = 0.3, alpha = 0.9) +
  
  # Agregamos la frecuencia absoluta (n) en la punta de cada bastón (sin negrita)
  geom_text(aes(label = n), vjust = -0.8, size = 3.5) +
  
  # Expandimos el techo del gráfico un 15% para que los números no choquen con el borde
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  # Temas base
  theme_minimal() +
  
  # Personalización del tema estandarizada
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 20)),
    
    # Ejes
    axis.text = element_text(size = 11, color = "black"),
    
    # Limpiamos las líneas para mayor nitidez
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    
    # Forzamos la visibilidad de las líneas de los ejes
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Estilización simple de la fuente
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15))
  ) +
  
  # Etiquetas: Título único descriptivo y fuente
  labs(
    title = "Frecuencia de países según cantidad de áreas temáticas \n con intervención de actores no estatales",
    x = "Cantidad de áreas de intervención no estatal",
    y = "Cantidad de países",
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 4. Imprimimos el gráfico en consola
print(grafico_bastones_ane)

# 5. Exportación parametrizada con ruta y nomenclatura estricta
ggsave(
  filename = "scripts_GIRAI/exports/uni_cuanti_areas_ane.png",
  plot = grafico_bastones_ane,
  width = 6.5,
  height = 4.2,
  units = "in",
  dpi = 300
)