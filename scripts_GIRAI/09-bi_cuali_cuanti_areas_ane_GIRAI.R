# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Limpieza preventiva
# Es buena práctica quitar los datos faltantes antes de hacer un gráfico de dispersión
datos_dispersion <- datos %>%
  filter(!is.na(areas_ane), !is.na(GIRAI))

# 2. Cálculo del promedio para países con al menos 15 áreas de acción no estatal
promedio_15_mas <- datos_dispersion %>%
  filter(areas_ane >= 15) %>%
  summarise(promedio = round(mean(GIRAI, na.rm = TRUE), 1)) %>%
  pull(promedio)

# Imprimimos el resultado en la consola
print(paste0("El promedio del puntaje GIRAI en países con al menos 15 áreas de acción no estatal es: ", promedio_15_mas))

# 3. Generación del Gráfico de Dispersión con Jitter
grafico_dispersion_ane <- ggplot(datos_dispersion) +
  
  # Capa de estéticas independiente
  aes(x = areas_ane, y = GIRAI) +
  
  # Dibujamos los puntos con Jitter. 
  # width = 0.2 separa horizontalmente, height = 0 respeta el puntaje exacto en Y.
  geom_jitter(color = "slateblue", alpha = 0.6, size = 3, width = 0.2, height = 0) +
  
  # Agregamos la línea de tendencia matemática (Linear Model = "lm")
  geom_smooth(method = "lm", color = "tomato", linetype = "dashed", se = FALSE, linewidth = 1) +
  
  # Temas base
  theme_minimal() +
  
  # Personalización del tema estandarizada
  theme(
    # Título centrado respecto a la imagen completa
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 20)),
    
    # Ejes
    axis.text = element_text(size = 11, color = "black"),
    
    # Limpiamos la cuadrícula menor de fondo
    panel.grid.minor = element_blank(), 
    
    # Forzamos la visibilidad de las líneas de los ejes
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Fuente anclada al lienzo completo, a la izquierda (hjust = 0) y con sangría (l = 15)
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15, l = 15))
  ) +
  
  # Etiquetas: Título único descriptivo y fuente
  labs(
    title = "Distribución de GIRAI según cantidad de áreas \n temáticas con intervención no estatal",
    x = "Cantidad de áreas temáticas con acción no estatal",
    y = "GIRAI (0 - 100)",
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 4. Imprimimos el gráfico en consola
print(grafico_dispersion_ane)

# 5. Exportación parametrizada con ruta y nomenclatura estricta
ggsave(
  filename = "scripts_GIRAI/exports/bi_cuanti_cuanti_areas_ane_GIRAI.png",
  plot = grafico_dispersion_ane,
  width = 6.5,
  height = 4.2,
  units = "in",
  dpi = 300
)