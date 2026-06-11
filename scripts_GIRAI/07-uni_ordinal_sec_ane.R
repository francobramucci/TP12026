# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Preparación de los datos (El secreto está en el factor)
datos_sec_ane <- datos %>%
  # Limpiamos NAs antes de generar las categorías
  filter(!is.na(sec_ane)) %>%
  
  # Transformamos el texto en un factor ordinal
  mutate(sec_ane = factor(sec_ane, levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))) %>%
  count(sec_ane) %>%
  mutate(porcentaje = round((n / sum(n)) * 100, 1))

# 2. Generación del gráfico vertical
grafico_sec_ane <- ggplot(datos_sec_ane) +
  
  # CAMBIO AQUÍ: Usamos 'porcentaje' en el eje Y
  aes(x = sec_ane, y = porcentaje) +
  
  # Construcción de la geometría
  geom_col(fill = "slateblue", alpha = 0.8) +
  
  # Usamos vjust para poner el texto sobre la barra (sin negrita, según estándar)
  geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5, size = 3.5) +
  
  # Expandimos el eje Y un poco hacia arriba para que el porcentaje no choque con el borde
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  # Temas base
  theme_minimal() +
  
  # Personalización del tema estandarizada
  theme(
    # Título centrado respecto a la imagen completa
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 20)),
    
    # Ejes
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    panel.grid.major.x = element_blank(), # Quitamos las líneas verticales de fondo
    panel.grid.minor = element_blank(),
    
    # Forzamos la visibilidad de las líneas de los ejes
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Fuente anclada al lienzo completo, a la izquierda (hjust = 0) y con sangría (l = 15)
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15, l = 15))
  ) +
  
  # Etiquetas: Título único descriptivo y fuente
  labs(
    title = "Distribución de países según nivel de desarrollo de actores \n no estatales en el uso responsable de la IA según fuentes secundarias",
    x = "Nivel de desarrollo",
    y = "Porcentaje de países (%)", # CAMBIO AQUÍ: Etiqueta actualizada
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 3. Imprimir el gráfico en consola
print(grafico_sec_ane)

# 4. Exportación parametrizada con ruta y nomenclatura estricta
ggsave(
  filename = "scripts_GIRAI/exports/uni_cuali_sec_ane.png",
  plot = grafico_sec_ane,
  width = 6.5,
  height = 4.2,
  units = "in",
  dpi = 300
)