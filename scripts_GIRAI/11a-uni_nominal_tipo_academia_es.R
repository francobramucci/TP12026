# Cargar librerías
library(dplyr)
library(ggplot2)
library(stringr)

# 1. Preparación de los datos
datos_tipo_academia <- datos %>%
  filter(!is.na(tipo_academia_es)) %>%
  count(tipo_academia_es) %>%
  mutate(porcentaje = round((n / sum(n)) * 100, 1))

# 2. Generación del gráfico de barras horizontales
distr_tipo_academia <- ggplot(datos_tipo_academia) +
  
  # Capa de estéticas independiente
  aes(x = n, y = reorder(str_wrap(tipo_academia_es, width = 28), n)) +
  
  # Construcción de la geometría (mantenemos el "seagreen" para la academia)
  geom_col(fill = "seagreen", alpha = 0.8) +
  
  # Etiquetas de datos
  geom_text(aes(label = paste0(porcentaje, "%")), hjust = -0.2, size = 3.5) +
  
  # Agregamos un 15% de margen extra a la derecha para que las etiquetas no se recorten
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  # Temas base
  theme_minimal() +
  
  # Personalización del tema estandarizada
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 20)),
    
    # Texto de categorías con comportamiento por defecto para envolver texto largo
    axis.text.y = element_text(size = 9, color = "black", lineheight = 0.8),
    axis.text.x = element_text(size = 11, color = "black"),
    
    panel.grid.major.y = element_blank(),
    
    # Forzamos la visibilidad de las líneas de los ejes
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Estilización simple de la fuente
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15))
  ) +
  
  # Etiquetas: Único título descriptivo y fuente agregada
  labs(
    title = "Tipos de iniciativas más frecuente de la academia en IA",
    x = "Cantidad de países",
    y = NULL,
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 3. Imprimir el gráfico en consola
print(distr_tipo_academia)

# 4. Exportación parametrizada con ruta y nomenclatura estricta
ggsave(
  filename = "scripts_GIRAI/exports/uni_nominal_tipo_academia_es.png",
  plot = distr_tipo_academia,
  width = 6.5,
  height = 4.2,
  units = "in",
  dpi = 300
)