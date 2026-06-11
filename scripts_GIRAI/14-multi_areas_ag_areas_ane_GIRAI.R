# Cargar librerías
library(dplyr)
library(ggplot2)

# 1. Filtramos datos válidos
datos_ecosistema <- datos %>%
  filter(!is.na(areas_ane), !is.na(areas_ag), !is.na(GIRAI))

# --- NUEVO: Cálculos de los cuadrantes extremos ---

# Cuadrante Alto
analisis_cuadrante_alto <- datos_ecosistema %>%
  filter(areas_ane >= 10, areas_ag > 10) %>%
  summarise(
    cantidad_paises = n(),
    promedio_GIRAI = round(mean(GIRAI), 1)
  )

# Cuadrante Bajo
analisis_cuadrante_bajo <- datos_ecosistema %>%
  filter(areas_ane < 10, areas_ag < 10) %>%
  summarise(
    cantidad_paises = n(),
    promedio_GIRAI = round(mean(GIRAI), 1)
  )

# Cuadrante Bajo Derecho
analisis_cuadrante_bajo_der <- datos_ecosistema %>%
  filter(areas_ane > 10, areas_ag < 10) %>%
  summarise(
    cantidad_paises = n(),
    promedio_GIRAI = round(mean(GIRAI), 1)
  )


# Imprimimos por consola la comparación
print("--- Análisis de Alta Intervención Conjunta (ambas >= 10) ---")
print(analisis_cuadrante_alto)
print("--- Análisis de Baja Intervención Conjunta (ambas < 10) ---")
print(analisis_cuadrante_bajo)
print("--- Análisis de Intervención Dispareja (areas_ane > 10, areas_ag < 10) ---")
print(analisis_cuadrante_bajo_der)
print("-----------------------------------------------------------")


# ------------------------------------------------------------------

# 2. Generamos el gráfico de dispersión con Jitter y línea de paridad
grafico_ecosistema <- ggplot(datos_ecosistema) +
  
  # Capa de estéticas independiente
  aes(x = areas_ane, y = areas_ag, color = GIRAI) +
  
  # Usamos jitter para evitar superposición de números enteros
  geom_jitter(width = 0.3, height = 0.3, size = 3, alpha = 0.8) +
  
  # LA CLAVE DEL ANÁLISIS: Una línea diagonal exacta de paridad (y = x)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  
  # Escala de colores continua (viridis)
  scale_color_viridis_c(option = "plasma", name = "GIRAI\n(0-100)") +
  
  # Temas base
  theme_minimal() +
  
  # Agregamos anotaciones de texto limpias (sin cursivas ni negritas)
  annotate("text", x = 1, y = max(datos_ecosistema$areas_ag) - 1, 
           label = "Dominio Estatal", color = "gray40", size = 4, hjust = 0) +
  annotate("text", x = max(datos_ecosistema$areas_ane) - 1, y = 1, 
           label = "Dominio No Estatal", color = "gray40", size = 4, hjust = 1) +
  
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
    
    # Estilización de la leyenda
    legend.position = "right",
    
    # Fuente anclada al lienzo completo, a la izquierda (hjust = 0) y con sangría (l = 15)
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15, l = 15))
  ) +
  
  # Etiquetas: Título único descriptivo y fuente
  labs(
    title = "Distribución de países y GIRAI según la intervención estatal y no estatal \n en áreas temáticas sobre IA",
    x = "Cantidad de áreas temáticas con accionar no estatal",
    y = "Cantidad de áreas temáticas con accionar gubernamental",
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 3. Imprimir el gráfico en consola
print(grafico_ecosistema)

# 4. Exportación parametrizada con ruta y nomenclatura estricta
ggsave(
  filename = "scripts_GIRAI/exports/multi_areas_ag_areas_ane_GIRAI.png",
  plot = grafico_ecosistema,
  width = 7.5,
  height = 5.0,
  units = "in",
  dpi = 300
)