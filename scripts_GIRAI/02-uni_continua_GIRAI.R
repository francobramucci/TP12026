# Cargar librerías
library(dplyr)
library(ggplot2)

# 1. Calculo la mediana para agregarla al gráfico
estadisticas_girai <- datos %>%
  # Limpiamos NAs por seguridad analítica
  filter(!is.na(GIRAI)) %>% 
  summarise(
    mediana = median(GIRAI)
  )

mediana_global <- estadisticas_girai$mediana

# 2. Genero el Histograma
grafico_girai_general <- ggplot(datos %>% filter(!is.na(GIRAI))) +
  aes(x = GIRAI) +
  
  # MAGIA AQUÍ: Forzamos a que el eje Y calcule el porcentaje en lugar de la cantidad
  geom_histogram(
    aes(y = after_stat(count / sum(count) * 100)), 
    fill = "steelblue", color = "white", bins = 12, alpha = 0.8
  ) +
  
  # Marco la mediana mundial
  geom_vline(xintercept = mediana_global, linetype = "dashed", color = "tomato", linewidth = 1) +
  
  # Indico valor de la mediana global (Ajustamos el Y a un valor porcentual prudente, ej: 20%)
  annotate("text", x = mediana_global + 3, y = 20, 
           label = paste("Mediana Mundial:", round(mediana_global, 1)), 
           color = "tomato", fontface = "plain", hjust = 0) +
  
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  coord_cartesian(xlim = c(0, 100)) +
  
  # Temas base
  theme_minimal() +
  
  # Personalización del tema estandarizada
  theme(
    # Título centrado respecto a la imagen completa
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 20)),
    
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank(),
    
    # Líneas de eje
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Fuente anclada al lienzo completo, a la izquierda (hjust = 0) y con sangría (l = 15)
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15, l = 15))
  ) +
  
  # Etiquetas: Título único descriptivo y fuente
  labs(
    title = "Distribución de países según el Índice GIRAI",
    x = "GIRAI (0-100)",
    y = "Porcentaje de países (%)", # Etiqueta del eje Y actualizada
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 3. Calculo percentil para el valor 1
percentil_menor_1 <- datos %>%
  filter(!is.na(GIRAI)) %>% 
  summarise(
    porcentaje = mean(GIRAI < 1) * 100
  ) %>%
  # Extraigo el número exacto
  pull(porcentaje)

# Imprimo el resultado con un texto descriptivo en la consola
print(paste0("El ", round(percentil_menor_1, 1), "% de los países tiene un puntaje GIRAI menor a 1."))

# 4. Imprimo el gráfico en consola
print(grafico_girai_general)

# 5. Exportación parametrizada con ruta y nomenclatura estricta
ggsave(
  filename = "scripts_GIRAI/exports/uni_continua_GIRAI.png", 
  plot = grafico_girai_general,         
  width = 6.5,                              
  height = 4.2,                             
  units = "in",                             
  dpi = 300                                 
)