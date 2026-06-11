# Cargar librerías
library(dplyr)
library(ggplot2)

# 1. Preparación de los datos y cálculo de porcentajes
datos_academia <- datos %>%
  filter(!is.na(academia)) %>%
  count(academia) %>%
  mutate(porcentaje = round((n / sum(n)) * 100, 1))

# 2. Generación del gráfico circular
grafico_torta_academia <- ggplot(datos_academia) +
  
  # Capa de estéticas independiente
  aes(x = "", y = n, fill = academia) +
  
  # Construcción de la geometría circular
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
  coord_polar("y", start = 0) +
  
  # Etiquetas de datos (Porcentaje y n)
  geom_text(aes(label = paste0(porcentaje, "%\n(n=", n, ")")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 5) +
  
  # Paleta de colores
  scale_fill_manual(values = c("Sí" = "seagreen", "No" = "tomato")) +
  
  # Temas base para gráficos circulares
  theme_void() + 
  
  # Personalización del tema estandarizada
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(t = 5, b = -10)),
    legend.position = "bottom",
    plot.caption = element_text(size = 8, color = "gray40", hjust = 1, margin = margin(t = 15))
  ) +
  
  # Etiquetas: Único título descriptivo y fuente
  labs(
    title = "Proporción de países según participación de la academia en IA",
    fill = "Participación Académica en IA:",
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 3. Imprimir el gráfico en consola
print(grafico_torta_academia)

# 4. Exportación parametrizada con ruta relativa estricta
ggsave(
  filename = "scripts_GIRAI/exports/uni_nominal_academia.png",
  plot = grafico_torta_academia,
  width = 6.5,
  height = 4.2,
  units = "in",
  dpi = 300
)