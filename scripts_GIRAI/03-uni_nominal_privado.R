# Cargar librerías
library(dplyr)
library(ggplot2)

# 1. Preparación de los datos y cálculo de porcentajes
datos_privado <- datos %>%
  filter(!is.na(privado)) %>%
  count(privado) %>%
  mutate(porcentaje = round((n / sum(n)) * 100, 1))

# 2. Generación del gráfico circular
grafico_torta_privado <- ggplot(datos_privado) +
  
  # Capa de estéticas independiente
  aes(x = "", y = n, fill = privado) +
  
  # Construcción de la geometría circular
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
  coord_polar("y", start = 0) +
  
  # Etiquetas de datos (Porcentaje y n)
  geom_text(aes(label = paste0(porcentaje, "%\n(n=", n, ")")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 5) +
  
  # Paleta de colores
  scale_fill_manual(values = c("Sí" = "springgreen4", "No" = "tomato")) +
  
  # Temas base para gráficos circulares
  theme_void() + 
  
  # Personalización del tema 
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(t= 5, b = -10)),
    legend.position = "bottom"
  ) +
  
  # Etiquetas: Único título descriptivo
  labs(
    title = "Proporción de países según participación del sector privado en IA",
    fill = "Participación Privada:"
  )

# 3. Imprimir el gráfico en consola
print(grafico_torta_privado)

# 4. Exportación parametrizada con ruta relativa estricta
ggsave(
  filename = "scripts_GIRAI/exports/uni_nominal_privado.png",
  plot = grafico_torta_privado,
  width = 6.5,
  height = 4.2,
  units = "in",
  dpi = 300
)