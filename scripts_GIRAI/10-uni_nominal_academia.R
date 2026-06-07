# Cargar librerías
library(dplyr)
library(ggplot2)

# 1. Preparación de los datos y cálculo de porcentajes
datos_academia <- datos %>%
  # Limpiamos los valores nulos
  filter(!is.na(academia)) %>%
  count(academia) %>%
  # Calculamos el peso relativo para las etiquetas
  mutate(porcentaje = round((n / sum(n)) * 100, 1))

# 2. Generación del gráfico circular
grafico_torta_academia <- ggplot(datos_academia, aes(x = "", y = n, fill = academia)) +
  
  # Construimos la geometría base y la transformamos a coordenadas polares
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
  coord_polar("y", start = 0) +
  
  # Insertamos las etiquetas con el Porcentaje y el (n) absoluto
  geom_text(aes(label = paste0(porcentaje, "%\n(n=", n, ")")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 5, fontface = "bold") +
  
  # Usamos verde (seagreen) para diferenciar al sector académico del privado
  scale_fill_manual(values = c("No" = "tomato", "Sí" = "seagreen")) +
  
  theme_void() + 
  
  labs(
    title = "Estructura Global del Ecosistema de IA",
    subtitle = "Proporción de países según la participación del sector académico",
    fill = "Participación Académica:"
  ) +
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom"
  )

# 3. Imprimir el gráfico en la consola
print(grafico_torta_academia)
