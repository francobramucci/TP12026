# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Filtramos los datos con tu doble condición
datos_capacidades <- datos %>%
  # Nos quedamos con países con sector privado Y alta participación temática
  filter(privado == "Sí", areas_ane > 10) %>%
  # Quitamos los NA de la variable 'cap' para que el gráfico no tire advertencias
  filter(!is.na(cap))

# 2. Generamos el Histograma y lo guardamos en una variable
grafico_histograma_cap <- ggplot(datos_capacidades, aes(x = cap)) +
  # geom_histogram es la función correcta para variables continuas
  # binwidth = 10 agrupa los puntajes de 10 en 10. Si ves que queda feo, puedes cambiarlo a 5 o 20.
  geom_histogram(fill = "steelblue", color = "white", binwidth = 10, alpha = 0.8) +
  
  theme_minimal() +
  
  labs(
    title = "Distribución de Capacidades en IA (Alta Intervención Privada)",
    subtitle = "Países con sector privado activo en más de 10 áreas temáticas (areas_ane > 10)",
    x = "Puntaje de Capacidades (cap)",
    y = "Frecuencia (Cantidad de Países)"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 11, color = "black")
  )

# 3. Imprimimos el gráfico para visualizarlo en RStudio
print(grafico_histograma_cap)
