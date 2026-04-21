# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Filtramos y preparamos los datos
datos_concientizacion <- datos %>%
  # Nos quedamos SOLO con los países sin sector privado
  filter(privado == "No", !is.na(areas_ag)) %>%
  # Contamos cuántos países hicieron 0 áreas, 1 área, 2 áreas, etc.
  count(areas_ag)

# 2. Generamos el gráfico de bastones y lo guardamos en una variable
grafico_bastones_estado <- ggplot(datos_concientizacion, aes(x = factor(areas_ag), y = n)) +
  # width = 0.4 hace que las barras sean finitas, simulando "bastones"
  geom_col(fill = "steelblue", width = 0.4, alpha = 0.9) + 
  
  # Agregamos la cantidad exacta arriba de cada bastón
  geom_text(aes(label = n), vjust = -0.8, size = 4, fontface = "bold") +
  
  theme_minimal() +
  
  # Damos aire arriba para que los números no se corten
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  
  labs(
    title = "Accionar gubernamentales en ausencia del Sector Privado",
    subtitle = "Distribución de áreas accionar gubernamental en países donde 'privado = No'",
    x = "Cantidad de Áreas Temáticas (areas_ag)",
    y = "Frecuencia (Cantidad de Países)"
  ) +
  theme(
    panel.grid.minor = element_blank(), # Limpiamos líneas de fondo innecesarias
    axis.text = element_text(size = 11, color = "black")
  )

# 3. Imprimimos el gráfico para que aparezca en RStudio
print(grafico_bastones_estado)
