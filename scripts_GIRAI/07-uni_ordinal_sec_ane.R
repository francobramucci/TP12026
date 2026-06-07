# Cargar las librerías necesarias
library(dplyr)
library(ggplot2)

# 1. Preparación de los datos (El secreto está en el factor)
datos_sec_ane <- datos %>%
  # CAMBIO CLAVE: Transformamos el texto en un factor ordinal
  mutate(sec_ane = factor(sec_ane, levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))) %>%
  count(sec_ane) %>%
  mutate(porcentaje = round((n / sum(n)) * 100, 1))

# 2. Generación del gráfico vertical
grafico_sec_ane <-ggplot(datos_sec_ane, aes(x = sec_ane, y = n)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  
  # CAMBIO CLAVE: Usamos vjust (vertical justification) para poner el texto sobre la barra
  geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5, size = 3.5, fontface = "bold") +
  
  theme_minimal() +
  
  # Expandimos el eje Y un poco hacia arriba para que el porcentaje no choque con el borde
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  labs(
    title = "Nivel de Desarrollo de Actores No Estatales",
    subtitle = "Distribución según fuentes secundarias externas",
    x = "Nivel de Desarrollo (Progresión)",
    y = "Cantidad de Países"
  ) +
  theme(
    # Mejoramos la legibilidad del eje X
    axis.text.x = element_text(size = 11, color = "black"),
    panel.grid.major.x = element_blank() # Quitamos las líneas verticales de fondo
  )
print(grafico_sec_ane)
