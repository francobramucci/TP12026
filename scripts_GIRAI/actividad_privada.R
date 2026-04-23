library(dplyr)
library(ggplot2)

# 1. Filtramos los países que sí tienen un dato válido en la columna 'privado'
datos_analisis <- datos %>%
  filter(!is.na(privado))

# -------------------------------------------------------------------
# NUEVO BLOQUE: Cálculo de cantidades y medianas
# -------------------------------------------------------------------
resumen_estadistico <- datos_analisis %>%
  group_by(privado) %>%
  summarise(
    cantidad_paises = n(), # n() cuenta cuántas filas (países) hay en cada grupo
    mediana_GIRAI = round(median(GIRAI, na.rm = TRUE), 1) # Calcula la mediana y redondea a 1 decimal
  )

# Imprimimos esta pequeña tabla en la consola para que tengas los datos exactos
print("--- Resumen Estadístico ---")
print(resumen_estadistico)
print("---------------------------")
# -------------------------------------------------------------------


# 2. Generamos el gráfico
grafico_privado <- ggplot(datos_analisis, aes(x = privado, y = GIRAI, fill = privado)) +
  # Dibujamos el boxplot, un poco transparente
  geom_boxplot(alpha = 0.5, outlier.shape = NA) + 
  
  # Agregamos los puntos individuales
  geom_jitter(width = 0.2, alpha = 0.8, color = "darkgray") +
  
  theme_minimal() +
  labs(
    title = "Impacto del Sector Privado en el Índice de IA",
    subtitle = "Los puntos grises representan cada país individual",
    x = "¿Hay actores del sector privado trabajando en IA?",
    y = "Puntaje Global GIRAI"
  ) +
  theme(legend.position = "none") + 
  stat_summary(
    fun.data = function(x) {
      stats <- boxplot.stats(x)$stats
      data.frame(y = stats, label = round(stats, 1))
    },
    geom = "text",
    position = position_jitter(width = 0.1),
    size = 3
  )

# 3. Mostramos el gráfico
print(grafico_privado)
