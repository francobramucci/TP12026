library(tidyverse)

# 1. Transformamos los datos para poder graficar todas las áreas p70 juntas
datos_p70_largo <- datos %>%
  select(privado, starts_with("p70_")) %>%
  pivot_longer(
    cols = starts_with("p70_"), 
    names_to = "area_tematica", 
    values_to = "alcanzo_meta"
  ) %>%
  filter(!is.na(privado), !is.na(alcanzo_meta))

# 2. Calculamos el porcentaje de éxito por grupo y área
resumen_p70 <- datos_p70_largo %>%
  group_by(privado, area_tematica) %>%
  summarise(porcentaje_exito = mean(alcanzo_meta) * 100)

# 3. Gráfico de barras agrupadas (Respuesta Múltiple)
grafico_p70 <- ggplot(resumen_p70, aes(x = area_tematica, y = porcentaje_exito, fill = privado)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() + # Barras horizontales para leer mejor los nombres
  theme_minimal() +
  labs(
    title = "Áreas de Excelencia (>70 pts) según Participación Privada",
    x = "Área Temática",
    y = "% de países que superan los 70 puntos",
    fill = "Sector Privado"
  )
print(grafico_p70)
