# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyr)  # Fundamental para pivotar los datos
library(stringr)

# 1. Definir el total de países (para el cálculo de porcentajes)
# Asumimos que cada fila en 'datos' es un país
total_paises <- nrow(datos)

# 2. Preparación de los datos (Pivotar de ancho a largo)
datos_p70_dummies <- datos %>%
  # Nos quedamos SOLO con las columnas que empiezan con "p70"
  select(starts_with("p70")) %>%
  
  # MAGIA AQUÍ: Convertimos las múltiples columnas en dos (la opción y si se marcó o no)
  pivot_longer(cols = everything(), names_to = "opcion", values_to = "marcado") %>%
  
  # Filtramos solo aquellas donde el país marcó la opción (asumiendo que 1 = Sí)
  filter(marcado == 1) %>%
  
  # Contamos cuántos '1' obtuvo cada opción
  count(opcion) %>%
  
  # Limpiamos el texto: quitamos el "p70_" para que el gráfico quede profesional
  mutate(opcion_limpia = str_remove(opcion, "p70_")) %>%
  
  # Calculamos el porcentaje sobre el total de países
  mutate(porcentaje = round((n / total_paises) * 100, 1))

# 3. Generación del gráfico de barras horizontales
grafico_p70 <- ggplot(datos_p70_dummies, aes(x = n, y = reorder(opcion_limpia, n))) +
  
  geom_col(fill = "steelblue", alpha = 0.8) +
  
  # Etiquetas de porcentaje alineadas a la izquierda (base de la barra)
  geom_text(aes(x = 0, label = paste0(porcentaje, "% (n=", n, ")")), 
            hjust = 0, nudge_x = 0.5, size = 3.5, color = "white", fontface = "bold") +
  
  theme_minimal() +
  
  labs(
    title = "Prevalencia de las Dimensiones Evaluadas (Variable p70)",
    subtitle = paste0("Porcentaje de países que abordan cada dimensión (N = ", total_paises, ")"),
    x = "Cantidad de Países",
    y = "Dimensión (Categoría)"
  ) +
  
  theme(
    axis.text.y = element_text(size = 9, color = "black", hjust = 0),
    panel.grid.major.y = element_blank()
  )

# 4. Imprimir el gráfico
print(grafico_p70)
