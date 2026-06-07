# Cargar librerías necesarias
library(dplyr)
library(tidyr)

# Generación de la Tabla de Contingencia
tabla_contingencia <- datos %>%
  filter(!is.na(privado), !is.na(academia)) %>%
  # Contamos las frecuencias
  count(privado, academia) %>%
  # Calculamos el porcentaje global
  mutate(porcentaje = round((n / sum(n)) * 100, 1),
         # Unimos el número y el porcentaje en un solo texto para la tabla
         celda = paste0(n, " (", porcentaje, "%)")) %>%
  select(-n, -porcentaje) %>%
  # Pivotamos la tabla para que "Academia" sean las columnas y "Privado" las filas
  pivot_wider(names_from = academia, values_from = celda, names_prefix = "Academia_") %>%
  rename(`Sector Privado` = privado)

# Imprimir la tabla en la consola
print(tabla_contingencia)
