# Cargar librerías necesarias
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra) # Librería nativa para dibujar tablas
library(grid)      # Motor gráfico base de R para los textos

# 1. Preparación de los datos y conteo base
conteo_base <- datos %>%
  filter(!is.na(privado), !is.na(academia)) %>%
  count(privado, academia)

# 2. Construcción de la matriz numérica con totales
tabla_num <- conteo_base %>%
  pivot_wider(names_from = academia, values_from = n, values_fill = list(n = 0)) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  bind_rows(summarise(., across(where(is.numeric), sum), privado = "Total Global"))

total_global <- tabla_num %>% 
  filter(privado == "Total Global") %>% 
  pull(Total)

# 3. Fusión de números y porcentajes en formato texto
tabla_contingencia <- tabla_num %>%
  mutate(
    across(where(is.numeric), 
           ~ paste0(.x, " (", round((.x / total_global) * 100, 1), "%)"))
  ) %>%
  rename(
    `Privado en IA` = privado,
    `Academia en IA: No` = `No`,
    `Academia en IA: Sí` = `Sí`,
    `Total Global` = Total
  )

# Imprimir la tabla en consola para tener la referencia
print(tabla_contingencia)

# 4. Diseño estético avanzado nativo con gridExtra
n_filas <- nrow(tabla_contingencia)
n_cols <- ncol(tabla_contingencia)

# 4.A Construimos la matriz de fuentes (Negritas)
matriz_fuentes <- matrix("plain", nrow = n_filas, ncol = n_cols)
matriz_fuentes[, 1] <- "bold"          # Primera columna
matriz_fuentes[n_filas, ] <- "bold"    # Fila de Total Global
matriz_fuentes[, n_cols] <- "bold"     # Columna de Total Global

# 4.B Construimos la matriz de colores de fondo
matriz_fondos <- matrix("white", nrow = n_filas, ncol = n_cols)
matriz_fondos[n_filas, ] <- "#EBF5FB"  # Fila Total Global (Celeste pastel)
matriz_fondos[, n_cols] <- "#EBF5FB"   # Columna Total Global (Celeste pastel)
matriz_fondos[n_filas, n_cols] <- "#D6EAF8" # Intersección del Total

# Resaltado en verde de la intersección de Sí y Sí
fila_si <- which(tabla_contingencia$`Privado en IA` == "Sí")
col_si <- which(colnames(tabla_contingencia) == "Academia en IA: Sí")
matriz_fondos[fila_si, col_si] <- "palegreen1" 

# NUEVO: Pisamos el fondo de la primera columna para que sea idéntica a la cabecera superior
matriz_fondos[, 1] <- "steelblue"

# 4.C NUEVO: Construimos la matriz de colores de texto
matriz_textos <- matrix("black", nrow = n_filas, ncol = n_cols)
matriz_textos[, 1] <- "white" # Letras blancas solo para la primera columna (cabecera de filas)

# 4.D Ensamblamos el tema inyectando las tres matrices
tema_tabla_color <- ttheme_minimal(
  core = list(
    bg_params = list(fill = matriz_fondos, col = "white", lwd = 2), 
    # Añadimos col = matriz_textos para que R sepa qué color de letra usar en cada celda
    fg_params = list(fontsize = 10, fontface = matriz_fuentes, col = matriz_textos)
  ),
  colhead = list(
    bg_params = list(fill = "steelblue", col = "white", lwd = 2),    
    fg_params = list(fontsize = 11, fontface = "bold", col = "white")
  )
)

# Renderizamos la tabla como un objeto gráfico aplicando el nuevo tema
grob_tabla <- tableGrob(tabla_contingencia, rows = NULL, theme = tema_tabla_color)

# Construimos los bloques de texto para el título y la fuente
titulo <- textGrob(
  "Contingencia entre el Sector Privado y Académico en IA\n(Frecuencia absoluta y porcentaje global)",
  gp = gpar(fontsize = 12, fontface = "plain"), 
  just = "center"
)

fuente <- textGrob(
  "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA",
  gp = gpar(fontsize = 8, col = "gray40"), 
  just = "left", 
  x = 0.05 
)

# Ensamblamos todo 
grob_final <- arrangeGrob(
  titulo,
  grob_tabla,
  fuente,
  nrow = 3,
  heights = unit(c(1, 4, 0.5), "null")
)

# 5. Exportación usando ggsave
ggsave(
  filename = "scripts_GIRAI/exports/bi_cuali_cuali_privado_academia.png",
  plot = grob_final,
  width = 6.5,
  height = 3.5,
  units = "in",
  dpi = 300,
  bg = "white" 
)