# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyr)  
library(stringr)

# 1. Definir el total de países (para el cálculo de porcentajes)
# Asumimos que cada fila en 'datos' es un país
total_paises <- nrow(datos)

# 2. Preparación de los datos y recodificación
datos_p70_dummies <- datos %>%
  # Nos quedamos SOLO con las columnas que empiezan con "p70"
  select(starts_with("p70")) %>%
  
  # Pivotamos de ancho a largo
  pivot_longer(cols = everything(), names_to = "opcion", values_to = "marcado") %>%
  
  # Filtramos solo aquellas donde el país marcó la opción (1 = Sí)
  filter(marcado == 1) %>%
  
  # Contamos cuántos '1' obtuvo cada opción
  count(opcion) %>%
  
  # MAGIA AQUÍ: Traducimos el nombre de la variable a su etiqueta descriptiva
  mutate(
    opcion_limpia = case_match(
      opcion,
      "p70_sesgo"   ~ "Sesgo y Discriminación Injusta",
      "p70_infancia"~ "Derechos de la Infancia",
      "p70_divers"  ~ "Diversidad Cultural y Lingüística",
      "p70_datpers" ~ "Protección de Datos y Privacidad",
      "p70_genero"  ~ "Igualdad de Género",
      "p70_suphum"  ~ "Supervisión Humana",
      "p70_laboral" ~ "Protección Laboral y Derecho al Trabajo",
      "p70_segu"    ~ "Seguridad, Precisión y Fiabilidad",
      "p70_transp"  ~ "Transparencia y Explicabilidad",
      .default      = opcion # Por si aparece una categoría no contemplada
    ),
    # Calculamos el porcentaje sobre el total de países
    porcentaje = round((n / total_paises) * 100, 1)
  )

# 3. Generación del gráfico de barras horizontales
grafico_p70 <- ggplot(datos_p70_dummies) +
  
  # CAMBIO AQUÍ: Usamos 'porcentaje' para el eje X y para ordenar el eje Y
  aes(x = porcentaje, y = reorder(str_wrap(opcion_limpia, width = 35), porcentaje)) +
  
  # Construcción de la geometría
  geom_col(fill = "steelblue", alpha = 0.8) +
  
  # Etiquetas de datos por fuera de la barra
  geom_text(aes(label = paste0(porcentaje, "%")), hjust = -0.1, size = 3.5) +
  
  # Agregamos un 20% de margen extra a la derecha para no recortar el texto
  scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
  
  # Temas base
  theme_minimal() +
  
  # Personalización del tema estandarizada
  theme(
    # Título centrado respecto a la imagen completa
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 20)),
    
    # Texto de categorías y comportamiento por defecto en Y
    axis.text.y = element_text(size = 9, color = "black", lineheight = 0.8),
    axis.text.x = element_text(size = 11, color = "black"),
    
    # Estilización del título del eje Y con margen derecho para que respire
    axis.title.y = element_text(size = 11, color = "black", margin = margin(r = 15)),
    
    panel.grid.major.y = element_blank(),
    
    # Forzamos la visibilidad de las líneas de los ejes
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Fuente anclada al lienzo completo, a la izquierda (hjust = 0) y con sangría (l = 15)
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15, l = 15))
  ) +
  
  # Etiquetas: Título descriptivo, nuevo eje Y y fuente
  labs(
    title = "Proporción de países con puntaje superior a 70 por área temática",
    x = "Porcentaje de países (%)", # CAMBIO AQUÍ: Etiqueta actualizada
    y = "Áreas temáticas", 
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 4. Imprimir el gráfico en consola
print(grafico_p70)

# 5. Exportación parametrizada con ruta y nomenclatura estricta
ggsave(
  filename = "scripts_GIRAI/exports/uni_rta_multiple_p70.png",
  plot = grafico_p70,
  width = 6.5,
  height = 4.2,
  units = "in",
  dpi = 300
)