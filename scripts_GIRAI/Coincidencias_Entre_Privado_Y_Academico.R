# -------------- COINCIDENCIAS ENTRE PRIVADO Y ACADEMICO -------------------
library(dplyr)
library(ggplot2)

# 1. Función para agrupar iniciativas en categorías más generales
clasificar_tipo <- function(tipo) {
  case_when(
    tipo %in% c("Investigación y análisis") ~ "conocimiento",
    
    tipo %in% c("Formación y cursos",
                "Conferencias y talleres",
                "Campañas de sensibilización y promoción") ~ "difusion",
    
    tipo %in% c("Desarrollo de directrices éticas, normas y mejores prácticas",
                "Establecimiento de alianzas y colaboraciones") ~ "gobernanza",
    
    TRUE ~ NA_character_
  )
}

# 2. Filtrado y transformación
datos_cooperacion <- datos %>%
  filter(
    !is.na(GIRAI),
    academia == "Sí",
    privado == "Sí",
    !is.na(tipo_academia_es),
    !is.na(tipo_privado_es)
  ) %>%
  
  mutate(
    grupo_academia = clasificar_tipo(tipo_academia_es),
    grupo_privado = clasificar_tipo(tipo_privado_es)
  ) %>%
  
  filter(!is.na(grupo_academia), !is.na(grupo_privado)) %>%
  
  # 3. Índice de compatibilidad
  mutate(
    compatibilidad = case_when(
      grupo_academia == grupo_privado ~ 1,
      
      # relaciones "cercanas"
      (grupo_academia == "conocimiento" & grupo_privado == "difusion") |
        (grupo_academia == "difusion" & grupo_privado == "conocimiento") ~ 0.5,
      
      TRUE ~ 0
    ),
    
    compatibilidad_pct = compatibilidad * 100
  )

# 4. Gráfico
grafico_cooperacion <- ggplot(datos_cooperacion, aes(x = compatibilidad_pct, y = GIRAI)) +
  
  geom_jitter(width = 5, height = 0.3, size = 3, alpha = 0.7, color = "#33A02C") +
  
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  
  theme_minimal() +
  labs(
    title = "Relación entre Compatibilidad de Iniciativas y Puntaje GIRAI",
    subtitle = "Alineación funcional entre sector académico y privado",
    x = "Compatibilidad de iniciativas (%)",
    y = "Puntaje GIRAI"
  )

# 5. Mostrar
print(grafico_cooperacion)
