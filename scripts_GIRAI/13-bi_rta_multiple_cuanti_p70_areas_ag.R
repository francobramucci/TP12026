# Cargar librerías necesarias
library(dplyr)
library(ggplot2)
library(tidyr)  
library(stringr)

# 1. Preparación de la variable agrupada y cálculo de totales por grupo
datos_agrupados <- datos %>%
  # Limpiamos países sin datos en la variable gubernamental
  filter(!is.na(areas_ag)) %>%
  mutate(
    # Creamos la variable categórica
    grupo_ag = if_else(areas_ag >= 10, "Fuerte (≥ 10 áreas)", "Débil (< 10 áreas)"),
    # La convertimos en factor para fijar su orden lógico en la leyenda
    grupo_ag = factor(grupo_ag, levels = c("Fuerte (≥ 10 áreas)", "Débil (< 10 áreas)"))
  )

# Guardamos cuántos países conforman cada grupo para calcular el porcentaje real
totales_grupo <- datos_agrupados %>%
  count(grupo_ag, name = "total_grupo")

# 2. Pivotar y preparar los datos de las dimensiones (p70)
datos_p70_comparativo <- datos_agrupados %>%
  select(grupo_ag, starts_with("p70")) %>%
  pivot_longer(cols = starts_with("p70"), names_to = "opcion", values_to = "marcado") %>%
  
  # Nos quedamos con las opciones marcadas (1)
  filter(marcado == 1) %>%
  count(grupo_ag, opcion) %>%
  
  # --- SOLUCIÓN AL 0% ---
  # Forzamos a que existan todas las combinaciones cruzadas de grupos y opciones.
  # Si un grupo no marcó nada en una opción, rellena el valor 'n' con 0.
  complete(grupo_ag, opcion, fill = list(n = 0)) %>%
  
  # Cruzamos con los totales de cada grupo para calcular el peso relativo
  left_join(totales_grupo, by = "grupo_ag") %>%
  
  mutate(
    porcentaje = round((n / total_grupo) * 100, 1),
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
      .default      = opcion
    ),
    # Envolvemos el texto en este paso para usarlo directo en el gráfico
    opcion_wrap = str_wrap(opcion_limpia, width = 32)
  )

# Para ordenar el eje Y, calculamos la frecuencia total global de cada opción
orden_opciones <- datos_p70_comparativo %>%
  group_by(opcion_wrap) %>%
  summarise(total = sum(n)) %>%
  arrange(total) %>%
  pull(opcion_wrap)

# Aplicamos el orden bloqueándolo como factor
datos_p70_comparativo <- datos_p70_comparativo %>%
  mutate(opcion_wrap = factor(opcion_wrap, levels = orden_opciones))

# 3. Generación del Gráfico de Barras Agrupadas
grafico_comparativo_p70 <- ggplot(datos_p70_comparativo) +
  
  # Capa de estéticas (usamos el porcentaje en X)
  aes(x = porcentaje, y = opcion_wrap, fill = grupo_ag) +
  
  # --- SOLUCIÓN AL ESPACIADO ---
  geom_col(position = position_dodge(width = 0.7), width = 0.55, alpha = 0.85) +
  
  # Etiquetas numéricas relativas a cada barra (ajustamos el dodge para que coincida)
  geom_text(aes(label = paste0(porcentaje, "%")), 
            position = position_dodge(width = 0.7), 
            hjust = -0.15, size = 3) +
  
  # Paleta de colores analítica
  scale_fill_manual(values = c("Fuerte (≥ 10 áreas)" = "indianred1", "Débil (< 10 áreas)" = "darkorange")) +
  
  # Margen extra para que el porcentaje no choque con el margen derecho
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  # Temas base
  theme_minimal() +
  
  # Personalización del tema estandarizada
  theme(
    # Título centrado respecto a la imagen completa
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 20)),
    
    legend.position = "bottom",
    
    # Texto y ejes
    axis.text.y = element_text(size = 9, color = "black", lineheight = 0.8),
    axis.text.x = element_text(size = 11, color = "black"),
    
    # Estilización del título del eje Y
    axis.title.y = element_text(size = 11, color = "black", margin = margin(r = 15)),
    
    panel.grid.major.y = element_blank(),
    
    # Forzamos la visibilidad de las líneas de los ejes
    axis.line = element_line(color = "black", linewidth = 0.5),
    
    # Fuente anclada al lienzo completo, a la izquierda (hjust = 0) y con sangría (l = 15)
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0, margin = margin(t = 15, l = 15))
  ) +
  
  # SOLUCIÓN DE ESPACIO DE LA LEYENDA: Título arriba y categorías centradas abajo
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  
  # Etiquetas descriptivas mejoradas
  labs(
    title = "Proporción de países con puntaje superior a 70 por área temática, \nsegún nivel de participación gubernamental",
    x = "Porcentaje de países (%)", 
    y = "Áreas temáticas", 
    fill = "Participación del gobierno en áreas temáticas:",
    caption = "Fuente: Índice Global de IA Responsable 2023-2024, Centro Global para la Gobernanza de la IA"
  )

# 4. Imprimimos el gráfico en consola
print(grafico_comparativo_p70)

# 5. Exportación parametrizada
ggsave(
  filename = "scripts_GIRAI/exports/bi_rta_multiple_cuanti_p70_areas_ag.png",
  plot = grafico_comparativo_p70,
  width = 7.5,  
  height = 5.0, 
  units = "in",
  dpi = 300
)