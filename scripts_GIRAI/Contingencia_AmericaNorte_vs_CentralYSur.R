install.packages("tidyr")
library(dplyr)
library(ggplot2)
library(tidyr)

# ----------------- DISTRIBUCION GIRAI GENERAL -------------------------
# 1. Preparación de datos
datos_boxplot <- datos %>%
  filter(!is.na(GIRAI), !is.na(GIRAI_region)) %>%
  mutate(region_grupo = case_when(
    GIRAI_region == "América del Norte" ~ "América del Norte",
    GIRAI_region == "América del Sur y Central" ~ "América Central y Sur",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(region_grupo))

# 2. Resumen estadístico para etiquetas
resumen <- datos_boxplot %>%
  group_by(region_grupo) %>%
  summarise(
    min = min(GIRAI),
    q1 = quantile(GIRAI, 0.25),
    mediana = median(GIRAI),
    q3 = quantile(GIRAI, 0.75),
    max = max(GIRAI)
  )

# 3. Gráfico
grafico_boxplot <- ggplot(datos_boxplot, aes(x = region_grupo, y = GIRAI, fill = region_grupo)) +
  
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.color = "red", outlier.size = 2) +
  
  # Línea de promedio global
  geom_hline(yintercept = mean(datos_boxplot$GIRAI, na.rm = TRUE),
             linetype = "dashed", color = "brown", linewidth = 1) +
  
  # Etiquetas de mediana
  geom_text(data = resumen, aes(x = region_grupo, y = mediana, label = round(mediana,2)),
            vjust = -1, color = "black", size = 4) +
  
  # Etiquetas de Q1 y Q3
  geom_text(data = resumen, aes(x = region_grupo, y = q1, label = round(q1,2)),
            vjust = 1.5, color = "blue", size = 3) +
  
  geom_text(data = resumen, aes(x = region_grupo, y = q3, label = round(q3,2)),
            vjust = -0.5, color = "blue", size = 3) +
  
  # Etiquetas de min y max
  geom_text(data = resumen, aes(x = region_grupo, y = min, label = round(min,2)),
            vjust = 1.5, color = "darkgreen", size = 3) +
  
  geom_text(data = resumen, aes(x = region_grupo, y = max, label = round(max,2)),
            vjust = -0.5, color = "darkgreen", size = 3) +
  
  scale_fill_manual(values = c("#A6CEE3", "#1F78B4")) +
  
  theme_minimal() +
  labs(
    title = "Comparación del Puntaje GIRAI por Región",
    subtitle = "Boxplot con estadísticos explícitos",
    x = "Región",
    y = "Puntaje GIRAI"
  ) +
  
  theme(legend.position = "none")

# 4. Mostrar
print(grafico_boxplot)


# ---------- ACCIONES GUBERNAMENTALES POR REGION ----------------
datos_ag <- datos %>%
  filter(!is.na(areas_ag), !is.na(GIRAI_region)) %>%
  mutate(region_grupo = case_when(
    GIRAI_region == "América del Norte" ~ "América del Norte",
    GIRAI_region == "América del Sur y Central" ~ "América Central y Sur",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(region_grupo))

grafico_ag <- ggplot(datos_ag, aes(x = region_grupo, y = areas_ag, fill = region_grupo)) +
  
  geom_boxplot(alpha = 0.7) +
  
  theme_minimal() +
  labs(
    title = "Acciones Gubernamentales en IA por Región",
    x = "Región",
    y = "Cantidad de acciones en IA"
  ) +
  
  theme(legend.position = "none")

print(grafico_ag)



# ------------------- CANTIDAD DE LEYES POR REGION ----------------------
# 1. Datos
datos_mng <- datos %>%
  filter(!is.na(areas_mng), !is.na(GIRAI_region)) %>%
  mutate(region_grupo = case_when(
    GIRAI_region == "América del Norte" ~ "América del Norte",
    GIRAI_region == "América del Sur y Central" ~ "América Central y Sur",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(region_grupo))

# 2. Gráfico
grafico_mng <- ggplot(datos_mng, aes(x = region_grupo, y = areas_mng, fill = region_grupo)) +
  
  geom_boxplot(alpha = 0.7) +
  
  theme_minimal() +
  labs(
    title = "Cantidad de Marcos Normativos en IA por Región",
    x = "Región",
    y = "Cantidad de áreas con regulación"
  ) +
  
  theme(legend.position = "none")

print(grafico_mng)



# --------------------- COMPARACION POR PILARES COMPLETO POR REGION -------------------
datos_pilares <- datos %>%
  filter(!is.na(GIRAI_region)) %>%
  mutate(region_grupo = case_when(
    GIRAI_region == "América del Norte" ~ "América del Norte",
    GIRAI_region == "América del Sur y Central" ~ "América Central y Sur",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(region_grupo)) %>%
  select(region_grupo, mng, ag, ane) %>%
  pivot_longer(cols = c(mng, ag, ane), names_to = "pilar", values_to = "valor")

grafico_pilares <- ggplot(datos_pilares, aes(x = pilar, y = valor, fill = region_grupo)) +
  
  geom_boxplot(position = "dodge") +
  
  theme_minimal() +
  labs(
    title = "Comparación de Pilares del GIRAI por Región",
    x = "Pilar",
    y = "Puntaje"
  )

print(grafico_pilares)



# ----------------- RELACION ENTRE EL PUNTAJE GIRAI Y LA ACCION NO ESTATAL POR REGION -------------------------
# 1. Datos
datos_ane <- datos %>%
  filter(!is.na(GIRAI), !is.na(areas_ane), !is.na(GIRAI_region)) %>%
  mutate(region_grupo = case_when(
    GIRAI_region == "América del Norte" ~ "América del Norte",
    GIRAI_region == "América del Sur y Central" ~ "América Central y Sur",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(region_grupo))

# 2. Gráfico
grafico_ane <- ggplot(datos_ane, aes(x = areas_ane, y = GIRAI, color = region_grupo)) +
  
  geom_jitter(width = 0.3, height = 0.3, size = 2, alpha = 0.7) +
  
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_minimal() +
  labs(
    title = "Relación entre Actores No Estatales y Puntaje GIRAI",
    x = "Cantidad de áreas con actores no estatales",
    y = "Puntaje GIRAI",
    color = "Región"
  )

print(grafico_ane)
