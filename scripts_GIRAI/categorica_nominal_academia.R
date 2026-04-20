library(dplyr)
library(ggplot2)
library(stringr)

datos_tipo_academia<- datos %>%
  filter(!is.na(tipo_academia_es)) %>%
  count(tipo_academia_es) %>%
  mutate(porcentaje = round((n / sum(n)) * 100, 1))

distr_tipo_academia <- ggplot(datos_tipo_academia, aes(x = n, y = reorder(str_wrap(tipo_academia_es, width = 28), n))) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = paste0(porcentaje, "%")), hjust = -0.2, size = 3.5) +
  theme_minimal() +
  labs(
    title = "Tipos de Iniciativas de la Academia en IA",
    subtitle = "Frecuencia observada en los países con academia en IA",
    x = "Cantidad de Países",
    y = NULL 
  ) +
  theme(
    # Achicamos un poco la letra y ajustamos el interlineado (lineheight) para que quede prolijo
    axis.text.y = element_text(
                               size = 9, 
                               color = "black", 
                               lineheight = 0.8,
                               hjust = 0,
                               ),
    panel.grid.major.y = element_blank() 
  )

print(distr_tipo_academia)

