library(ggplot2)

# Gráfico de dispersión multivariado
grafico_multivariado <- ggplot(datos, aes(x = mng, y = GIRAI, color = academia)) +
  geom_point(size = 3, alpha = 0.7) + # Puntos
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + # Línea de tendencia
  theme_minimal() +
  scale_color_manual(values = c("No" = "tomato", "Sí" = "steelblue")) +
  labs(
    title = "El camino hacia la cima: Interacción entre Estado y Privados",
    subtitle = "Relación entre Marcos Normativos y el Índice GIRAI, agrupado por acción privada",
    x = "Acción del Estado: Marcos Normativos (0-100)",
    y = "Puntaje Global GIRAI (0-100)",
    color = "Sector Privado:"
  ) +
  theme(legend.position = "bottom")

print(grafico_multivariado)
