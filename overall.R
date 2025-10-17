# RANGO DE FECHAS DE RESERVA
# na.rm = TRUE para ignorar NAs si los hay
resul <- datos %>%
  summarise(
    primer_reserva = min(date, na.rm = TRUE),
    ultima_reserva = max(date, na.rm = TRUE)
  )
print(resul)



# TOTAL RESERVAS
resul <- datos %>%
  summarise(
    total_reservas = n()
  )
print(resul)



# DATA PARA EL GRAFICO
datos_pastel <- datos %>%
  # cuenta por booking_status, genera columna de nombre n
  count(booking_status) %>%
  # ordenando por la columna n
  arrange(n) %>%
  # definiendo columnas adicionales
  mutate(
    # n es el total por booking_status
    porcentaje = n/sum(n),
    # etiqueta para cada booking_status
    etiqueta_porc = percent(porcentaje, accuracy = 0.01),
    # posición de la etiqueta de cada booking_status dentro del pastel
    posicion_y = cumsum(porcentaje)-porcentaje/2,
  )
print(datos_pastel)



# EL GRAFICO
grafico <- ggplot(
  datos_pastel, 
  #aes(x = "", y = porcentaje, fill = booking_status)
  aes(x = "", y = porcentaje, fill = paste0(booking_status," ",etiqueta_porc))
) +
  
  # Capa de Geometría (barras)
  geom_bar(stat = "identity", width = 1) +
  
  # Capa de Coordenadas (convierte a pastel)
  coord_polar("y", start = 0) +
  
  # Capa de Etiquetas (solo el porcentaje)
  #geom_text(aes(y = posicion_y, label = etiqueta_porc), color = "black") +
  
  # Etiquetas y limpieza básica de ejes
  labs(
    title = "Distribución de Estados de Reserva",
    x = NULL, y = NULL,
    fill = "Estado"
  ) +
  
  # Tema: Elimina todos los elementos no esenciales
  theme_void()
print(grafico)