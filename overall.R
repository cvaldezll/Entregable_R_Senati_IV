# --->>> RANGO DE FECHAS DE RESERVA <<<---
# na.rm = TRUE para ignorar NAs si los hay
resul <- datos %>%
  summarise(
    primer_reserva = min(date, na.rm = TRUE),
    ultima_reserva = max(date, na.rm = TRUE)
  )
print(resul)



# --->>> TOTAL RESERVAS <<<---
resul <- datos %>%
  summarise(
    total_reservas = n()
  )
print(resul)


# --->>> BOOKING STATUS BREAKDOWN <<<---
# DATA
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



# GRAFICO
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



# --->>> RIDE VOLUME OVER TIME <<<---
# DATA
total_por_mes <- datos %>%
  
  # 1. Crear la columna del mes (como nombre del mes)
  # Usamos month() de lubridate con label = TRUE para obtener el nombre corto del mes
  mutate(Mes = month(date, label = TRUE, abbr = TRUE)) %>%
  
  # 2. Agrupar por la columna del mes
  group_by(Mes) %>%
  
  # 3. Contar el número de filas en cada grupo y asignarle un nombre
  summarise(Total_Viajes = n()) %>%
  
  # (Opcional) Desagrupar el resultado para futuras operaciones
  ungroup()

# Mostrar el resultado
print(total_por_mes)


vector_viajes <- total_por_mes %>% 
  pull(Total_Viajes)

datos_viajes <- data.frame(
  Mes = factor(c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                 "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre"),
               levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                          "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")),
  Volumen = vector_viajes
)



# GRAFICO
grafico_area <- ggplot(datos_viajes, aes(x = Mes, y = Volumen, group = 1)) +
  
  # 1. Gráfico de Área (relleno)
  # Usamos 'fill' para el color de relleno y 'alpha' para la transparencia si es necesario
  geom_area(fill = "skyblue", color = NA, alpha = 0.8) +
  
  # 2. Línea sobre el área (para el borde)
  # Usamos 'color' para el color de la línea y 'size' para el grosor
  geom_line(color = "darkblue", size = 1) +
  
  # 3. Personalización de Ejes y Títulos
  labs(
    title = "Ride Volume Over Time",
    x = "Month",
    y = "Count of Booking ID"
  ) +
  
  # 4. Ajuste del Eje Y
  # Para un mejor visual, ajustamos los límites para que se parezca al gráfico (que empieza cerca de 12,000)
  ylim(min(datos_viajes$Volumen) - 100, max(datos_viajes$Volumen) + 100) +
  
  # 5. Temas y Formato (para una apariencia limpia por defecto)
  theme_minimal() +
  
  # 6. Ajustar el ángulo de las etiquetas del eje X si es necesario (no necesario en este caso)
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Mostrar el gráfico
print(grafico_area)