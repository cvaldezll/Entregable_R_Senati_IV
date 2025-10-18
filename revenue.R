# Reemplaza 'su_tibble' con el nombre real de tu tibble
sumatoria <- datos %>%
  # 1. Crear una nueva columna con el número del día del mes (1-31)
  mutate(
    dia_del_mes = day(date)
  ) %>%
  # 2. Agrupar los datos por el nuevo día del mes
  group_by(dia_del_mes) %>%
  # 3. Calcular la suma de ride_distance para cada día
  summarise(
    suma_ride_distance = sum(ride_distance, na.rm = TRUE)
  ) %>%
  # Opcional: Desagrupar
  ungroup()

# Muestra el resultado
print(sumatoria)

# 2. Creación del gráfico de barras
grafico <- sumatoria %>%
  # Mapeamos 'dia_del_mes' al eje X (como factor) y 'suma_ride_distance' al eje Y
  ggplot(aes(x = factor(dia_del_mes), y = suma_ride_distance)) +
  
  # *** ESTA FUNCIÓN ES LA CLAVE PARA LAS BARRAS ***
  geom_col(fill = "gray") + 
  
  # Etiquetas y títulos
  labs(
    title = "Suma Total de Distancia Recorrida por Día del Mes",
    subtitle = "El valor representa la suma de todos los viajes para ese día (1-31) a través del tiempo.",
    x = "Día del Mes",
    y = "Suma de Distancia (Ride Distance)"
  ) +
  
  # Tema y ajustes para legibilidad
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0),
    panel.grid.major.x = element_blank()
  )

print(grafico)





# Reemplaza 'su_tibble' con el nombre real de tu tibble
sumatoria_pago <- datos %>%
  # 1. Agrupar los datos por el método de pago
  group_by(payment_method) %>%
  # 2. Calcular la suma de booking_value para cada grupo
  summarise(
    total_booking_value = sum(booking_value, na.rm = TRUE)
  ) %>%
  # Opcional: Desagrupar el tibble (buena práctica)
  ungroup()

# Muestra el resultado
print(sumatoria_pago)


grafico_valor <- sumatoria_pago %>%
  # Mapeamos 'payment_method' al eje X y 'total_booking_value' al eje Y
  ggplot(aes(x = payment_method, y = total_booking_value)) +
  
  # Usamos geom_col() para generar las barras
  geom_col(fill = "#882255") + # Color morado oscuro
  
  # Etiquetas y títulos
  labs(
    title = "Ingresos Totales (Booking Value) por Método de Pago",
    x = "Método de Pago",
    y = "Total Booking Value"
  ) +
  
  # Mejorar la escala del eje Y (útil para grandes sumas de dinero)
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  
  # Tema minimalista y ajustes para ordenar las barras
  theme_minimal() +
  theme(
    # Ordena las barras para que la más alta (mayor ingreso) esté primero
    # Puedes omitir esta línea si no quieres que estén ordenadas
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

# Muestra el gráfico
print(grafico_valor)






# 1. Calcular el valor total de booking_value por cada cliente
resumen_clientes <- datos %>%
  group_by(customer_id) %>%
  summarise(
    total_booking_value = sum(booking_value, na.rm = TRUE)
  ) %>%
  ungroup() # Es buena práctica desagrupar antes de ordenar o seleccionar

# 2. Obtener el Top 5
top_5_clientes <- resumen_clientes %>%
  # Ordenar de mayor a menor total_booking_value
  arrange(desc(total_booking_value)) %>%
  # Seleccionar las 5 primeras filas
  head(5)

# 3. Calcular la sumatoria total de esos Top 5
total_top_5 <- top_5_clientes %>%
  summarise(
    customer_id = "Total Top 5", # Asignar una etiqueta para la fila de total
    total_booking_value = sum(total_booking_value) # Sumar los 5 valores
  )

# 4. Combinar el Top 5 con la fila de Total
resultado_final <- top_5_clientes %>%
  bind_rows(total_top_5)

# Muestra el resultado final (Top 5 + Fila Total)
print(resultado_final)


resul <- datos %>%
  summarise(
    primer_reserva = min(date, na.rm = TRUE),
    ultima_reserva = max(date, na.rm = TRUE)
  )
print(resul)
