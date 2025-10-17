datos_agrupados <- datos %>%
  group_by(vehicle_type) %>%
  summarise(
    valor_total_de_la_reserva = sum(booking_value, na.rm = TRUE),
    recorridos_completados = sum(booking_status == "Completed", na.rm = TRUE),
    promedio_recorridos_completados = mean(ride_distance, na.rm = TRUE),
    suma_recorridos_completados = sum(ride_distance, na.rm = TRUE)
  ) %>%
  rename(tipo_de_vehiculo = vehicle_type)

print(datos_agrupados)

