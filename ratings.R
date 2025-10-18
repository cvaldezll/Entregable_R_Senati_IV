# --->>> CUSTOMER RATINGS - VALORACIONES DE LOS CLIENTES <<<---
# 1. Agrupar por vehicle_type.
# 2. Calcular el promedio de customer_rating para cada grupo.
resultados_dplyr <- datos %>%
  group_by(vehicle_type) %>%
  summarise(
    promedio_rating = mean(customer_rating, na.rm = TRUE)
  )

# Usar pivot_wider para convertir las filas de vehicle_type en columnas
resultados_pivotados <- resultados_dplyr %>%
  pivot_wider(
    names_from = vehicle_type,  # Los valores de esta columna serán los nombres de las nuevas columnas
    values_from = promedio_rating # Los valores de esta columna rellenarán las nuevas columnas
  )

# Imprime el resultado
print("1. VALORACIONES DE LOS CLIENTES")
print(resultados_pivotados)



# --->>> DRIVER RATINGS - CALIFICACIONES DE LOS CONDUCTORES <<<---
resultados_dplyr2 <- datos %>%
  group_by(vehicle_type) %>%
  summarise(
    promedio_rating = mean(driver_ratings, na.rm = TRUE)
  )

resultados_pivotados2 <- resultados_dplyr2 %>%
  pivot_wider(
    names_from = vehicle_type,  # Los valores de esta columna serán los nombres de las nuevas columnas
    values_from = promedio_rating # Los valores de esta columna rellenarán las nuevas columnas
  )

print("2. CALIFICACIONES DE LOS CONDUCTORES")
print(resultados_pivotados2)