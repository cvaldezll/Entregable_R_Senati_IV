# FRONT-END
poPageRatings <- function() {
  tagList(
    fluidRow(
      column(width=6,
             box(title="Customer Ratings", status="primary", solidHeader=TRUE, width=12, 
                 DTOutput("ratings_customer")
             )
      ),
      
      column(width=6,
             box(title="Driver Ratings", status="primary", solidHeader=TRUE, width=12, 
                 DTOutput("ratings_driver")
             )
      )
    ),
  )
}

# BACK-END
poDaoRatings <- function(tTbDatos) {
  print("**************** BackEnd: ratings ****************")
  
  # --->>> CUSTOMER RATINGS - VALORACIONES DE LOS CLIENTES <<<---
  # 1. Agrupar por vehicle_type.
  # 2. Calcular el promedio de customer_rating para cada grupo.
  resultados_dplyr <- tTbDatos %>%
    group_by(vehicle_type) %>%
    summarise(promedio_rating = mean(customer_rating, na.rm = TRUE)) %>%
    arrange(desc(promedio_rating)) %>%
    rename(
      "Vehicle" = vehicle_type,
      "Rating" = promedio_rating
    )
  
  # Usar pivot_wider para convertir las filas de vehicle_type en columnas
  resultados_pivotados <- resultados_dplyr %>%
    pivot_wider(
      names_from = Vehicle,  # Los valores de esta columna serán los nombres de las nuevas columnas
      values_from = Rating # Los valores de esta columna rellenarán las nuevas columnas
    )
  
  # Imprime el resultado
  ###print("1. VALORACIONES DE LOS CLIENTES")
  ###print(resultados_pivotados)
  
  
  
  # --->>> DRIVER RATINGS - CALIFICACIONES DE LOS CONDUCTORES <<<---
  resultados_dplyr2 <- tTbDatos %>%
    group_by(vehicle_type) %>%
    summarise(promedio_rating = mean(driver_ratings, na.rm = TRUE)) %>%
    arrange(desc(promedio_rating)) %>%
    rename(
      "Vehicle" = vehicle_type,
      "Rating" = promedio_rating
    )
  
  resultados_pivotados2 <- resultados_dplyr2 %>%
    pivot_wider(
      names_from = Vehicle,  # Los valores de esta columna serán los nombres de las nuevas columnas
      values_from = Rating # Los valores de esta columna rellenarán las nuevas columnas
    )
  
  ###print("2. CALIFICACIONES DE LOS CONDUCTORES")
  ###print(resultados_pivotados2)
  
  
  
  # ESTO ES PARA EL CONTROLLER EN ENTREGABLE.R
  # PARA ACTUALIZAR LA DATA EN EL FRONT-END
  return(list(
    customer = resultados_dplyr, # ratings_customer
    driver   = resultados_dplyr2 # ratings_driver
  ))
}