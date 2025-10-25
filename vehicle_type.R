# Front End
poPageVehicleType <- function() {
  tagList(
    # Fila para el rango de fechas
    fluidRow(
      # Utilizamos un box para simular el área superior con el selector de fecha/rango
      box(title="Date", status="primary", solidHeader=TRUE, width=12,
        # Selector de fechas (simulación)
        dateRangeInput("vehicle_date_range", label=NULL, start=as.Date("2024-01-01"), end=as.Date("2024-12-30"))
      )
    ),
    
    # Fila principal para la tabla (el componente central de la imagen)
    fluidRow(
      box(title="Metrics", status="primary", solidHeader=TRUE, width=12,
        DTOutput("vehicle_type_table")
      )
    )
  )
}

# Back End
poDaoVehicleType <- function(tTbDatos) {
  print("**************** BackEnd: vehicle_type ****************")

  datos_agrupados <- tTbDatos %>%
    group_by(vehicle_type) %>%
    summarise(
      "Total Booking value"      = sum(booking_value, na.rm=TRUE),
      "Success Booking value"    = sum(booking_status=="Completed", na.rm=TRUE),
      "Avg. Distance Travelled"  = mean(ride_distance, na.rm=TRUE),
      "Total Distance Travelled" = sum(ride_distance, na.rm=TRUE)
    ) %>%
    rename("Vehicle Type" = vehicle_type)
  
  print(datos_agrupados)
  
  
  
  # ESTO ES PARA poBackEnd DE ENTREGABLE.R
  return(list(
    tabla_de_tipos  = datos_agrupados
  ))
}