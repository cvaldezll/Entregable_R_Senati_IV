# FRONT-END
poPageCancellation <- function() {
  tagList(
    fluidRow(
      column(width = 8,
             box(title="Cancelled Rides by Customers", status="primary", solidHeader=TRUE, width=12, 
                 plotOutput("cancellation_customers") # se actualiza desde el CONTROLLER
             ),
             
             box(title="Cancelled Rides by Drivers", status="primary", solidHeader=TRUE, width=12, 
                 plotOutput("cancellation_drivers") # se actualiza desde el CONTROLLER
             )
      ),
      
      column(width = 4,
             box(title="Date", status="primary", solidHeader=TRUE, width=12,
               dateRangeInput("cancellation_date_range", label=NULL, start=as.Date("2024-01-01"), end=as.Date("2024-12-30"))
             ),
             
             box(title="Total Bookings", status="primary", solidHeader=TRUE, width=12,
                 div(style="text-align: center; font-size: 2rem;", 
                     textOutput("cancellation_total_bookings")) # se actualiza desde el CONTROLLER
             ),
             
             box(title="Success Bookings", status="primary", solidHeader=TRUE, width=12,
                 div(style="text-align: center; font-size: 2rem;", 
                     textOutput("cancellation_success_bookings")) # se actualiza desde el CONTROLLER
             ),
             
             box(title="Cancelled Bookings", status="primary", solidHeader=TRUE, width=12,
                 div(style="text-align: center; font-size: 2rem;", 
                     textOutput("cancellation_bookings")) # se actualiza desde el CONTROLLER
             ),
             
             box(title="Cancellation Rate", status="primary", solidHeader=TRUE, width=12,
                 div(style="text-align: center; font-size: 2rem;", 
                     textOutput("cancellation_rate")) # se actualiza desde el CONTROLLER
             )
      )
    )
  )
}

# BACK-END
poDaoCancellation <- function(tTbDatos) {
  print("*******************************************************")
  print("**************** BackEnd: cancellation ****************")
  print("*******************************************************")
  print("PRIMEROS REGISTROS:")
  print(head(tTbDatos))
  print("ÚLTIMOS REGISTROS:")
  print(tail(tTbDatos))
  
  
  # --->>> VIAJES CANCELADOS POR CLIENTES - CANCELLED RIDES BY CUSTOMERS <<<---
  # Data
  datos_pastel <- tTbDatos %>%
    filter(booking_status == "Cancelled by Customer") %>%
    group_by(reason_for_cancelling_by_customer) %>%
    summarise(Conteo = n()) %>%
    ungroup()
  
  datos_pastel_final <- datos_pastel %>%
    
    # Ordenar los datos (por ejemplo, de mayor a menor conteo para mejor visualización)
    arrange(desc(Conteo)) %>%
    
    # Calcular variables clave para el gráfico de pastel
    mutate(
      Total_General = sum(Conteo),
      Porcentaje = Conteo / Total_General,
      
      # 1. Crear las etiquetas de valor (K + Porcentaje)
      Etiqueta_Valor = paste0(
        format(round(Conteo / 1000, 2), nsmall = 2), "K (", 
        round(Porcentaje * 100, 2), "%)"
      ),
      
      # 2. Calcular la posición inicial de cada rebanada
      # Usamos cumsum() para apilar y calcular la posición de inicio
      Posicion_Y = cumsum(Porcentaje) - Porcentaje / 2,
      
      # 3. Calcular la posición de las etiquetas (para colocarlas fuera, cerca de la mitad de cada rebanada)
      Posicion_Texto_X = 1.3 # Un valor mayor a 1 para colocar las etiquetas fuera del borde del pastel
    )
  
  ###print(datos_pastel_final)
  
  
  
  # Gráfico
  grafico_final_corregido <- ggplot(datos_pastel_final, aes(x = factor(1), y = Porcentaje, fill = reason_for_cancelling_by_customer)) +
    
    # Gráfico de barras que después se transformará a pastel
    geom_bar(stat = "identity", width = 1, color = "white") +
    
    # Coloca las etiquetas afuera
    geom_text(
      aes(y = Posicion_Y, label = Etiqueta_Valor), # mapea 'y' a la posición central calculada para que la etiqueta se alinee con su rebanada
      x = 1.5, # fijamos la posición X (e.g., 1.5) para ubicar el texto fuera del pastel (1 es el borde)
      size = 4, # tamaño del texto
      color = "black", # aseguramos que el color del texto sea visible
      fontface = "bold" # negrita
    ) +
    
    # Transforma el gráfico de barra para apariencia de pastel
    coord_polar(theta = "y", start = 0) +
    
    # Definición de paleta de colores
    scale_fill_brewer(palette = "Set2") +
    
    # Personalización y Títulos
    labs(fill = "Cancellation Reason", x = NULL, y = NULL) +
    
    # Tema (Limpieza)
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 10)
    )
  
  # Mostrar el gráfico
  ###print(grafico_final_corregido)
  
  
  
  # --->>> VIAJES CANCELADOS POR CONDUCTORES - CANCELLED RIDES BY DRIVERS <<<---
  # Data
  datos_pastel2 <- tTbDatos %>%
    filter(booking_status == "Cancelled by Driver") %>%
    group_by(driver_cancellation_reason) %>%
    summarise(Conteo = n()) %>%
    ungroup()
  
  datos_pastel_final2 <- datos_pastel2 %>%
    
    # Ordenar los datos (por ejemplo, de mayor a menor conteo para mejor visualización)
    arrange(desc(Conteo)) %>%
    
    # Calcular variables clave para el gráfico de pastel
    mutate(
      Total_General = sum(Conteo),
      Porcentaje = Conteo / Total_General,
      
      # 1. Crear las etiquetas de valor (K + Porcentaje)
      Etiqueta_Valor = paste0(
        format(round(Conteo / 1000, 2), nsmall = 2), "K (", 
        round(Porcentaje * 100, 2), "%)"
      ),
      
      # 2. Calcular la posición inicial de cada rebanada
      # Usamos cumsum() para apilar y calcular la posición de inicio
      Posicion_Y = cumsum(Porcentaje) - Porcentaje / 2,
      
      # 3. Calcular la posición de las etiquetas (para colocarlas fuera, cerca de la mitad de cada rebanada)
      Posicion_Texto_X = 1.3 # Un valor mayor a 1 para colocar las etiquetas fuera del borde del pastel
    )
  
  ###print(datos_pastel_final2)
  
  
  
  # Gráfico
  grafico_final_corregido2 <- ggplot(datos_pastel_final2, aes(x = factor(1), y = Porcentaje, fill = driver_cancellation_reason)) +
    
    # Gráfico de barras que después se transformará a pastel
    geom_bar(stat = "identity", width = 1, color = "white") +
    
    # Coloca las etiquetas afuera
    geom_text(
      aes(y = Posicion_Y, label = Etiqueta_Valor), # mapea 'y' a la posición central calculada para que la etiqueta se alinee con su rebanada
      x = 1.5, # fijamos la posición X (e.g., 1.5) para ubicar el texto fuera del pastel (1 es el borde)
      size = 4, # tamaño del texto
      color = "black", # aseguramos que el color del texto sea visible
      fontface = "bold" # negrita
    ) +
    
    # Transforma el gráfico de barra para apariencia de pastel
    coord_polar(theta = "y", start = 0) +
    
    # Definición de paleta de colores
    scale_fill_brewer(palette = "Set2") +
    
    # Personalización y Títulos
    labs(fill = "Cancellation Reason", x = NULL, y = NULL) +
    
    # Tema (Limpieza)
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 10)
    )
  
  # Mostrar el gráfico
  ###print(grafico_final_corregido2)
  
  
  
  # --->>> RANGO DE FECHAS DE RESERVA <<<---
  # na.rm = TRUE para ignorar NAs si los hay
  resul <- tTbDatos %>%
    summarise(
      primer_reserva = min(date, na.rm = TRUE),
      ultima_reserva = max(date, na.rm = TRUE)
    )
  #print(resul)
  
  
  
  # --->>> TOTAL RESERVAS <<<---
  resul2 <- tTbDatos %>%
    summarise(
      total_reservas = n()
    )
  #print(resul2)
  
  
  
  # --->>> RESERVAS DE ÉXITO <<<---
  resul3 <- tTbDatos %>%
    filter(booking_status == "Completed") %>%
    summarise(
      reservas_exito = n()
    )
  #print(resul3)
  
  
  
  # --->>> RESERVAS CANCELADAS <<<---
  # 1. Obtener el total de filas del tibble original
  total_filas_original <- nrow(tTbDatos)
  
  # 2. Tu código para obtener el conteo de canceladas
  resul4 <- tTbDatos %>%
    filter(booking_status == "Cancelled by Customer" | booking_status == "Cancelled by Driver") %>%
    summarise(
      reservas_cancelado = n()
    )
  
  # 3. Calcular el porcentaje
  porcentaje_cancelado <- resul4 %>%
    mutate(
      Porcentaje = (reservas_cancelado / total_filas_original) * 100
    )
  
  # Mostrar el resultado
  #print(porcentaje_cancelado)
  
  # Juntando los tibbles:
  tibble_final <- bind_cols(resul, resul2, resul3, resul4, porcentaje_cancelado)
  ###print(tibble_final)
  
  
  
  # ESTO RETORNA AL CONTROLLER EN ENTREGABLE.R QUE ACTUALIZA EL FRONT-END
  return(list(
    grafico_customers  = grafico_final_corregido,  # cancellation_customers
    grafico_drivers    = grafico_final_corregido2, # cancellation_drivers
    total_bookings     = resul2[1],                # cancellation_total_bookings
    success_bookings   = resul3[1],                # cancellation_success_bookings
    cancelled_bookings = resul4[1],                # cancellation_bookings
    cancellation_rate  = porcentaje_cancelado[1,2] # cancellation_rate
  ))
}