# FRONT-END
poPageRevenue <- function() {
  tagList(
    fluidRow(
      column(width=12,
             box(title="Ride Distance Distribution", status="primary", solidHeader=TRUE, width=12,
               plotOutput("revenue_ride_distance_distribution") # se actualiza desde el CONTROLLER
             )
      )
    ),
    
    fluidRow(
      column(width = 8,
             box(title="Revenue by Payment Method", status="primary", solidHeader=TRUE, width=12,
               plotOutput("revenue_payment_method") # se actualiza desde el CONTROLLER
             )
      ),
      
      column(width = 4,
             box(title="Top 5 Customers", status="primary", solidHeader=TRUE, width=12,
               DTOutput("revenue_top_customers") # se actualiza desde el CONTROLLER
             ),
             
             box(title="Date", status="primary", solidHeader=TRUE, width=12,
               dateRangeInput("revenue_date_range", label=NULL, start=as.Date("2024-01-01"), end=as.Date("2024-12-30"))
             )
      )
    )
  )
}

# BACK-END
poDaoRevenue <- function(tTbDatos) {
  print("**************************************************")
  print("**************** BackEnd: revenue ****************")
  print("**************************************************")
  print("PRIMEROS REGISTROS:")
  print(head(tTbDatos))
  print("ÚLTIMOS REGISTROS:")
  print(tail(tTbDatos))
  
  
  # Viajes por día, indistinto al mes
  sumatoria <- tTbDatos %>%
    mutate(dia_del_mes = day(date)) %>%
    group_by(dia_del_mes) %>%
    summarise(suma_ride_distance = sum(ride_distance, na.rm = TRUE)) %>%
    ungroup()
  
  # Muestra el resultado
  ###print(sumatoria)
  
  # 2. Creación del gráfico de barras
  grafico <- ggplot(sumatoria, aes(x = factor(dia_del_mes), y = suma_ride_distance)) +
    
    # Gráfico de barras
    geom_col() + 
    
    # Etiquetas y títulos
    labs(x = "Day", y = "Ride Distance" ) +
    
    # Tema y ajustes para legibilidad
    theme_minimal() +
    theme(
      axis.title.x = element_text(face = "bold", size = 12, margin=margin(t=20)),
      axis.title.y = element_text(face = "bold", size = 12, margin=margin(r=20)),
      axis.text.x = element_text(angle = 0, size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid.major.x = element_blank() # elimina las línea verticales del fondo del gráfico
    )
  
  ###print(grafico)
  
  
  
  
  
  # Reemplaza 'su_tibble' con el nombre real de tu tibble
  sumatoria_pago <- tTbDatos %>%
    filter(!is.na(payment_method)) %>%
    group_by(payment_method) %>%
    summarise(total_booking_value = sum(booking_value, na.rm = TRUE)) %>%
    arrange(desc(total_booking_value)) %>%
    mutate(payment_method_factor = fct_inorder(payment_method)) %>% # columna adicional para q ggplot2 respete arrange
    ungroup()
  
  # Muestra el resultado
  ###print(sumatoria_pago)
  
  
  grafico_valor <- ggplot(sumatoria_pago, aes(x = payment_method_factor, y = total_booking_value, fill = payment_method_factor)) +
    
    # Gráfico de columnas
    geom_col() +
    
    # Definición de paleta de colores
    scale_fill_brewer(palette = "Set2") +
    
    # Etiquetas y títulos
    labs(x = "Payment Method", y = "Booking Value") +
    
    # Mejorar la escala del eje Y (útil para grandes sumas de dinero)
    scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
    
    # Sin leyenda
    guides(fill = "none") +
    
    # Tema minimalista y ajustes para ordenar las barras
    theme_minimal() +
    theme(
      axis.title.x = element_text(face = "bold", size = 12, margin=margin(t=20)),
      axis.title.y = element_text(face = "bold", size = 12, margin=margin(r=20)),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10), # angle y hjust para el orden de las barras
      axis.text.y = element_text(face = "bold", size = 10)
    )
  
  # Muestra el gráfico
  ###print(grafico_valor)
  
  
  
  
  
  
  # 1. Calcular el valor total de booking_value por cada cliente
  resumen_clientes <- tTbDatos %>%
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
      customer_id = "T O T A L", # Asignar una etiqueta para la fila de total
      total_booking_value = sum(total_booking_value) # Sumar los 5 valores
    )
  
  # 4. Combinar el Top 5 con la fila de Total
  resultado_final <- top_5_clientes %>%
    bind_rows(total_top_5) %>%
    rename("Customer ID" = customer_id, "Sum of Booking Value" = total_booking_value)
  
  # Muestra el resultado final (Top 5 + Fila Total)
  ###print(resultado_final)
  
  
  resul <- tTbDatos %>%
    summarise(
      primer_reserva = min(date, na.rm = TRUE),
      ultima_reserva = max(date, na.rm = TRUE)
    )
  ###print(resul)
  
  
  
  # ESTO RETORNA AL CONTROLLER EN ENTREGABLE.R QUE ACTUALIZA EL FRONT-END
  return(list(
    grafico_ride_distance_distribution = grafico,        # revenue_ride_distance_distribution
    grafico_revenue_payment_method     = grafico_valor,  # revenue_payment_method
    revenue_top_customers              = resultado_final # revenue_top_customers
  ))
}