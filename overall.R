# FRONT-END
poPageOverall <- function() {
  tagList(
    fluidRow(
      column(width=4,
             box(title="Date", status="primary", solidHeader=TRUE, width=12,
                 dateRangeInput("overall_date_range", label=NULL, start=as.Date("2024-01-01"), end=as.Date("2024-12-30"))
             ),
             
             box(title="Total Bookings", status="primary", solidHeader=TRUE, width=12,
                 div(style="text-align: center; font-size: 2rem;", 
                     textOutput("overall_total_bookings")) # del CONTROLLER
             )
      ),
      
      column(width=8,
             box(title="Booking Status Breakdown", status="primary", solidHeader=TRUE, width=12,
                 plotOutput("overall_booking_status_plot") # del CONTROLLER
             )
      )
    ), 
    
    fluidRow(
      column(width=12,
             box(title="Ride Volume Over Time", status="primary", solidHeader=TRUE, width=12,
                 plotOutput("overall_ride_volume_plot") # del CONTROLLER
             )
      )
    )
  )
}

# BACK-END
poDaoOverall <- function(tTbDatos) {
  print("**************** BackEnd: overall ****************")
  
  
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
  
  # Juntando los tibbles
  tibble_resul <- bind_cols(resul, resul2)
  ###print(tibble_resul)
  
  
  
  # --->>> DESGLOSE DEL ESTADO DE LA RESERVA - BOOKING STATUS BREAKDOWN <<<---
  # Data
  datos_pastel <- tTbDatos %>%
    filter(booking_status != "Incomplete") %>%
    group_by(booking_status) %>%
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
  grafico_final_corregido <- ggplot(datos_pastel_final, 
                                    aes(x = factor(1), y = Porcentaje, 
                                        fill = booking_status)) +
    
    # 1. Dibuja las rebanadas
    geom_bar(stat = "identity", width = 1, color = "white") +
    
    # 2. Coloca las etiquetas afuera
    geom_text(
      # Mapea 'y' a la posición central calculada para que la etiqueta se alinee con su rebanada
      aes(y = Posicion_Y, 
          label = Etiqueta_Valor),
      
      # Fijamos la posición X (e.g., 1.5) para ubicar el texto fuera del pastel (1 es el borde)
      x = 1.5, 
      
      size = 4, 
      color = "black" # Aseguramos que el color del texto sea visible
    ) +
    
    # 3. Transformación a Polares
    # Es crucial para la apariencia del pastel
    coord_polar(theta = "y", start = 0) +
    
    # 4. Personalización y Títulos
    labs(
      #title = "Desglose del Estado de las Reservas",
      fill = "Booking Status",
      x = NULL,
      y = NULL
    ) +
    
    # 5. Tema (Limpieza)
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "right"
    )
  
  # Mostrar el gráfico
  ###print(grafico_final_corregido)
  
  
  
  # --->>> RIDE VOLUME OVER TIME - VOLUMEN DE VIAJES A LO LARGO DEL TIEMPO <<<---
  # DATA
  meses_es <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")
  total_por_mes <- tTbDatos %>%
    {
      # Creamos columna temporal local, sin alterar tTbDatos
      data.frame(
        Mes = meses_es[month(lubridate::ymd(.$date), label = FALSE)],
        stringsAsFactors = FALSE
      ) %>%
        mutate(Mes = factor(Mes, levels = meses_es)) %>%
        count(Mes, name = "Total_Viajes") %>%
        complete(Mes = factor(meses_es, levels = meses_es),
                 fill = list(Total_Viajes = 0)) %>%
        arrange(match(Mes, meses_es))
    }
  
  # Mostrar el resultado
  ###print(total_por_mes)
  
  
  vector_viajes <- total_por_mes %>% 
    pull(Total_Viajes)
  
  datos_viajes <- data.frame(
    Mes = factor(c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                   "Jul", "Ago", "Set", "Oct", "Nov", "Dic"),
                 levels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                            "Jul", "Ago", "Set", "Oct", "Nov", "Dic")),
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
      #title = "Volumen de viajes a lo largo del tiempo",
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
  ###print(grafico_area)
  
  
  
  # ESTO ES PARA EL CONTROLLER EN ENTREGABLE.R
  # PARA ACTUALIZAR LA DATA EN EL FRONT-END
  return(list(
    total_reservas = resul2[1],               # overall_total_bookings
    grafico_pastel = grafico_final_corregido, # overall_booking_status_plot
    grafico_linea  = grafico_area             # overall_ride_volume_plot
  ))
}