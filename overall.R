# --->>> RANGO DE FECHAS DE RESERVA <<<---
# na.rm = TRUE para ignorar NAs si los hay
resul <- datos %>%
  summarise(
    primer_reserva = min(date, na.rm = TRUE),
    ultima_reserva = max(date, na.rm = TRUE)
  )
#print(resul)

# --->>> TOTAL RESERVAS <<<---
resul2 <- datos %>%
  summarise(
    total_reservas = n()
  )
#print(resul2)

# Juntando los tibbles
tibble_resul <- bind_cols(resul, resul2)
print(tibble_resul)



# --->>> DESGLOSE DEL ESTADO DE LA RESERVA - BOOKING STATUS BREAKDOWN <<<---
# Data
datos_pastel <- datos %>%
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

print(datos_pastel_final)



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
    title = "Desglose del Estado de las Reservas",
    fill = "Estados de las Reservas",
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
print(grafico_final_corregido)



# --->>> RIDE VOLUME OVER TIME - VOLUMEN DE VIAJES A LO LARGO DEL TIEMPO <<<---
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
    title = "Volumen de viajes a lo largo del tiempo",
    x = "Mes",
    y = "Reservas"
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