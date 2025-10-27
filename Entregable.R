cat("\014")               # limpia la consola
source("usar_librerias.R")
source("cargar_data.R")   # tibble "pTbData"



# FUNCIONES PARA CADA PESTAÑA DEL DASHBOARD
#                           FRONT-END            y BACK-END
source("overall.R")       # poPageOverall()      y poDaoOverall()
source("vehicle_type.R")  # poPageVehicleType()  y poDaoVehicleType()
source("revenue.R")       # poPageRevenue()      y poDaoRevenue()
source("cancellation.R")  # poPageCancellation() y poDaoCancellation()
source("ratings.R")       # poPageRatings()      y poDaoRatings()



# 1. ENCABEZADO O TÍTULO (HEADER)
poHeader <- dashboardHeader(title = "Uber Dashboard", disable = FALSE)



# 2. PANEL DE PESTAÑAS (SIDEBAR)
poSidebar <- dashboardSidebar(
  sidebarMenu(
    id       = "sidebarMenu", # ID
    menuItem("Overall"     , tabName="overall"     , icon=icon("chart-line")     ),
    menuItem("Vehicle Type", tabName="vehicle_type", icon=icon("car")            ),
    menuItem("Revenue"     , tabName="revenue"     , icon=icon("money-bill-wave")),
    menuItem("Cancellation", tabName="cancellation", icon=icon("times-circle")   ),
    menuItem("Ratings"     , tabName="ratings"     , icon=icon("star")           ),
    selected = "overall"      # pestaña inicial
  )
)



# 3. CUERPO DONDE SE DEFINE EL CONTENIDO DE CADA PESTAÑA (BODY)
poBody <- dashboardBody(
  tags$head(
    tags$style(
      HTML("
        .box-header .box-title {
          float: none !important; /* Desactiva la alineación a la izquierda por defecto */
          display: block;         /* Opcional: Ayuda a asegurar el centrado */
          text-align: center;     /* Centra el texto */
        }
      ")
    )
  ),
  tabItems(
    tabItem(tabName="overall"     , poPageOverall()     ), # FRONT-END de overall.R
    tabItem(tabName="vehicle_type", poPageVehicleType() ), # FRONT-END de vehicle_type.R
    tabItem(tabName="revenue"     , poPageRevenue()     ), # FRONT-END de revenue.R
    tabItem(tabName="cancellation", poPageCancellation()), # FRONT-END de cancellation.R
    tabItem(tabName="ratings"     , poPageRatings()     )  # FRONT-END de ratings.R
  )
)



# 4. INTERFÁZ DE USUARIO
poInterfaz <- dashboardPage(poHeader, poSidebar, poBody, skin="black")



# 5. SERVIDOR (CONTROLLER)
poController <- function(input, output, session) {
  # Objeto reactivo global para llenar el parámetro output
  resultados_cache <- reactiveValues(overall=NULL, vehicle=NULL, revenue=NULL, cancellation=NULL, ratings=NULL)
  
  
  # Tibble "pTbData" filtrado para cada BACK-END de cada pestaña
  datos_overall_filtrados      <- reactive({
    req(input$overall_date_range)      # captura fechas de la pestaña "overall"
    pTbData %>% filter(date>=input$overall_date_range[1] & date<=input$overall_date_range[2])
  })
  datos_vehicle_filtrados      <- reactive({
    req(input$vehicle_date_range)      # captura fechas de la pestaña "vehicle_type"
    pTbData %>% filter(date>=input$vehicle_date_range[1] & date<=input$vehicle_date_range[2])
  })
  datos_revenue_filtrados      <- reactive({
    req(input$revenue_date_range)      # captura fechas de la pestaña "revenue"
    pTbData %>% filter(date>=input$revenue_date_range[1] & date<=input$revenue_date_range[2])
  })
  datos_cancellation_filtrados <- reactive({
    req(input$cancellation_date_range) # captura fechas de la pestaña "cancellation"
    pTbData %>% filter(date>=input$cancellation_date_range[1] & date<=input$cancellation_date_range[2])
  })
  datos_rating_filtrados <- reactive({
    #                                  # no hay fechas en la pestaña "ratings"
    pTbData
  })
  
  
  # Se ejecuta al inicio y cuando se cambia de pestaña
  # Ejecuta un BACK-END por cada pestaña
  observe({
    pestaña_activa <- input$sidebarMenu
    if (is.null(pestaña_activa)) return()
    
    print(paste("PESTAÑA ACTIVA:", pestaña_activa))
    
    if (pestaña_activa=="overall") {
      ##############################
      # BACK-END de overall.R      #
      ##############################
      resultados_cache$overall      <- poDaoOverall(datos_overall_filtrados())
    } 
    else if (pestaña_activa=="vehicle_type") {
      ##############################
      # BACK-END de vehicle_type.R #
      ##############################
      resultados_cache$vehicle      <- poDaoVehicleType(datos_vehicle_filtrados())
    } 
    else if (pestaña_activa=="revenue") {
      ##############################
      # BACK-END de revenue.R      #
      ##############################
      resultados_cache$revenue      <- poDaoRevenue(datos_revenue_filtrados())
    } 
    else if (pestaña_activa=="cancellation") {
      ##############################
      # BACK-END de cancellation.R #
      ##############################
      resultados_cache$cancellation <- poDaoCancellation(datos_cancellation_filtrados())
    } 
    else if (pestaña_activa=="ratings") {
      ##############################
      # BACK-END de ratings.R      #
      ##############################
      resultados_cache$ratings      <- poDaoRatings(datos_rating_filtrados())
    }
  })
  
  
  # Actualiza los datos de la pestaña "overall"
  output$overall_total_bookings      <- renderText({
    if (is.null(resultados_cache$overall)) return("Cargando...")
    valor <- resultados_cache$overall$total_reservas # del BACK-END
    paste0(format(round(valor / 1000, 2), nsmall=2), "K")
  })
  output$overall_booking_status_plot <- renderPlot({
    if (is.null(resultados_cache$overall)) return("Cargando...")
    resultados_cache$overall$grafico_pastel          # del BACK-END
  })
  output$overall_ride_volume_plot    <- renderPlot({
    if (is.null(resultados_cache$overall)) return("Cargando...")
    resultados_cache$overall$grafico_linea           # del BACK-END
  })
  
  # Actualiza los datos de la pestaña "vehicle_type"
  output$vehicle_type_table <- renderDT({
    if (is.null(resultados_cache$vehicle)) return("Cargando...")
    
    vehicle_tabla_formateada <- resultados_cache$vehicle$tabla_de_tipos %>% # del BACK-END
      dplyr::mutate(
        `Total Booking value`      = paste0(format(round(`Total Booking value` / 1000 / 1000, 2), nsmall = 2), "M"),
        `Success Booking value`    = paste0(format(round(`Success Booking value` / 1000, 2)     , nsmall = 2), "K"),
        `Avg. Distance Travelled`  = paste0(format(round(`Avg. Distance Travelled` * 1.00, 2)   , nsmall = 2), "" ),
        `Total Distance Travelled` = paste0(format(round(`Total Distance Travelled` / 1000, 2)  , nsmall = 2), "K")
      )
    
    datatable(
      vehicle_tabla_formateada, 
      options   = list(dom='t', paging=FALSE, searching=FALSE, ordering=FALSE),
      rownames  = FALSE,
      selection = 'none'
    )
  })
  
  # Actualiza los datos de la pestaña "revenue"
  output$revenue_ride_distance_distribution <- renderPlot({
    if (is.null(resultados_cache$revenue)) return("Cargando...")
    resultados_cache$revenue$grafico_ride_distance_distribution # del BACK-END
  })
  output$revenue_payment_method             <- renderPlot({
    if (is.null(resultados_cache$revenue)) return("Cargando...")
    resultados_cache$revenue$grafico_revenue_payment_method     # del BACK-END
  })
  output$revenue_top_customers              <- renderDT({
    if (is.null(resultados_cache$revenue)) return("Cargando...")
    
    datatable(
      resultados_cache$revenue$revenue_top_customers,           # del BACK-END
      options   = list(dom='t', paging=FALSE, searching=FALSE, ordering=FALSE),
      rownames  = FALSE,
      selection = 'none'
    )
  })
  
  # Actualiza los datos de la pestaña "cancellation"
  output$cancellation_customers        <- renderPlot({
    if (is.null(resultados_cache$cancellation)) return("Cargando...")
    resultados_cache$cancellation$grafico_customers           # del BACK-END
  })
  output$cancellation_drivers          <- renderPlot({
    if (is.null(resultados_cache$cancellation)) return("Cargando...")
    resultados_cache$cancellation$grafico_drivers             # del BACK-END
  })
  output$cancellation_total_bookings   <- renderText({
    if (is.null(resultados_cache$cancellation)) return("Cargando...")
    valor <- resultados_cache$cancellation$total_bookings     # del BACK-END
    paste0(format(round(valor / 1000, 2), nsmall=2), "K")
  })
  output$cancellation_success_bookings <- renderText({
    if (is.null(resultados_cache$cancellation)) return("Cargando...")
    valor <- resultados_cache$cancellation$success_bookings   # del BACK-END
    paste0(format(round(valor / 1000, 2), nsmall=2), "K")
  })
  output$cancellation_bookings         <- renderText({
    if (is.null(resultados_cache$cancellation)) return("Cargando...")
    valor <- resultados_cache$cancellation$cancelled_bookings # del BACK-END
    paste0(format(round(valor / 1000, 2), nsmall=2), "K")
  })
  output$cancellation_rate             <- renderText({
    if (is.null(resultados_cache$cancellation)) return("Cargando...")
    valor <- resultados_cache$cancellation$cancellation_rate  # del BACK-END
    paste0(format(round(valor * 1.00, 2), nsmall=2), "%")
  })
  
  # Actualiza los datos de la pestaña "ratings"
  output$ratings_customer              <- renderDT({
    if (is.null(resultados_cache$ratings)) return("Cargando...")
    
    ratings_tabla_formateada_customer <- resultados_cache$ratings$customer %>% # del BACK-END
      dplyr::mutate(
        `Rating` = paste0(format(round(`Rating`, 4), nsmall = 4), "")
      )
    
    datatable(
      ratings_tabla_formateada_customer, 
      options   = list(dom='t', paging=FALSE, searching=FALSE, ordering=FALSE),
      rownames  = FALSE,
      selection = 'none'
    )
  })
  output$ratings_driver                <- renderDT({
    if (is.null(resultados_cache$ratings)) return("Cargando...")
    
    ratings_tabla_formateada_driver <- resultados_cache$ratings$driver %>% # del BACK-END
      dplyr::mutate(
        `Rating` = paste0(format(round(`Rating`, 4), nsmall = 4), "")
      )
    
    datatable(
      ratings_tabla_formateada_driver, 
      options   = list(dom='t', paging=FALSE, searching=FALSE, ordering=FALSE),
      rownames  = FALSE,
      selection = 'none'
    )
  })
}



# 6. EJECUCIÓN DEL DASHBOARD
shinyApp(ui=poInterfaz, server=poController)