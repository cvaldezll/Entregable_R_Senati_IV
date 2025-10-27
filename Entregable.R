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
poController <- source("controller.R")$value



# 6. EJECUCIÓN DEL DASHBOARD
shinyApp(ui=poInterfaz, server=poController)