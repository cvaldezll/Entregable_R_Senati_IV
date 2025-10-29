############################################################
# El archivo principal del dashboard se debe llamar app.R  #
# Para que shinyapps.io lo reconozca al momento de subirlo #
#                                                          #
# library(rsconnect)
# rsconnect::deployApp(
#   appDir = 'D:/Dropbox/Estudios SENATI/4to Semestre/Lenguaje de Programación para Ciencia de Datos/Entregable/',
#   appName = 'Entregable-R-Senati-IV'
# )
############################################################


cat("\014")               # limpia la consola
source("usar_librerias.R")
source("cargar_data.R")   # tibble "pTbData" y dataframe "pDfCredenciales"
source("estilos.R")       # estilos del dashboard (como CSS)


# FUNCIONES PARA CADA PESTAÑA DEL DASHBOARD
#                           FRONT-END            y BACK-END
source("overall.R")       # poPageOverall()      y poDaoOverall()
source("vehicle_type.R")  # poPageVehicleType()  y poDaoVehicleType()
source("revenue.R")       # poPageRevenue()      y poDaoRevenue()
source("cancellation.R")  # poPageCancellation() y poDaoCancellation()
source("ratings.R")       # poPageRatings()      y poDaoRatings()


# 1. ENCABEZADO O TÍTULO (HEADER)
poHeader <- dashboardHeader(title="Uber Dashboard", disable=FALSE)


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
  tabItems(
    tabItem(tabName="overall"     , poPageOverall()     ), # FRONT-END de overall.R
    tabItem(tabName="vehicle_type", poPageVehicleType() ), # FRONT-END de vehicle_type.R
    tabItem(tabName="revenue"     , poPageRevenue()     ), # FRONT-END de revenue.R
    tabItem(tabName="cancellation", poPageCancellation()), # FRONT-END de cancellation.R
    tabItem(tabName="ratings"     , poPageRatings()     )  # FRONT-END de ratings.R
  )
)


# 4. INTERFÁZ DE USUARIO
poInterfaz <- tagList(
  estilosBody(),   # estilos del dashboardBody
  dashboardPage(poHeader, poSidebar, poBody, skin="black")
)
# Envolviendo en modo seguro (LOGIN):
poInterfaz_secure <- secure_app(poInterfaz, theme="cyborg", background="gray", language="es")


# 5. SERVIDOR (CONTROLLER)
poController        <- source("controller.R")$value
# Envolviendo en modo seguro (LOGIN):
poController_secure <- function(input, output, session) {
  res_auth <- secure_server(check_credentials = check_credentials(pDfCredenciales))
  poController(input, output, session)
}



################################################################################
# El Dashboard de Shiny se genera internamente con el framework de Bootstrap.  #
# Shiny tiene su propia forma de invocar los elementos de bootstrap.           #
# Cada fila (fluidRow) de cada pestaña es un grid de bootstrap de 12 columnas. #
# Cada columna (column) de cada fila también es de 12 columnas.                #
# Cada caja (box) de cada fila o columna también es de 12 columnas.            #
################################################################################

# 6. EJECUCIÓN DEL DASHBOARD
shinyApp(ui=poInterfaz_secure, server=poController_secure)