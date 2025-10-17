# Título: Entregable R
# Autores: Christian Valdez, Jair Pacahuala, Marck de la Cruz
# Formato: R
# Editor: R Studio
# Dashboard: Shiny



# Llamar librerías .R
source("usar_librerias.R")
source("cargar_data.R")



# ==== Datos de ejemplo ====
set.seed(123)

data <- data.frame(
  day = 1:30,
  ride_distance = sample(4000:9000, 30, replace = TRUE),
  payment_method = sample(c("UPI", "Cash", "Uber Wallet", "Credit Card", "Debit Card"), 30, replace = TRUE),
  booking_value = sample(50000:200000, 30, replace = TRUE)
)

top_customers <- data.frame(
  Customer_ID = paste0("CID", sample(100000:999999, 5)),
  Booking_Value = sample(3800:4100, 5)
)

# ==== Interfaz ====
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "UBER Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overall", tabName = "overall", icon = icon("dashboard")),
      menuItem("Vehicle Type", tabName = "vehicle", icon = icon("car")),
      menuItem("Revenue", tabName = "revenue", icon = icon("money-bill")),
      menuItem("Cancellation", tabName = "cancel", icon = icon("ban")),
      menuItem("Ratings", tabName = "star", icon = icon("star"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "revenue",
              fluidRow(
                box(width = 12, status = "primary",
                    title = "Ride Distance Distribution",
                    plotlyOutput("ride_plot", height = "300px"))
              ),
              fluidRow(
                box(width = 8, status = "primary",
                    title = "Revenue by Payment Method",
                    plotlyOutput("payment_plot", height = "300px")),
                box(width = 4, status = "info",
                    title = "Top 5 Customers",
                    tableOutput("top_customers"))
              )
      )
    )
  )
)

# ==== Servidor ====
server <- function(input, output) {
  
  # Gráfico 1 - Ride Distance Distribution
  output$ride_plot <- renderPlotly({
    plot_ly(
      data,
      x = ~day,
      y = ~ride_distance,
      type = "bar",
      marker = list(color = "#7f8c8d")
    ) %>%
      layout(
        title = "Ride Distance Distribution",
        xaxis = list(title = "Day"),
        yaxis = list(title = "Ride Distance"),
        plot_bgcolor = "#ecf0f1",
        paper_bgcolor = "#ecf0f1"
      )
  })
  
  # Gráfico 2 - Revenue by Payment Method
  output$payment_plot <- renderPlotly({
    df <- data %>%
      group_by(payment_method) %>%
      summarise(Total = sum(booking_value))
    
    plot_ly(
      df,
      x = ~payment_method,
      y = ~Total,
      type = "bar",
      marker = list(color = "#95a5a6")
    ) %>%
      layout(
        title = "Revenue by Payment Method",
        xaxis = list(title = "Payment Method"),
        yaxis = list(title = "Booking Value"),
        plot_bgcolor = "#ecf0f1",
        paper_bgcolor = "#ecf0f1"
      )
  })
  
  # Tabla - Top 5 Customers
  output$top_customers <- renderTable({
    top_customers
  })
}

# ==== Ejecutar ====
shinyApp(ui, server)