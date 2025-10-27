# Estilos del dashboardBody
estilosBody <- function() {
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
  )
}