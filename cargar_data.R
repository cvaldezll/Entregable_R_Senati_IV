# IMPORTANDO DATA
datos <- read_csv(
  "data/ncr_ride_bookings.csv",
  na = c("null", ""),     # Trata "null" y cadenas vacías como NA (valor faltante)
  quote = "\"\"\""        # Indica el carácter de comilla que rodea los campos (aquí, triple comilla)
)
head(datos)



# LIMPIANDO ESTRUCTURA
datos <- datos %>%
  mutate(across(where(is.character), ~ str_remove_all(., "\""))) %>%
  clean_names()
head(datos)



# ESTRUCTURA FINAL
# 1. Definimos una función segura para obtener la clase como una sola cadena
get_single_class <- function(x) {
  # Toma la clase y la convierte a una sola cadena de texto (ej. "hms, difftime")
  return(toString(class(x)))
}

# 2. Obtenemos los nombres y aplicamos la función segura a todos los tipos
nombres_columnas <- names(datos)
tipos_columnas <- datos %>% 
  purrr::map_chr(get_single_class) # Aplicamos la función segura

# 3. Combinamos y formateamos la lista verticalmente
map2_chr(nombres_columnas, tipos_columnas, ~ paste0(.x, " (", .y, ")")) %>%
  cat(sep = "\n")



# DATA DE MUESTRA
print(head(datos, 50))