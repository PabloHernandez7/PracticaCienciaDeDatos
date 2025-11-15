library(readxl)
library(dplyr)
library(openxlsx)

datos_original <- read_excel("Tercer Práctico Escenarios.xlsx", sheet = "Hoja1")

#rellena con nulos las combinaciones

escenarios <- datos_original %>%
  tidyr::fill(`Etiqueta Interacción`, `Etiqueta Actividad Resumida`, `Lugar de actividad`, .direction = "down")

combos <- escenarios %>%
  distinct(`Etiqueta Interacción`, `Etiqueta Actividad Resumida`, `Lugar de actividad`) %>%
  arrange(`Etiqueta Interacción`, `Etiqueta Actividad Resumida`, `Lugar de actividad`)


# Cargar workbook existente
wb <- loadWorkbook("Tercer Práctico Escenarios.xlsx")

# Crear nueva hoja
addWorksheet(wb, "Combinaciones")

# Escribir los datos
writeData(wb, sheet = "Combinaciones", combos)

# Guardar sobre el mismo archivo
saveWorkbook(wb, "Tercer Práctico Escenarios.xlsx", overwrite = TRUE)