# SCRIPT TP3 - Escenarios: medias ponderadas de emociones por minutos
# Requisitos: readxl, dplyr, tidyr, writexl
# Ejecutar en RStudio con el Excel "Tercer Práctico Escenarios.xlsx" en el working dir.

# instalar paquetes si es necesario (descomentá si faltan)
# install.packages(c("readxl","dplyr","tidyr","writexl","stringr"))

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(stringr)

# ---------- 1) Leer datos ----------
file_in <- "Tercer Práctico Escenarios.xlsx"

hoja1 <- "Hoja1"        
hoja2 <- "Combinaciones" 

df <- read_excel(file_in, sheet = hoja1)
map <- read_excel(file_in, sheet = hoja2)

# ---------- 2) Normalizar nombres ----------
df <- df %>%
  mutate(
    `Etiqueta Interacción` = str_trim(`Etiqueta Interacción`),
    `Etiqueta Actividad Resumida` = str_trim(`Etiqueta Actividad Resumida`),
    `Lugar de actividad` = str_trim(`Lugar de actividad`)
  )

map <- map %>%
  mutate(
    `Etiqueta Interacción` = str_trim(`Etiqueta Interacción`),
    `Etiqueta Actividad Resumida` = str_trim(`Etiqueta Actividad Resumida`),
    `Lugar de actividad` = str_trim(`Lugar de actividad`),
    CLASIFICACIÓN = str_trim(CLASIFICACIÓN)
  )

# ---------- 3) Emociones y columna minutos ----------
emotions <- c("Preocupación","Prisa","Irritación","Depresión","Tensión","Calma","Disfrute")
col_minutes <- "Total en minutos de la actividad"
col_agusto <- "A_gusto"

# ---------- 4) Imputar NAs: medias globales para las 7 emociones (NO A_gusto) ----------
# calcular medias globales (sobre todas las filas, excluyendo NA)
global_means <- df %>%
  summarise(across(all_of(emotions), ~ mean(.x, na.rm = TRUE))) %>%
  unlist()

cat("Medias globales (usadas para imputar NAs):\n")
print(global_means)

# Imputación en df (reemplazar NA por la media global)
df_imp <- df %>%
  mutate(across(all_of(emotions), ~ ifelse(is.na(.x), global_means[cur_column()], .x)))

# ---------- 5) Unir la clasificación desde la hoja "Combinaciones" ----------
# Join por las tres columnas de mapping
df_joined <- df_imp %>%
  left_join(map, by = c("Etiqueta Interacción", "Etiqueta Actividad Resumida", "Lugar de actividad"))

# ---------- 6) Calcular ponderaciones (emoción * minutos) ----------
# Crear columnas de ponderación para las 7 emociones y A_gusto también (sin imputar)
df_weighted <- df_joined %>%
  mutate(across(all_of(emotions), ~ .x * .data[[col_minutes]], .names = "{.col}_pond")) %>%
  mutate(!!paste0(col_agusto, "_pond") := .data[[col_agusto]] * .data[[col_minutes]])

# ---------- 7) Agrupar por persona y CLASIFICACIÓN y resumir ----------
grouped <- df_weighted %>%
  group_by(identificacion, CLASIFICACIÓN) %>%
  summarise(
    total_minutos = sum(.data[[col_minutes]], na.rm = TRUE),
    # sumas ponderadas para las emociones (7)
    across(all_of(paste0(emotions, "_pond")), ~ sum(.x, na.rm = TRUE), .names = "sum_{.col}"),
    # suma ponderada A_gusto 
    sum_A_gusto_pond = sum(.data[[paste0(col_agusto, "_pond")]], na.rm = TRUE),
    # contar registros y minutos no nulos
    n_registros = n(),
    .groups = "drop"
  )

# ---------- 8) Calcular media ponderada (dividir por total_minutos) ----------
# Para evitar division por cero, ponemos NA cuando total_minutos == 0
calc_weighted_mean <- function(sum_col, total_min_col) {
  dplyr::if_else(total_min_col > 0, sum_col / total_min_col, NA_real_)
}

# Agregamos columnas finales con las medias ponderadas
res <- grouped %>%
  mutate(
    Preocupacion_ponderada = calc_weighted_mean(sum_Preocupación_pond, total_minutos),
    Prisa_ponderada        = calc_weighted_mean(sum_Prisa_pond, total_minutos),
    Irritacion_ponderada   = calc_weighted_mean(sum_Irritación_pond, total_minutos),
    Depresion_ponderada    = calc_weighted_mean(sum_Depresión_pond, total_minutos),
    Tension_ponderada      = calc_weighted_mean(sum_Tensión_pond, total_minutos),
    Calma_ponderada        = calc_weighted_mean(sum_Calma_pond, total_minutos),
    Disfrute_ponderada     = calc_weighted_mean(sum_Disfrute_pond, total_minutos),
    A_gusto_ponderada      = calc_weighted_mean(sum_A_gusto_pond, total_minutos)
  ) %>%
  # Orden lógico de columnas
  select(
    identificacion, CLASIFICACIÓN, n_registros, total_minutos,
    starts_with("Preocupacion_ponderada"),
    starts_with("Prisa_ponderada"),
    starts_with("Irritacion_ponderada"),
    starts_with("Depresion_ponderada"),
    starts_with("Tension_ponderada"),
    starts_with("Calma_ponderada"),
    starts_with("Disfrute_ponderada"),
    starts_with("A_gusto_ponderada")
  )

# ---------- 9) A_gusto solo relevante en Socialización y Relación Laboral ----------
res <- res %>%
  mutate(
    A_gusto_ponderada = ifelse(CLASIFICACIÓN %in% c("Socialización", "Relación Laboral"),
                               A_gusto_ponderada, NA_real_)
  )

# ---------- 10) Dividir por categoría y preparar hojas para exportar ----------
sheet_Deber <- res %>% filter(CLASIFICACIÓN == "Deber")
sheet_Ocio  <- res %>% filter(CLASIFICACIÓN == "Ocio y Recreación")
sheet_Social <- res %>% filter(CLASIFICACIÓN == "Socialización")
sheet_RelLab <- res %>% filter(CLASIFICACIÓN == "Relación Laboral")

cat("Filas por categoría:\n")
print(tibble(
  categoria = c("Deber","Ocio y Recreación","Socialización","Relación Laboral"),
  filas = c(nrow(sheet_Deber), nrow(sheet_Ocio), nrow(sheet_Social), nrow(sheet_RelLab))
))

# ---------- 11) Guardar Excel con 4 hojas ----------
out_file <- "TP3_Emociones_Ponderadas_por_Persona.xlsx"
write_xlsx( 
  x = list(
    "Deber" = sheet_Deber,
    "Ocio_y_Recreacion" = sheet_Ocio,
    "Socializacion" = sheet_Social,
    "Relacion_Laboral" = sheet_RelLab
  ),
  path = out_file
)

cat("✅ Archivo generado:", out_file, "\n")

# Fin del script
