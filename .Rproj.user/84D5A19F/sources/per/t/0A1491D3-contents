# ============================
# Leer archivo Excel (.xlsx)
# ============================

# Instalar librería si no está
# install.packages("readxl")

library(readxl)
library(dplyr)
library(writexl)

# Ruta del archivo Excel
datos_original <- read_excel("Viajes origen destino optativa Exactas.xlsx", sheet = "Base de datos para el práctico")

# --- Calcular tiempo de actividad usando solo dplyr ---
datos_con_actividad <- datos_original %>%
  mutate(
    # Convertir texto tipo "7:45:00 a.m." -> tiempo POSIXct
    HoraIni = as.POSIXct(HoraInicio, format = "%I:%M:%S %p"),
    HoraFin = as.POSIXct(HoraFin, format = "%I:%M:%S %p")
  ) %>%
  group_by(Identificacion) %>%
  mutate(
    HoraFin_anterior = lag(HoraFin),
    t_actividad_min = as.numeric(difftime(HoraIni, HoraFin_anterior, units = "mins")),
    t_actividad_min = ifelse(JOURNEY == "X" | is.na(HoraFin_anterior) | t_actividad_min < 0,
                             NA, t_actividad_min)
  ) %>%
  ungroup()

# 2) A partir de datos_con_actividad: definimos blocks de journey y flags + t_actividad_fila
base_para_ttr <- datos_con_actividad %>%
  mutate(JOURNEY = trimws(as.character(JOURNEY))) %>%
  group_by(Identificacion) %>%
  # jblock: cada vez que aparece un valor (no-NA) en JOURNEY arranca un bloque nuevo (emula celdas combinadas)
  mutate(jblock = cumsum(!is.na(JOURNEY))) %>%
  group_by(Identificacion, jblock) %>%
  mutate(
    # invalida TODO el bloque si alguna fila tiene X/x
    hay_x = any(JOURNEY %in% c("X","x"), na.rm = TRUE),
    # flag final de validez según tus reglas
    valido = !hay_x,
    # actividad que corresponde a ESTA fila: entre su fin y el inicio del siguiente viaje
    t_actividad_fila = lead(t_actividad_min),
    # si esta fila es inválida, no calculamos actividad
    t_actividad_fila = ifelse(valido, t_actividad_fila, NA_real_)
  ) %>%
  ungroup()

# Última fila de cada persona suele quedar NA (no hay viaje siguiente)
base_para_ttr %>%
  group_by(Identificacion) %>% slice_tail(n = 1) %>% ungroup() %>%
  summarise(total = n(), ultimas_con_NA = sum(is.na(t_actividad_fila)))


#datos_test <- datos_con_actividad %>%
#  mutate(diff_recalc = as.numeric(difftime(HoraInicio, HoraFin_anterior, units = "mins")))

#datos_test %>%
#  group_by(`Identificacion`) %>% slice_head(n = 1) %>% ungroup() %>%
#  summarise(total = n(), primeras_con_NA = sum(is.na(t_actividad_min)))
# primeras_con_NA debe == total

#datos_test %>%
 # filter(!is.na(t_actividad_min), t_actividad_min < 0) %>% nrow()

#datos_test %>%
#  filter(JOURNEY == "X") %>%
#  summarise(filas_X = n(), X_con_NA = sum(is.na(t_actividad_min)))


resultados <- base_para_ttr %>%
  mutate(
    TV     = as.numeric(TV),
    tv_min = TV * 1440,
    TTR    = ifelse(valido & !is.na(tv_min) & !is.na(t_actividad_fila) & tv_min >= 0,
                    tv_min / (tv_min + t_actividad_fila), NA_real_)
  )

prom_persona_actividad <- resultados %>%
  filter(valido, !is.na(tv_min)) %>%
  group_by(Identificacion, `Motivo del Viaje`) %>%
  summarise(
    n_obs = n(),
    tiempo_viaje_prom_min = mean(tv_min, na.rm = TRUE),
    TTR_prom = mean(TTR, na.rm = TRUE),
    .groups = "drop"
  )

prom_global_actividad <- resultados %>%
  filter(valido, !is.na(tv_min)) %>%
  group_by(`Motivo del Viaje`) %>%
  summarise(
    n_obs = n(),
    tiempo_viaje_prom_min = mean(tv_min, na.rm = TRUE),
    TTR_prom = mean(TTR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(`Motivo del Viaje`)



prom_persona_actividad %>% arrange(Identificacion, `Motivo del Viaje`) %>% head(15)
prom_global_actividad

prom_global_actividad <- prom_global_actividad %>%
  mutate(TTR_prom = ifelse(is.nan(TTR_prom), NA, TTR_prom))

write_xlsx(
  list(
    "Promedio por persona y actividad" = prom_persona_actividad,
    "Promedio global por actividad"    = prom_global_actividad
  ),
  path = "resultados_viajes.xlsx"
)