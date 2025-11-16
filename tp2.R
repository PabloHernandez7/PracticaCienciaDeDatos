library(readr)
library(dplyr)
library(ggplot2)

personas <- read_csv("Base de datos Personas Exactas.csv",
                     locale = locale(encoding = "UTF-8"))

glimpse(personas)

prom_persona_actividad <- read_excel("resultados_viajes.xlsx", sheet = "Promedio por persona y activi")

#Uno las dos bases para que tenga todos los datos
base_completa <- prom_persona_actividad %>%
  left_join(personas, by = c("Identificación" = "Identificacion"))

# Detecto la columna de edad por nombre (soporta "Edad", "Rango de edad", etc.)
edad_col <- names(base_completa)[grepl("edad", names(base_completa), ignore.case = TRUE)][1]
stopifnot(!is.na(edad_col))

# ---- Parser robusto de rangos de edad (solo base R) ----
parse_edad <- function(x) {
  s <- tolower(trimws(as.character(x)))
  s <- sub("^[a-z]\\.\\s*", "", s)                   # quita prefijos tipo "C. "
  s <- gsub("\\s+años?", "", s)                      # quita "años"
  nums_list <- regmatches(s, gregexpr("\\d+", s))
  get_num <- function(v, k) if (length(v) >= k) suppressWarnings(as.numeric(v[k])) else NA_real_
  
  lo <- mapply(function(v, txt) {
    v <- regmatches(txt, gregexpr("\\d+", txt))[[1]]
    if (grepl("menor", txt) && length(v) >= 1) 0 else get_num(v, 1)
  }, nums_list, s)
  
  hi <- mapply(function(v, txt) {
    v <- regmatches(txt, gregexpr("\\d+", txt))[[1]]
    if (grepl("menor", txt) && length(v) >= 1) as.numeric(v[1]) - 1
    else if (grepl("(más|mas|\\+)$", txt)) NA_real_
    else get_num(v, 2)
  }, nums_list, s)
  
  label <- ifelse(!is.na(hi), paste0(lo, "-", hi),
                  ifelse(is.na(hi) & !is.na(lo), paste0(lo, "+"), NA))
  
  mid <- ifelse(!is.na(hi), (lo + hi)/2, lo)
  
  data.frame(edad_lo = lo, edad_hi = hi, edad_rango = label, edad_mid = mid)
}

# ---- Aplicación a tu base ----

base_actividades <- prom_persona_actividad %>%
  left_join(personas, by = c("Identificación" = "Identificacion"))

edad_col <- names(base_actividades)[
  grepl("edad", names(base_actividades), ignore.case = TRUE)
][1]

edad_col   # debería mostrar el nombre de la columna de edad en el CSV

age_info <- parse_edad(base_actividades[[edad_col]])

base_actividades <- base_actividades %>%
  bind_cols(age_info) %>%
  mutate(
    edad_rango = factor(
      edad_rango,
      levels = unique(edad_rango[order(edad_lo, edad_mid, na.last = TRUE)]),
      ordered = TRUE
    ),
    Genero = case_when(
      `Género` %in% c("Masculino","M","m","masculino") ~ "Masculino",
      `Género` %in% c("Femenino","F","f","femenino")   ~ "Femenino",
      TRUE ~ as.character(`Género`)
    ),
    Genero    = factor(Genero),
    Ocupacion = as.factor(`Ocupación`)
  )

glimpse(base_actividades)


# 2) Actividades por persona
activ_por_persona <- base_actividades %>%
  filter(
    !is.na(`Motivo del Viaje`),
    !grepl("volver a casa", `Motivo del Viaje`, ignore.case = TRUE)
  ) %>%
  group_by(Identificación) %>%
  summarise(
    # cuántas veces realiza actividades (usando n_obs del TP1)
    total_actividades = sum(n_obs, na.rm = TRUE),
    edad_rango = first(edad_rango),
    Genero     = first(Genero),
    Ocupacion  = first(Ocupacion),
    .groups = "drop"
  )

glimpse(activ_por_persona)

edad_res <- activ_por_persona %>%
  filter(!is.na(edad_rango)) %>%
  group_by(edad_rango) %>%
  summarise(
    n_personas = n(),
    prom_actividades = mean(total_actividades, na.rm = TRUE),
    .groups = "drop"
  )

p_edad <- ggplot(edad_res,
       aes(x = edad_rango, y = prom_actividades, fill = prom_actividades)) +
  geom_col(show.legend = FALSE) +
  # sin coord_flip()
  scale_fill_gradient(low = "#90CAF9", high = "#1565C0") +
  labs(
    title = "Promedio de actividades por rango etario",
    x = "Rango etario",
    y = "Promedio de actividades por persona"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # inclina labels del eje X
  )


genero_res <- activ_por_persona %>%
  filter(!is.na(Genero)) %>%
  group_by(Genero) %>%
  summarise(
    n_personas = n(),
    prom_actividades = mean(total_actividades, na.rm = TRUE),
    .groups = "drop"
  )

p_genero <- ggplot(genero_res,
       aes(x = Genero, y = prom_actividades, fill = Genero)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Promedio de actividades por género",
    x = "Género",
    y = "Promedio de actividades por persona"
  ) +
  theme_minimal(base_size = 12)


top_ocup <- activ_por_persona %>%
  filter(!is.na(Ocupacion)) %>%
  count(Ocupacion, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(Ocupacion)

ocup_res <- activ_por_persona %>%
  filter(!is.na(Ocupacion)) %>%
  group_by(Ocupacion) %>%
  summarise(
    n_personas = n(),
    prom_actividades = mean(total_actividades, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(prom_actividades)

p_ocup <- ggplot(ocup_res,
       aes(x = reorder(Ocupacion, prom_actividades),
           y = prom_actividades)) +
  geom_segment(aes(xend = Ocupacion, y = 0, yend = prom_actividades)) +
  geom_point(size = 3) +
  coord_flip() +
  labs(
    title = "Promedio de actividades por ocupación",
    x = "Ocupación",
    y = "Promedio de actividades por persona"
  ) +
  theme_minimal(base_size = 12)

edad_tabla <- activ_por_persona %>%
  filter(!is.na(edad_rango)) %>%
  group_by(edad_rango) %>%
  summarise(
    n_personas = n(),
    prom_actividades = mean(total_actividades, na.rm = TRUE),
    prom_actividades_red = round(prom_actividades, 2),
    .groups = "drop"
  ) %>%
  arrange(edad_rango)

edad_tabla
# o:
View(edad_tabla)


genero_tabla <- activ_por_persona %>%
  filter(!is.na(Genero)) %>%
  group_by(Genero) %>%
  summarise(
    n_personas = n(),
    prom_actividades = mean(total_actividades, na.rm = TRUE),
    prom_actividades_red = round(prom_actividades, 2),
    .groups = "drop"
  )

genero_tabla
# o:
View(genero_tabla)


ocup_tabla <- activ_por_persona %>%
  filter(!is.na(Ocupacion)) %>%
  group_by(Ocupacion) %>%
  summarise(
    n_personas = n(),
    prom_actividades = mean(total_actividades, na.rm = TRUE),
    prom_actividades_red = round(prom_actividades, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(prom_actividades))

ocup_tabla
# o:
View(ocup_tabla)

library(writexl)

write_xlsx(
  list(
    "Promedio_edad"   = edad_tabla,
    "Promedio_genero" = genero_tabla,
    "Promedio_ocup"   = ocup_tabla
  ),
  path = "resumen_promedios_actividades.xlsx"
)

ggsave("Edad.png", plot = p_edad, width = 7, height = 5, dpi = 300)
ggsave("Genero.png", plot = p_genero, width = 7, height = 5, dpi = 300)
ggsave("Ocupacion.png", plot = p_ocup, width = 7, height = 5, dpi = 300)

getwd()


