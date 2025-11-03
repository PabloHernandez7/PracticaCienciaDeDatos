library(readr)
library(dplyr)
library(ggplot2)

personas <- read_csv("Base de datos Personas Exactas.csv",
                     locale = locale(encoding = "UTF-8"))

glimpse(personas)

prom_persona_actividad <- read_excel("resultados_viajes.xlsx", sheet = "Promedio por persona y activi")

#Uno las dos bases para que tenga todos los datos
base_completa <- prom_persona_actividad %>%
  left_join(personas, by = "Identificacion")

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
age_info <- parse_edad(base_completa[[edad_col]])

base_completa <- bind_cols(base_completa, age_info) %>%
  mutate(
    # ordenar el factor de menor a mayor usando el límite inferior / midpoint
    edad_rango = factor(
      edad_rango,
      levels = unique(edad_rango[order(edad_lo, edad_mid, na.last = TRUE)]),
      ordered = TRUE
    )
  )

#COLUMNAS SIN TILDE

base_completa <- base_completa %>%
  mutate(Genero = if ("Género" %in% names(.)) as.factor(`Género`) else as.factor(Genero),
         Ocupacion = if ("Ocupación" %in% names(.)) as.factor(`Ocupación`) else as.factor(Ocupacion))


# ====== Base para graficar (quita NAs de la métrica) ======
bc <- base_completa %>%
  filter(!is.na(tiempo_viaje_prom_min))

# ========== 1) Barras por RANGO ETARIO (promedio general) ==========
bc <- base_completa %>%
  filter(!is.na(tiempo_viaje_prom_min),
         !is.na(edad_rango),
         !is.na(Ocupacion))

edad_res <- bc %>%
  group_by(edad_rango) %>%
  summarise(
    n = n(),
    prom_min = mean(tiempo_viaje_prom_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # ordenar por promedio (de mayor a menor)
  arrange(desc(prom_min))

ggplot(edad_res, aes(x = edad_rango, y = prom_min, fill = prom_min)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradient(low = "#90CAF9", high = "#1565C0") +
  labs(title = "Tiempo de viaje promedio por rango etario",
       x = "Rango etario", y = "Promedio (min)") +
  theme_minimal(base_size = 12)

# ========== 2) Barras por GÉNERO para los 6 MOTIVOS más frecuentes ==========
bc <- base_completa %>%
  filter(!is.na(tiempo_viaje_prom_min),
         !is.na(edad_rango),
         !is.na(Ocupacion))

top_motivos <- bc %>%
  count(`Motivo del Viaje`, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(`Motivo del Viaje`)

genero_res <- bc %>%
  filter(`Motivo del Viaje` %in% top_motivos, !is.na(Genero)) %>%
  group_by(Genero, `Motivo del Viaje`) %>%
  summarise(
    n = n(),
    prom_min = mean(tiempo_viaje_prom_min, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(genero_res,
       aes(x = reorder(`Motivo del Viaje`, prom_min, FUN = max), y = prom_min, fill = Genero)) +
  geom_col(position = position_dodge(width = 0.8)) +
  coord_flip() +
  labs(title = "Tiempo de viaje promedio por género (top 6 motivos)",
       x = "Motivo del viaje", y = "Promedio (min)", fill = "Género") +
  theme_minimal(base_size = 12)

# ========== 3) Barras por OCUPACIÓN (top-10 por cantidad) ==========
bc <- base_completa %>%
  filter(!is.na(tiempo_viaje_prom_min),
         !is.na(edad_rango),
         !is.na(Ocupacion))

top_ocup <- bc %>%
  count(Ocupacion, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(Ocupacion)

ocup_res <- bc %>%
  filter(Ocupacion %in% top_ocup) %>%
  group_by(Ocupacion) %>%
  summarise(
    n = n(),
    prom_min = mean(tiempo_viaje_prom_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(prom_min)


ggplot(ocup_res, aes(x = reorder(Ocupacion, prom_min), y = prom_min, fill = Ocupacion)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Tiempo de viaje promedio por ocupación (top 10)",
       x = "Ocupación", y = "Promedio (min)") +
  theme_minimal(base_size = 12)

