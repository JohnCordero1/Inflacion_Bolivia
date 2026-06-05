# ==============================================================================
# GRÁFICO DE INFLACIÓN GENERAL - BOLIVIA
# Versión con template HTML separado (evita errores de locale y límite de chars)
# ==============================================================================
#
# ESTRUCTURA DE ARCHIVOS necesaria en la misma carpeta que este script:
#
#   Inflacion Bolivia Abril.R   <- este script
#   template_inflacion.html     <- el template HTML (sin datos)
#
# El script lee el template, inyecta los datos y guarda el resultado en
# RUTA_SALIDA.
# ==============================================================================

# install.packages(c("readxl","dplyr","lubridate","jsonlite"))
library(readxl)
library(dplyr)
library(lubridate)
library(jsonlite)

# ==============================================================================
# 1. CONFIGURACIÓN — ajustá estas rutas a tu equipo
# ==============================================================================
RUTA_IPC      <- "D:/Usuario/Desktop/Inflación/IPC Bolivia.xlsx"
RUTA_TEMPLATE <- "D:/Usuario/Desktop/Inflación/Inflacion general/template_inflacion.html"
RUTA_SALIDA   <- "D:/Usuario/Desktop/Inflación/Inflacion general/index.html"
FECHA_ASUNCION <- as.Date("2025-11-01")

# ==============================================================================
# 2. CARGA DE DATOS
# ==============================================================================
ipc_raw <- read_excel(RUTA_IPC)

ig_largo <- ipc_raw %>%
  rename_with(tolower) %>%
  rename(periodo = fecha, ipc = ipc) %>%
  mutate(
    periodo = as.Date(periodo),
    ipc     = as.numeric(ipc)
  ) %>%
  arrange(periodo)

# ==============================================================================
# 3. CÁLCULO DE INDICADORES
# ==============================================================================
dic_por_anio <- ig_largo %>%
  filter(month(periodo) == 12) %>%
  mutate(anio = year(periodo)) %>%
  select(anio, ipc_dic = ipc)

ig_ind <- ig_largo %>%
  arrange(periodo) %>%
  mutate(anio = year(periodo), mes = month(periodo)) %>%
  left_join(
    dic_por_anio %>% mutate(anio = anio + 1) %>% rename(ipc_dic_ant = ipc_dic),
    by = "anio"
  ) %>%
  mutate(
    ipc_dic_ant = ifelse(is.na(ipc_dic_ant), ipc, ipc_dic_ant),
    inf_mensual = (ipc / lag(ipc, 1)  - 1) * 100,
    inf_12m     = (ipc / lag(ipc, 12) - 1) * 100,
    inf_acum    = (ipc / ipc_dic_ant  - 1) * 100
  )

ultimo      <- ig_ind %>% filter(!is.na(inf_12m)) %>% tail(1)
ultimo_dato <- max(ig_ind$periodo)
primer_dato <- min(ig_ind$periodo)

# ==============================================================================
# 4. SERIALIZAR A JSON
# ==============================================================================
datos_json <- ig_ind %>%
  mutate(
    fecha     = format(periodo, "%Y-%m-%d"),
    ipc       = round(ipc, 4),
    mensual   = round(inf_mensual, 4),
    doce_m    = round(inf_12m, 4),
    acumulada = round(inf_acum, 4)
  ) %>%
  select(fecha, ipc, mensual, doce_m, acumulada) %>%
  toJSON(na = "null", dataframe = "rows")

# ==============================================================================
# 5. PREPARAR VALORES PARA LOS PLACEHOLDERS
# ==============================================================================
meta_primer_anio    <- as.character(year(primer_dato))
meta_ultimo_anio    <- as.character(year(ultimo_dato))
meta_ultimo_fecha   <- format(ultimo$periodo, "%b %Y")
meta_ultimo_12m     <- as.character(round(ultimo$inf_12m, 2))
meta_fecha_asuncion <- format(FECHA_ASUNCION, "%Y-%m-%d")

# ==============================================================================
# 6. LEER TEMPLATE E INYECTAR DATOS
# ==============================================================================
# Leer el template con la codificación correcta
template <- readLines(RUTA_TEMPLATE, encoding = "UTF-8", warn = FALSE)
html      <- paste(template, collapse = "\n")

# Reemplazar cada placeholder por su valor real
html <- gsub("{{DATOS_JSON}}",    datos_json,          html, fixed = TRUE)
html <- gsub("{{PRIMER_ANIO}}",   meta_primer_anio,    html, fixed = TRUE)
html <- gsub("{{ULTIMO_ANIO}}",   meta_ultimo_anio,    html, fixed = TRUE)
html <- gsub("{{ULTIMO_FECHA}}",  meta_ultimo_fecha,   html, fixed = TRUE)
html <- gsub("{{ULTIMO_12M}}",    meta_ultimo_12m,     html, fixed = TRUE)
html <- gsub("{{FECHA_ASUNCION}}", meta_fecha_asuncion, html, fixed = TRUE)

# ==============================================================================
# 7. GUARDAR RESULTADO FINAL
# ==============================================================================
writeLines(html, RUTA_SALIDA, useBytes = FALSE)
message("Listo. Guardado en: ", RUTA_SALIDA)
