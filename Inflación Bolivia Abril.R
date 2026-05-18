# ==============================================================================
# GRÁFICO DE INFLACIÓN GENERAL - BOLIVIA
# Series: Mensual (línea) | A 12 meses | Acumulada continua (3 tramos coloreados)
# + Fondo verde claro desde nov-2025
# ==============================================================================

# install.packages(c("readxl","dplyr","tidyr","plotly","lubridate","htmlwidgets"))
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(lubridate)
library(htmlwidgets)

# ==============================================================================
# 1. CONFIGURACIÓN
# ==============================================================================

RUTA_IPC            <- "D:/Usuario/Desktop/Inflación/IPC producto.xlsx"
FILA_INDICE_GENERAL <- 1
FECHA_ASUNCION      <- as.Date("2025-11-01")

# ==============================================================================
# 2. PARSER DE FECHAS
# ==============================================================================

parsear_periodo <- function(x) {
  meses_es <- c(
    "ene"="01","feb"="02","mar"="03","abr"="04",
    "may"="05","jun"="06","jul"="07","ago"="08",
    "sep"="09","sept"="09","oct"="10","nov"="11","dic"="12"
  )
  partes   <- strsplit(tolower(trimws(x)), "-")[[1]]
  mes_num  <- meses_es[partes[1]]
  if (is.na(mes_num)) stop(paste("Mes no reconocido:", partes[1]))
  anio_num <- as.integer(partes[2])
  if (anio_num < 100) anio_num <- 2000 + anio_num
  as.Date(paste(anio_num, mes_num, "01", sep = "-"))
}

# ==============================================================================
# 3. CARGA DE DATOS
# ==============================================================================

ipc_raw    <- read_excel(RUTA_IPC)
cols_fecha <- setdiff(names(ipc_raw), c("CODIGO", "DESCRIPCION"))

fechas_convertidas <- setNames(
  sapply(cols_fecha, parsear_periodo),
  cols_fecha
)

ig_largo <- ipc_raw[FILA_INDICE_GENERAL, ] %>%
  select(CODIGO, DESCRIPCION, all_of(cols_fecha)) %>%
  pivot_longer(cols = all_of(cols_fecha), names_to = "periodo", values_to = "ipc") %>%
  mutate(
    ipc     = as.numeric(ipc),
    periodo = as.Date(fechas_convertidas[periodo])
  ) %>%
  arrange(periodo)

# ==============================================================================
# 4. CÁLCULO DE INDICADORES
# ==============================================================================

# Dic de cada año (base acumulada anual)
dic_por_anio <- ig_largo %>%
  filter(month(periodo) == 12) %>%
  mutate(anio = year(periodo)) %>%
  select(anio, ipc_dic = ipc)

ipc_dic2024 <- dic_por_anio %>% filter(anio == 2024) %>% pull(ipc_dic)
ipc_oct2025 <- ig_largo %>% filter(periodo == as.Date("2025-10-01")) %>% pull(ipc)

if (length(ipc_dic2024) == 0) stop("No se encontró IPC dic-2024.")
if (length(ipc_oct2025) == 0) stop("No se encontró IPC oct-2025.")

ig_ind <- ig_largo %>%
  arrange(periodo) %>%
  mutate(anio = year(periodo), mes = month(periodo)) %>%
  left_join(
    dic_por_anio %>% mutate(anio = anio + 1) %>% rename(ipc_dic_ant = ipc_dic),
    by = "anio"
  ) %>%
  mutate(
    ipc_dic_ant = ifelse(is.na(ipc_dic_ant), ipc, ipc_dic_ant),
    
    # Mensual
    inf_mensual = (ipc / lag(ipc, 1) - 1) * 100,
    
    # 12 meses
    inf_12m = (ipc / lag(ipc, 12) - 1) * 100,
    
    # Acumulada estándar (enero - mes actual)
    inf_acum = (ipc / ipc_dic_ant - 1) * 100
  )

#Extraer último dato
ultimo      <- ig_ind %>% filter(!is.na(inf_12m)) %>% tail(1)
primer_dato <- min(ig_ind$periodo)
ultimo_dato <- max(ig_ind$periodo)

# ==============================================================================
# 5. GRÁFICO
# ==============================================================================

fig <- plot_ly()

# Fondo verde claro: gestión nueva (nov-2025 en adelante)
fig <- fig %>%
  layout(
    shapes = list(
      list(
        type      = "rect",
        xref      = "x", yref = "paper",
        x0        = as.numeric(FECHA_ASUNCION) * 86400000,
        x1        = as.numeric(ultimo_dato + 30) * 86400000,
        y0        = 0, y1 = 1,
        fillcolor = "rgba(200, 240, 200, 0.6)",
        line      = list(width = 0),
        layer     = "below"
      )
    )
  )

# ---- Inflación Acumulada  ----
fig <- fig %>%
  add_trace(
    data          = ig_ind %>% filter(!is.na(inf_acum)),
    x             = ~periodo, y = ~inf_acum,
    type          = "scatter", mode = "lines",
    line          = list(color = "#4E79A7", width = 2.5, dash = "dot"),
    name          = "Acumulada (enero–mes actual)",
    hovertemplate = "Acumulada: <b>%{y:.2f}%</b><br>%{x|%b %Y}<extra></extra>"
  )

# ---- Inflación a 12 meses ----
fig <- fig %>%
  add_trace(
    data          = ig_ind %>% filter(!is.na(inf_12m)),
    x             = ~periodo, y = ~inf_12m,
    type          = "scatter", mode = "lines",
    line          = list(color = "#1F3A5F", width = 2.5),
    name          = "A 12 meses",
    hovertemplate = "12 meses: <b>%{y:.2f}%</b><br>%{x|%b %Y}<extra></extra>"
  )

# ---- Inflación mensual ----
fig <- fig %>%
  add_trace(
    data          = ig_ind %>% filter(!is.na(inf_mensual)),
    x             = ~periodo, y = ~inf_mensual,
    type          = "scatter", mode = "lines",
    line          = list(color = "#D6B8B8", width = 2),
    name          = "Mensual",
    hovertemplate = "Mensual: <b>%{y:.2f}%</b><br>%{x|%b %Y}<extra></extra>"
  )

# ==============================================================================
# 6. LAYOUT
# ==============================================================================

fig <- fig %>%
  layout(
    title = list(
      text = "<b>Tasa de variación del Indice de Precios al Consumidor (IPC) - Inflación</b>",
      font = list(size = 20, color = "#1a1a2e"),
      x    = 0.03
    ),
    
    xaxis = list(
      title      = "",
      showgrid   = TRUE,
      gridcolor  = "#EEEEEE",
      tickformat = "%b %Y",
      
      rangeselector = list(
        buttons = list(
          list(count = 1, label = "1A", step = "year", stepmode = "backward"),
          list(count = 3, label = "3A", step = "year", stepmode = "backward"),
          list(count = 5, label = "5A", step = "year", stepmode = "backward"),
          list(step = "all", label = "Todo")
        )
      ),
      
      rangeslider = list(visible = FALSE) #Para quitar la barra
    ),
    
    yaxis = list(
      title         = "Variación (%)",
      range         = c(-5, 25),
      showgrid      = TRUE,
      gridcolor     = "#EEEEEE",
      ticksuffix    = "%",
      zeroline      = TRUE,
      zerolinecolor = "#AAAAAA",
      zerolinewidth = 1.2
    ),
    
    legend = list(
      orientation = "h",
      x = 0, y = -0.18,
      bgcolor     = "rgba(255,255,255,0.9)",
      bordercolor = "#DDDDDD",
      borderwidth = 1,
      font        = list(size = 11)
    ),
    
    plot_bgcolor  = "white",
    paper_bgcolor = "white",
    hovermode     = "x unified",
    
    annotations = list(
      list(
        text = "Fuente: Instituto Nacional de Estadística (INE) - Bolivia",
        x = 0, y = -0.27, xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 10, color = "#888888")
      ),
      list(
        text = paste0(
          "Último dato: ", format(ultimo$periodo, "%b %Y"),
          " | 12m: <b>", round(ultimo$inf_12m, 2), "%</b>"
        ),
        x = 1, y = -0.27, xref = "paper", yref = "paper",
        showarrow = FALSE, xanchor = "right",
        font = list(size = 10, color = "#444444")
      )
    ),
    
    margin = list(t = 80, r = 50, b = 110, l = 60)
  )

# ==============================================================================
# 7. GUARDAR
# ==============================================================================

saveWidget(fig, "Grafico_inflacion_Bolivia.html", selfcontained = TRUE)
message("✓ Guardado: Grafico_inflacion_Bolivia.html")