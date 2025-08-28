# install.packages(c(
#   "shiny","shinyWidgets","bslib","sf","readr","dplyr","stringr",
#   "leaflet","plotly","lubridate","forcats","scales","ggplot2","markdown"
# ))

library(shiny)
library(shinyWidgets)
library(bslib)
library(sf)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(forcats)
library(leaflet)
library(plotly)
library(scales)
library(ggplot2)

# -------- Helper formato --------
fmt_money <- function(x, acc = 1) {
  scales::number(x, big.mark = ".", decimal.mark = ",", accuracy = acc)
}

# ------------------------
# 1) CARGA DE DATOS
# ------------------------
csv_path <- file.path("data", "datos.csv")
shp_path <- file.path("data", "COLOMBIA.shp")

if (!file.exists(csv_path)) stop("Error: No se encuentra el archivo ", csv_path, ".")
if (!file.exists(shp_path)) stop("Error: No se encuentra el archivo ", shp_path, ".")

tryCatch({
  datos <- suppressMessages(read_csv(csv_path, show_col_types = FALSE)) %>%
    mutate(
      FECHA_PRECIO     = suppressWarnings(as.Date(FECHA_PRECIO)),
      DEPARTAMENTO_EDS = str_squish(str_to_upper(DEPARTAMENTO_EDS)),
      MUNICIPIO_EDS    = str_squish(str_to_upper(MUNICIPIO_EDS))
    )
  if (nrow(datos) == 0) stop("El archivo de datos está vacío")
  required_cols <- c("FECHA_PRECIO","DEPARTAMENTO_EDS","MUNICIPIO_EDS","PRECIO_PROMEDIO_PUBLICADO")
  miss <- setdiff(required_cols, names(datos))
  if (length(miss) > 0) stop("Faltan columnas en el CSV: ", paste(miss, collapse = ", "))
}, error = function(e) stop("Error al cargar el CSV: ", e$message))

tryCatch({
  col_shp_raw <- suppressWarnings(st_read(shp_path, quiet = TRUE))
  col_shp     <- st_transform(col_shp_raw, 4326)
}, error = function(e) stop("Error al cargar el shapefile: ", e$message))

# Detecta columna de departamento en el SHP
cand_cols_depto_shp <- c("DPTO_CNMBRE","DPTO_CNMBR","NOMBRE_DPT","DPTO","DEPARTAMEN","NOM_DEPTO","NOMBRE_DPT")
col_depto_shp <- (cand_cols_depto_shp[cand_cols_depto_shp %in% names(col_shp)])[1]
if (is.na(col_depto_shp)) {
  char_cols <- names(col_shp)[sapply(col_shp, function(x) is.character(x) && !inherits(x, "sfc"))]
  if (length(char_cols) > 0) col_depto_shp <- char_cols[1] else stop("No se identificó columna de departamento en el SHP")
}
col_shp <- col_shp %>%
  mutate(depto_shp = str_squish(str_to_upper(.data[[col_depto_shp]])))

# Rango de fechas y precio para filtros
fecha_min <- min(datos$FECHA_PRECIO, na.rm = TRUE)
fecha_max <- max(datos$FECHA_PRECIO, na.rm = TRUE)
precio_min <- floor(min(datos$PRECIO_PROMEDIO_PUBLICADO, na.rm = TRUE))
precio_max <- ceiling(max(datos$PRECIO_PROMEDIO_PUBLICADO, na.rm = TRUE))

# ------------------------
# 2) UI
# ------------------------
theme_app <- bs_theme(
  version = 5,
  bootswatch = "cosmo",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins")
)

ui <- navbarPage(
  title = "GNCV Colombia",
  theme = theme_app,
  header = tags$head(tags$style(HTML("
    .navbar { box-shadow: 0 4px 14px rgba(0,0,0,.06); }
    .card {
      background: var(--bs-body-bg);
      border: 1px solid var(--bs-border-color);
      border-radius: 16px;
      padding: 18px 18px;
      box-shadow: 0 6px 18px rgba(2,9,31,.06);
      margin-bottom: 16px;
    }
    .bootstrap-select .dropdown-toggle { border-radius: 12px; padding: 10px 14px; }
    .bootstrap-select .bs-searchbox input { border-radius: 10px; }
  "))),
  
  tabPanel("Inicio",
           fluidPage(
             br(),
             div(class="card",
                 fluidRow(
                   column(3, uiOutput("logo_ui")),
                   column(
                     9,
                     h2("Contextualización del Gas Natural Comprimido Vehicular (GNCV) en Colombia"),
                     if (file.exists("contexto.md") && requireNamespace("markdown", quietly = TRUE)) {
                       includeMarkdown("contexto.md")
                     } else {
                       p("Agrega un archivo 'contexto.md' con la descripción del análisis y la georreferenciación.")
                     }
                   )
                 )
             )
           )
  ),
  
  tabPanel("Mapa & Gráficos",
           fluidPage(
             br(),
             fluidRow(
               column(4,
                      div(class="card",
                          h4("Filtros"),
                          airDatepickerInput(
                            inputId = "fecha_rango", label = "Rango de fechas",
                            value = c(fecha_min, fecha_max),
                            minDate = fecha_min, maxDate = fecha_max,
                            clearButton = TRUE, autoClose = TRUE, range = TRUE
                          ),
                          sliderInput(
                            inputId = "precio_rango", label = "Rango de precio promedio publicado (COP)",
                            min = precio_min, max = precio_max,
                            value = c(precio_min, precio_max), step = 1, width = "100%"
                          ),
                          div(
                            class="d-flex gap-2",
                            actionButton("aplicar", "Aplicar filtros", class="btn btn-primary"),
                            actionButton("clear",   "Limpiar", class="btn btn-outline-secondary")
                          ),
                          hr(),
                          sliderInput("topN", "Top N departamentos por precio",
                                      min = 5, max = min(32, length(unique(datos$DEPARTAMENTO_EDS))),
                                      value = min(15, length(unique(datos$DEPARTAMENTO_EDS))), step = 1, width = "100%"
                          )
                      )
               ),
               column(8,
                      div(class="card",
                          h4("Mapa interactivo"),
                          leafletOutput("mapa", height = 560)
                      ),
                      div(class="card",
                          h4("Serie temporal (promedio nacional / filtro)"),
                          plotlyOutput("serie", height = 320)
                      )
               )
             ),
             br(),
             div(class="card",
                 h4("Top por precio promedio"),
                 plotlyOutput("plot_top", height = 360)
             )
           )
  ),
  
  tabPanel("Conclusiones",
           fluidPage(
             br(),
             div(class="card",
                 h3("Hallazgos y recomendaciones"),
                 if (file.exists("conclusiones.md") && requireNamespace("markdown", quietly = TRUE)) {
                   includeMarkdown("conclusiones.md")
                 } else {
                   p("Agrega un archivo 'conclusiones.md' para mostrar aquí tus hallazgos.")
                 }
             )
           )
  )
)

# ------------------------
# 3) SERVER
# ------------------------
server <- function(input, output, session){
  
  # --- Logo con manejo mejorado ---
  output$logo_ui <- renderUI({
    # Verificar múltiples ubicaciones posibles del logo
    logo_paths <- c("www/logo.png", "logo.png", "www:/logo.png")
    logo_exists <- any(file.exists(logo_paths))
    logo_path <- logo_paths[file.exists(logo_paths)][1]
    
    if (logo_exists && !is.na(logo_path)) {
      # Si encontramos el logo, asegurarse de que esté en www/
      if (logo_path != "www/logo.png") {
        # Crear www si no existe
        if (!dir.exists("www")) {
          dir.create("www", showWarnings = FALSE)
        }
        # Copiar logo a la ubicación correcta
        if (file.copy(logo_path, "www/logo.png", overwrite = TRUE)) {
          message("Logo copiado a www/logo.png")
        }
      }
      
      # Mostrar el logo
      tags$img(
        src = "logo.png",  # Ruta relativa desde www/
        style = "max-width:100%; height:auto; border-radius:16px; object-fit: contain;",
        alt = "Logo GNCV"
      )
    } else {
      # Mostrar placeholder con diagnóstico
      tagList(
        div(
          style="height:120px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius:16px; display:flex; align-items:center; justify-content:center; color: white; text-align: center;",
          div(
            h4("GNCV", style="color:white; margin:0; font-weight:bold; font-size: 2em;"),
            p("Colombia", style="color:rgba(255,255,255,0.8); margin:5px 0 0 0; font-size: 1.1em;")
          )
        ),
        br(),
        details(
          summary("Diagnóstico del logo (clic para expandir)"),
          div(
            class = "alert alert-info",
            style = "font-size: 11px; margin-top: 10px;",
            HTML(paste0(
              "<strong>Información de diagnóstico:</strong><br>",
              "• Directorio actual: <code>", getwd(), "</code><br>",
              "• ¿Existe 'www/'? ", ifelse(dir.exists("www"), "✅ Sí", "❌ No"), "<br>",
              "• ¿Existe 'www:/? ", ifelse(dir.exists("www:"), "⚠️ Sí (nombre incorrecto)", "✅ No"), "<br>",
              "• ¿Existe 'www/logo.png'? ", ifelse(file.exists("www/logo.png"), "✅ Sí", "❌ No"), "<br>",
              "• ¿Existe 'www:/logo.png'? ", ifelse(file.exists("www:/logo.png"), "⚠️ Sí (carpeta incorrecta)", "❌ No"), "<br>",
              "• Archivos en directorio: <code>", paste(list.files(), collapse = ", "), "</code><br>",
              if (dir.exists("www")) {
                paste0("• Archivos en 'www/': <code>", paste(list.files("www"), collapse = ", "), "</code><br>")
              } else "",
              if (dir.exists("www:")) {
                paste0("• Archivos en 'www:/': <code>", paste(list.files("www:"), collapse = ", "), "</code>")
              } else ""
            ))
          )
        )
      )
    }
  })
  
  # ---- Limpieza de filtros ----
  observeEvent(input$clear, {
    updateAirDateInput(session, "fecha_rango", value = c(fecha_min, fecha_max))
    updateSliderInput(session, "precio_rango", value = c(precio_min, precio_max))
  })
  
  # ---- Datos filtrados (se recalcula al dar "Aplicar") ----
  filtered_data <- eventReactive(input$aplicar, {
    df <- datos
    
    # Fecha
    fr <- input$fecha_rango
    if (!is.null(fr) && length(fr) == 2) {
      df <- df %>% filter(FECHA_PRECIO >= fr[1], FECHA_PRECIO <= fr[2])
    }
    
    # Precio
    pr <- input$precio_rango
    if (!is.null(pr) && length(pr) == 2) {
      df <- df %>% filter(PRECIO_PROMEDIO_PUBLICADO >= pr[1], PRECIO_PROMEDIO_PUBLICADO <= pr[2])
    }
    
    df
  }, ignoreNULL = FALSE)
  
  # ---- Agregado por dpto ----
  datos_dpto <- reactive({
    df <- filtered_data()
    if (nrow(df) == 0) return(tibble(DEPARTAMENTO_EDS = character(0), PRECIO_PROMEDIO = numeric(0)))
    df %>%
      filter(!is.na(PRECIO_PROMEDIO_PUBLICADO)) %>%
      group_by(DEPARTAMENTO_EDS) %>%
      summarize(PRECIO_PROMEDIO = mean(PRECIO_PROMEDIO_PUBLICADO, na.rm = TRUE), .groups = "drop")
  })
  
  # ---- Join con shp ----
  shp_join <- reactive({
    datos_agg <- datos_dpto()
    if (nrow(datos_agg) == 0) {
      return(col_shp %>% select(geometry, depto_shp) %>% mutate(PRECIO_PROMEDIO = NA_real_))
    }
    left_join(
      col_shp %>% select(geometry, depto_shp),
      datos_agg %>% mutate(DEPARTAMENTO_EDS = str_squish(str_to_upper(DEPARTAMENTO_EDS))),
      by = c("depto_shp" = "DEPARTAMENTO_EDS")
    )
  })
  
  # ---- MAPA ----
  output$mapa <- renderLeaflet({
    shp <- shp_join()
    make_pal <- function(domain_vals) {
      if (requireNamespace("RColorBrewer", quietly = TRUE)) {
        colorNumeric("Blues", domain = domain_vals, na.color = "#f8f9fa")
      } else {
        colorNumeric(c("#f7fbff","#deebf7","#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c"),
                     domain = domain_vals, na.color = "#f8f9fa")
      }
    }
    
    if (all(is.na(shp$PRECIO_PROMEDIO))) {
      leaflet(shp) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = "#f8f9fa",
          color = "#333333", weight = 1, opacity = 1, fillOpacity = 0.3,
          label = ~lapply(sprintf("<b>%s</b><br>Sin datos disponibles", depto_shp), htmltools::HTML)
        )
    } else {
      pal <- make_pal(shp$PRECIO_PROMEDIO)
      leaflet(shp) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(PRECIO_PROMEDIO),
          color = "#333333", weight = 1, opacity = 1, fillOpacity = 0.8,
          label = ~lapply(
            sprintf("<b>%s</b><br>Precio promedio: <b>%s</b>",
                    depto_shp, ifelse(is.na(PRECIO_PROMEDIO), "—", fmt_money(PRECIO_PROMEDIO))),
            htmltools::HTML
          ),
          highlight = highlightOptions(weight = 2, color = "#000", bringToFront = TRUE)
        ) %>%
        addLegend(
          "bottomright", pal = pal, values = ~PRECIO_PROMEDIO,
          title = "Precio promedio (COP)", opacity = 0.8,
          labFormat = labelFormat(big.mark = ".", digits = 0)
        )
    }
  })
  
  # ---- TOP N ----
  output$plot_top <- renderPlotly({
    df <- datos_dpto() %>%
      filter(!is.na(PRECIO_PROMEDIO)) %>%
      arrange(desc(PRECIO_PROMEDIO)) %>%
      slice_head(n = input$topN) %>%
      mutate(DEPARTAMENTO_EDS = factor(DEPARTAMENTO_EDS, levels = rev(DEPARTAMENTO_EDS)))
    
    if (nrow(df) == 0) {
      p <- ggplot() +
        annotate("text", x = 1, y = 1, label = "No hay datos para mostrar", size = 5) +
        theme_void()
    } else {
      p <- ggplot(
        df,
        aes(
          x = DEPARTAMENTO_EDS, y = PRECIO_PROMEDIO,
          text = paste0("<b>", as.character(DEPARTAMENTO_EDS), "</b><br>",
                        "Precio prom: <b>", fmt_money(PRECIO_PROMEDIO), "</b>")
        )
      ) +
        geom_col(fill = "steelblue", alpha = 0.7) +
        coord_flip() +
        labs(x = NULL, y = "Precio prom. (COP)") +
        theme_minimal(base_family = "Arial") +
        theme(axis.text.y = element_text(size = 10))
    }
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l=10, r=10, b=10, t=10))
  })
  
  # ---- SERIE ----
  output$serie <- renderPlotly({
    df <- filtered_data()
    ts <- df %>%
      filter(!is.na(FECHA_PRECIO), !is.na(PRECIO_PROMEDIO_PUBLICADO)) %>%
      group_by(FECHA_PRECIO) %>%
      summarize(PRECIO_PROMEDIO = mean(PRECIO_PROMEDIO_PUBLICADO, na.rm = TRUE), .groups = "drop") %>%
      arrange(FECHA_PRECIO)
    
    if (nrow(ts) == 0) {
      p <- ggplot() +
        annotate("text", x = as.Date("2020-01-01"), y = 1, label = "No hay datos para mostrar", size = 5) +
        theme_void()
    } else {
      p <- ggplot(ts, aes(x = FECHA_PRECIO, y = PRECIO_PROMEDIO)) +
        geom_line(color = "steelblue", size = 1) +
        geom_point(color = "steelblue", size = 1.5, alpha = 0.7) +
        labs(x = NULL, y = "Precio prom. (COP)") +
        theme_minimal(base_family = "Arial") +
        scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))
    }
    ggplotly(p, tooltip = c("x","y"))
  })
}

# Asegurar que Shiny sirva los archivos estáticos correctamente
if (!dir.exists("www") && dir.exists("www:")) {
  # Si existe www: pero no www, renombrar automáticamente
  message("Detectada carpeta 'www:' - creando carpeta 'www' correcta...")
  dir.create("www", showWarnings = FALSE)
  if (file.exists("www:/logo.png")) {
    file.copy("www:/logo.png", "www/logo.png")
    message("Logo copiado de 'www:' a 'www/'")
  }
}

# Crear la app
shinyApp(ui, server)
