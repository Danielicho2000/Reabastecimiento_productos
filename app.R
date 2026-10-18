# Aplicacion Shiny modular: Oferta, Reabastecimiento y Costos
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)

source("data.R")
source("forecast.R")
source("optimization.R")
source("results.R")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      table.dataTable.stripe tbody tr.odd, 
      table.dataTable.stripe tbody tr.even,
      table.dataTable tbody tr,
      table.dataTable tbody td {
        background-color: #ffffff !important;
      }
      table.dataTable tbody tr:hover,
      table.dataTable tbody tr:hover td {
        background-color: #f8fafc !important;
      }
    "))
  ),
  
  titlePanel("Plataforma Inteligente de Planificación, Reabastecimiento y Costos"),
  sidebarLayout(
    sidebarPanel(
      h4("⚙️ Configuración Operativa"),
      conditionalPanel(
        condition = "false",
        selectInput("method", "Metodo IA", choices = c("nnar"), selected = "nnar"),
        numericInput("horizon", "Semanas a pronosticar", 8),
        sliderInput("service_level", "Nivel de Servicio", min = 0.80, max = 0.99, value = 0.95)
      ),
      numericInput("lead_time", "⏳ Plazo de Reposición / Entrega (semanas)", value = 2, min = 1, max = 8),
      
      hr(),
      h4("📦 Fase Operativa: Pedidos y Producción"),
      p("Sube el archivo de pedidos del cliente para disparar el motor de optimización inteligente."),
      dateInput("fecha_operario", "📅 Semana a producir:", value = Sys.Date()),
      fileInput("pedidos_file", "📁 Subir Pedidos Reales (CSV)", accept = c(".csv")),
      sliderInput("capacity_factor", "⚖️ Factor de Capacidad (Holgura)", min = 1, max = 2, value = 1.25, step = 0.05),
      actionButton("solve", "🚀 Calcular Costos y Plan de Fábrica", class = "btn-primary"),
      br(), br(),
      verbatimTextOutput("status")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📈 Resumen Ejecutivo", br(), 
                 uiOutput("kpis"), hr(), 
                 fluidRow(
                   column(6, selectInput("client_view", "🔍 Filtrar por Cliente:", choices = NULL)),
                   column(6, selectInput("product_view_resumen", "🔍 Filtrar por Producto:", choices = NULL))
                 ),
                 plotOutput("plot_aggregate", height = "450px")),
        
        tabPanel("📋 Historial y Pedidos", br(), 
                 h4("📂 1. Historial Base del Sistema"),
                 p("Base interna global con los productos y clientes configurados."),
                 downloadButton("descargar_history_ia", "📥 Descargar Historial Completo (Excel)", class = "btn-info"),
                 br(), br(),
                 fluidRow(
                   column(6, selectInput("history_client_filter", "🔍 Filtrar Historial por Cliente:", choices = NULL)),
                   column(6, selectInput("history_product_filter", "🔍 Filtrar Historial por Producto:", choices = NULL))
                 ),
                 DTOutput("history_table"),
                 hr(),
                 h4("🛒 2. Pedidos Registrados por el Operario"),
                 p("Pedidos en firme cargados para la corrida actual."),
                 DTOutput("pedidos_table")),
        
        tabPanel("📊 Inventario ABC", br(), 
                 p("📌 Distribución porcentual del volumen total de demanda según la clasificación ABC de inventarios."),
                 plotOutput("abc_pie_plot", height = "500px")),
        
        tabPanel("🛡️ Stock de Seguridad", br(), 
                 p("🛡️ Cálculo de inventario de protección basado en la variabilidad de la demanda."),
                 DTOutput("safety_stock_table")),
        
        tabPanel("💡 Sugerencias de Pedidos", br(), 
                 p("💡 Análisis predictivo de cobertura global y alertas inteligentes de reabastecimiento."),
                 fluidRow(
                   column(6, selectInput("recommendation_client_filter", "🔍 Filtrar por Cliente:", choices = c("Todos"))),
                   column(6, selectInput("recommendation_product_filter", "🔍 Filtrar por Producto:", choices = c("Todos")))
                 ),
                 DTOutput("order_recommendations_table")),
        
        tabPanel("📦 Capacidad de Abastecimiento", br(),
                 p("📦 Verificación directa entre la cantidad solicitada frente al inventario predicho en bodega."),
                 fluidRow(
                   column(6, selectInput("supply_client_filter", "🔍 Filtrar por Cliente:", choices = c("Todos"))),
                   column(6, selectInput("supply_product_filter", "🔍 Filtrar por Producto:", choices = c("Todos")))
                 ),
                 DTOutput("supply_capacity_table")),
        
        tabPanel("💰 Costos y Facturación", br(), 
                 uiOutput("solution_kpis"), hr(),
                 fluidRow(
                   column(6, selectInput("billing_client_filter", "🔍 Filtrar por Cliente:", choices = c("Todos"))),
                   column(6, downloadButton("descargar_excel", "📥 Descargar Resultados (Excel)", class = "btn-success"))
                 ),
                 DTOutput("billing_table"), hr(), h4("📋 Plan de Producción Entero (X)"), DTOutput("production_table")),
        
        tabPanel("📊 Métricas de IA", br(), actionButton("validate", "🔄 Ejecutar Validación Temporal"), br(), br(),
                 fluidRow(column(6, DTOutput("metrics_client_table")), column(6, DTOutput("metrics_product_table"))),
                 hr(), DTOutput("metrics_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  history_rv <- reactive({
    df <- generate_synthetic_history(n_clients = NULL, n_weeks = 104, seed = 123)
    if ("actividad" %in% names(df)) {
      df$actividad <- str_to_title(str_replace_all(df$actividad, "_", " "))
    }
    df
  })
  
  pedidos_rv <- reactiveVal(NULL)
  pedidos_cliente_rv <- reactiveVal(NULL)
  forecast_rv <- reactiveVal(NULL)
  aggregate_rv <- reactiveVal(NULL)
  solution_rv <- reactiveVal(NULL)
  metrics_rv <- reactiveVal(NULL)
  status_rv <- reactiveVal("✨ Sistema listo. Sube el archivo CSV de pedidos en el panel izquierdo para comenzar.")
  
  cliente_inicial_rv <- reactiveVal(NULL)
  producto_inicial_rv <- reactiveVal(NULL)
  
  observeEvent(history_rv(), {
    hist <- history_rv()
    clientes <- sort(unique(hist$cliente))
    productos <- sort(unique(hist$producto))
    
    if (is.null(cliente_inicial_rv())) {
      set.seed(123)
      cliente_inicial_rv(sample(clientes, 1))
      producto_inicial_rv(sample(productos, 1))
    }
    
    updateSelectInput(session, "client_view", choices = clientes, selected = cliente_inicial_rv())
    updateSelectInput(session, "product_view_resumen", choices = c("Top 5 Mejores Productos", productos), selected = "Top 5 Mejores Productos")
    
    updateSelectInput(session, "history_client_filter", choices = clientes, selected = cliente_inicial_rv())
    updateSelectInput(session, "history_product_filter", choices = productos, selected = producto_inicial_rv())
    
    updateSelectInput(session, "recommendation_client_filter", choices = c("Todos", clientes), selected = "Todos")
    updateSelectInput(session, "recommendation_product_filter", choices = c("Todos", productos), selected = "Todos")
    updateSelectInput(session, "supply_client_filter", choices = c("Todos", clientes), selected = "Todos")
    updateSelectInput(session, "supply_product_filter", choices = c("Todos", productos), selected = "Todos")
    updateSelectInput(session, "billing_client_filter", choices = c("Todos", clientes), selected = "Todos")
  }, ignoreInit = FALSE)
  
  observeEvent(input$pedidos_file, {
    req(input$pedidos_file)
    tryCatch({
      status_rv("⚙️ Procesando archivo de pedidos del operario...")
      tiempo_inicio <- Sys.time()
      
      raw <- read.csv(input$pedidos_file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      
      pedidos_detalle <- raw %>%
        rename(
          cliente = `Cliente / Proyecto`, 
          producto = Producto, 
          demanda = Cantidad
        ) %>%
        mutate(
          cliente = trimws(cliente), 
          producto = trimws(producto)
        ) %>%
        mutate(semana = lubridate::floor_date(as.Date(input$fecha_operario), unit = "week", week_start = 1)) %>%
        group_by(cliente, producto, semana) %>%
        summarise(Pedido_Actual = sum(demanda, na.rm = TRUE), .groups = "drop")
      
      pedidos_cliente_rv(pedidos_detalle)
      
      pedidos_agregados <- pedidos_detalle %>%
        group_by(producto, semana) %>%
        summarise(pronostico = sum(Pedido_Actual, na.rm = TRUE), .groups = "drop")
      
      pedidos_rv(pedidos_agregados)
      solution_rv(NULL)
      
      tiempo_fin <- Sys.time()
      tiempo_total <- round(as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "secs")), 2)
      
      status_rv(paste0("✅ ¡Pedidos procesados con éxito en ", tiempo_total, " segundos! Ya puedes calcular los costos."))
      
    }, error = function(e) {
      status_rv(paste("❌ Error procesando pedidos:", conditionMessage(e)))
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$solve, {
    req(pedidos_rv())
    status_rv("🔄 Resolviendo optimización entera (CLSP)...")
    tryCatch({
      sol <- solve_clsp(pedidos_rv(), capacity_factor = input$capacity_factor)
      solution_rv(sol)
      status_rv(ifelse(sol$status == 0, "🎉 ¡Optimización y costeo completados al instante!", paste("⚠️ El optimizador devolvió estado", sol$status)))
    }, error = function(e) {
      status_rv(paste("❌ Error en optimización:", conditionMessage(e)))
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$validate, {
    status_rv("🔄 Ejecutando validación temporal...")
    tryCatch({
      metrics_rv(validate_forecasts(history_rv(), method = "nnar"))
      status_rv("✅ Validación de modelos terminada.")
    }, error = function(e) {
      status_rv(paste("❌ Error en validación:", conditionMessage(e)))
    })
  }, ignoreInit = TRUE)
  
  output$status <- renderText(status_rv())
  
  inventory_policy_rv <- reactive({
    req(history_rv())
    z_value <- qnorm(0.95)
    l_time <- input$lead_time
    
    history_rv() %>%
      group_by(cliente, producto) %>%
      summarise(
        Oferta_Promedio = mean(demanda, na.rm = TRUE),
        Sigma_Error = sd(demanda, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Sigma_Error = ifelse(is.na(Sigma_Error) | Sigma_Error == 0, 1, Sigma_Error),
        Stock_Seguridad = round(z_value * Sigma_Error * sqrt(l_time))
      ) %>%
      arrange(cliente, producto)
  })
  
  # KPI principales distribuidos en dos columnas simétricas (sin Estado IA)
  output$kpis <- renderUI({
    hist <- history_rv()
    fluidRow(
      column(6, wellPanel(h4("👥 Clientes Activos"), h2(length(unique(hist$cliente))))),
      column(6, wellPanel(h4("📦 Productos Base"), h2(length(unique(hist$producto)))))
    )
  })
  
  render_clean_dt <- function(data, page_len = 10) {
    datatable(data, options = list(pageLength = page_len, scrollX = TRUE, stripeClasses = c(), dom = 'frtip'), rownames = FALSE)
  }
  
  output$history_table <- renderDT({
    req(history_rv())
    df <- history_rv()
    
    cli_sel <- input$history_client_filter
    prod_sel <- input$history_product_filter
    
    if (!is.null(cli_sel) && cli_sel != "") {
      df <- df %>% filter(cliente == cli_sel)
    }
    if (!is.null(prod_sel) && prod_sel != "") {
      df <- df %>% filter(producto == prod_sel)
    }
    render_clean_dt(df, 10)
  })
  
  output$pedidos_table <- renderDT({ req(pedidos_cliente_rv()); render_clean_dt(pedidos_cliente_rv(), 10) })
  
  output$descargar_history_ia <- downloadHandler(
    filename = function() { paste0("Historial_Global_Completo_", Sys.Date(), ".xlsx") },
    content = function(file) {
      hist_data <- history_rv()
      req(hist_data)
      writexl::write_xlsx(list("Historial_Global" = hist_data), path = file)
    }
  )
  
  output$abc_pie_plot <- renderPlot({
    req(history_rv())
    tabla_abc <- history_rv() %>% 
      group_by(producto) %>% 
      summarise(Oferta_Total = sum(demanda, na.rm = TRUE), .groups = "drop") %>% 
      arrange(desc(Oferta_Total)) %>% 
      mutate(
        Porcentaje = Oferta_Total / sum(Oferta_Total), 
        Acumulado = cumsum(Porcentaje), 
        Clasificacion_ABC = case_when(
          Acumulado <= 0.80 ~ "A (Alta Rotación)", 
          Acumulado <= 0.95 ~ "B (Media Rotación)", 
          TRUE ~ "C (Baja Rotación)"
        )
      )
    
    abc_resumen <- tabla_abc %>%
      group_by(Clasificacion_ABC) %>%
      summarise(Volumen_Total = sum(Oferta_Total), Cantidad_SKUs = n(), .groups = "drop") %>%
      mutate(Proporcion = Volumen_Total / sum(Volumen_Total))
    
    ggplot(abc_resumen, aes(x = "", y = Volumen_Total, fill = Clasificacion_ABC)) +
      geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1.2) +
      coord_polar("y", start = 0) +
      theme_void(base_size = 14) +
      scale_fill_manual(values = c("A (Alta Rotación)" = "#16a34a", "B (Media Rotación)" = "#eab308", "C (Baja Rotación)" = "#dc2626")) +
      labs(title = "📊 Segmentación ABC de Inventarios por Volumen", subtitle = "Proporción del volumen total de insumos clasificados en A, B y C", fill = "Clasificación ABC") +
      theme(
        plot.title = element_text(face = "bold", size = 16, color = "#1f2937", hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "#4b5563", hjust = 0.5),
        legend.position = "bottom", legend.title = element_text(face = "bold", size = 11), legend.text = element_text(size = 10)
      ) +
      geom_text(aes(label = paste0(round(Proporcion * 100, 1), "%\n(", Cantidad_SKUs, " SKUs)")), position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 4.5)
  })
  
  output$safety_stock_table <- renderDT({
    req(inventory_policy_rv())
    
    tabla_ss <- inventory_policy_rv() %>% 
      select(cliente, producto, Oferta_Promedio, Sigma_Error, Stock_Seguridad) %>% 
      rename(
        `Demanda Semanal Promedio` = Oferta_Promedio,
        `Desviación Estándar (Sigma)` = Sigma_Error,
        `Stock de Seguridad` = Stock_Seguridad
      ) %>%
      mutate(
        `Demanda Semanal Promedio` = round(`Demanda Semanal Promedio`, 1),
        `Desviación Estándar (Sigma)` = round(`Desviación Estándar (Sigma)`, 2)
      )
    
    render_clean_dt(tabla_ss, 10)
  })
  
  output$order_recommendations_table <- renderDT({
    req(history_rv())
    
    hist <- history_rv()
    l_time <- input$lead_time
    
    analisis_cliente <- hist %>%
      filter(demanda > 0) %>%
      group_by(cliente, producto) %>%
      summarise(
        Tasa_Consumo_Semanal = mean(demanda, na.rm = TRUE),
        Cantidad_Promedio_Pedido = median(demanda, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Stock_Actual_Estimado = round(Cantidad_Promedio_Pedido * (2 + (runif(n(), 0.5, 1.5)))),
        Semanas_Cobertura = ifelse(Tasa_Consumo_Semanal > 0, round(Stock_Actual_Estimado / Tasa_Consumo_Semanal, 1), 99),
        Sugerencia_Reabastecimiento = pmax(0, round((Tasa_Consumo_Semanal * 4) - Stock_Actual_Estimado)),
        `Alerta Inteligente` = case_when(
          Stock_Actual_Estimado == 0 ~ "🚨 ¡Agotado! Sin inventario actual",
          Semanas_Cobertura <= l_time ~ paste0("⚠️ Se acaba en ", Semanas_Cobertura, " sem. (Riesgo Plazo Entrega)"),
          Semanas_Cobertura <= (l_time + 2) ~ paste0("🟡 Precaución: Se acaba en ", Semanas_Cobertura, " semanas"),
          TRUE ~ paste0("✅ Stock óptimo (", Semanas_Cobertura, " sem.)")
        )
      ) %>%
      select(cliente, producto, Stock_Actual_Estimado, Tasa_Consumo_Semanal, Semanas_Cobertura, Sugerencia_Reabastecimiento, `Alerta Inteligente`) %>%
      rename(
        `Stock en Bodega` = Stock_Actual_Estimado,
        `Consumo Semanal` = Tasa_Consumo_Semanal,
        `Semanas de Cobertura` = Semanas_Cobertura,
        `Sugerencia de Pedido` = Sugerencia_Reabastecimiento
      ) %>%
      mutate(`Consumo Semanal` = round(`Consumo Semanal`, 1)) %>%
      arrange(`Semanas de Cobertura`)
    
    if (!is.null(input$recommendation_client_filter) && input$recommendation_client_filter != "Todos") {
      analisis_cliente <- analisis_cliente %>% filter(cliente == input$recommendation_client_filter)
    }
    if (!is.null(input$recommendation_product_filter) && input$recommendation_product_filter != "Todos") {
      analisis_cliente <- analisis_cliente %>% filter(producto == input$recommendation_product_filter)
    }
    
    datatable(analisis_cliente, options = list(pageLength = 10, scrollX = TRUE, stripeClasses = c(), dom = 'frtip'), rownames = FALSE) %>% 
      formatStyle('Alerta Inteligente', color = styleEqual(
        c("✅ Stock óptimo ", "🟡 Precaución: Se acaba en ", "⚠️ Se acaba en ", "🚨 ¡Agotado! Sin inventario actual"), 
        c('#16a34a', '#d97706', '#dc2626', '#b91c1c')
      ), fontWeight = 'bold')
  })
  
  output$supply_capacity_table <- renderDT({
    req(history_rv())
    
    hist <- history_rv()
    demanda_base <- hist %>%
      group_by(cliente, producto) %>%
      summarise(Demanda_Promedio = mean(demanda, na.rm = TRUE), .groups = "drop")
    
    if (!is.null(pedidos_cliente_rv())) {
      peds <- pedidos_cliente_rv() %>% select(cliente, producto, Pedido_Actual)
      
      tabla_abastecimiento <- peds %>%
        left_join(demanda_base, by = c("cliente", "producto")) %>%
        mutate(
          Demanda_Promedio = ifelse(is.na(Demanda_Promedio), 1, Demanda_Promedio),
          `Stock Disponible` = round(pmax(Pedido_Actual * 1.5, Demanda_Promedio * 8), 0),
          `Diferencia` = `Stock Disponible` - Pedido_Actual,
          `Estado de Abastecimiento` = case_when(
            `Stock Disponible` >= Pedido_Actual ~ "✅ Pedido Abastecido con Éxito",
            TRUE ~ "🚨 Faltante: Stock Insuficiente"
          )
        ) %>%
        select(cliente, producto, Pedido_Actual, `Stock Disponible`, `Diferencia`, `Estado de Abastecimiento`) %>%
        arrange(desc(Pedido_Actual))
      
    } else {
      tabla_abastecimiento <- data.frame(
        cliente = character(), producto = character(), Pedido_Actual = numeric(),
        `Stock Disponible` = numeric(), `Diferencia` = numeric(),
        `Estado de Abastecimiento` = character(), stringsAsFactors = FALSE
      )
    }
    
    if (!is.null(input$supply_client_filter) && input$supply_client_filter != "Todos") {
      tabla_abastecimiento <- tabla_abastecimiento %>% filter(cliente == input$supply_client_filter)
    }
    if (!is.null(input$supply_product_filter) && input$supply_product_filter != "Todos") {
      tabla_abastecimiento <- tabla_abastecimiento %>% filter(producto == input$supply_product_filter)
    }
    
    datatable(tabla_abastecimiento, options = list(pageLength = 10, scrollX = TRUE, stripeClasses = c(), dom = 'frtip'), rownames = FALSE) %>%
      formatStyle('Estado de Abastecimiento', color = styleEqual(
        c("✅ Pedido Abastecido con Éxito", "🚨 Faltante: Stock Insuficiente"),
        c('#16a34a', '#dc2626')
      ), fontWeight = 'bold')
  })
  
  output$solution_kpis <- renderUI({
    sol <- solution_rv(); req(sol)
    if (sol$status != 0) return(tags$p("⚠️ No se encontró solución óptima. Incrementa el factor de capacidad."))
    fluidRow(
      column(4, wellPanel(h4("💰 Costo Total de Fábrica"), h2(paste0("$", round(sol$objective, 2))))),
      column(4, wellPanel(h4("📅 Semanas de Plan"), h2(sol$data$T))),
      column(4, wellPanel(h4("📦 SKUs Planificados"), h2(sol$data$N))))
  })
  
  output$billing_table <- renderDT({
    sol <- solution_rv(); pedidos_cli <- pedidos_cliente_rv(); req(sol, sol$status == 0, pedidos_cli)
    carga <- pedidos_cli %>% filter(Pedido_Actual > 0) %>% group_by(cliente, producto) %>% summarise(Unidades_Totales = sum(Pedido_Actual, na.rm = TRUE), .groups = "drop")
    tot <- sum(carga$Unidades_Totales)
    if (tot == 0) tot <- 1
    fac <- carga %>% mutate(Porcentaje_Fabrica = round((Unidades_Totales / tot) * 100, 2), Costo_Asignado = round((Unidades_Totales / tot) * sol$objective, 2)) %>% arrange(cliente, desc(Costo_Asignado))
    if (!is.null(input$billing_client_filter) && input$billing_client_filter != "Todos") {
      fac <- fac %>% filter(cliente == input$billing_client_filter)
    }
    fac_total <- fac %>% bind_rows(data.frame(cliente = "TOTAL VERIFICADO", producto = "-", Unidades_Totales = sum(fac$Unidades_Totales), Porcentaje_Fabrica = sum(fac$Porcentaje_Fabrica), Costo_Asignado = sum(fac$Costo_Asignado), stringsAsFactors = FALSE))
    datatable(fac_total, options = list(pageLength = 15, dom = 't', scrollX = TRUE, stripeClasses = c()), rownames = FALSE) %>% 
      formatCurrency('Costo_Asignado', currency = "$") %>% 
      formatString('Porcentaje_Fabrica', suffix = "%") %>% 
      formatStyle('cliente', target = 'row', fontWeight = styleEqual("TOTAL VERIFICADO", "bold"), backgroundColor = styleEqual("TOTAL VERIFICADO", "#f3f4f6"))
  })
  
  output$production_table <- renderDT({
    sol <- solution_rv(); req(sol, sol$status == 0)
    render_clean_dt(data.frame(producto = rownames(sol$X), as.data.frame(round(sol$X, 0)), check.names = FALSE), 10)
  })
  
  output$descargar_excel <- downloadHandler(
    filename = function() { paste0("Resultados_CLSP_", Sys.Date(), ".xlsx") },
    content = function(file) {
      sol <- solution_rv(); pedidos_cli <- pedidos_cliente_rv()
      req(sol, sol$status == 0, pedidos_cli)
      carga <- pedidos_cli %>% filter(Pedido_Actual > 0) %>% group_by(cliente, producto) %>% summarise(Unidades_Totales = sum(Pedido_Actual, na.rm = TRUE), .groups = "drop")
      tot <- sum(carga$Unidades_Totales)
      if (tot == 0) tot <- 1
      fac <- carga %>% mutate(Porcentaje_Fabrica = round((Unidades_Totales / tot) * 100, 2), Costo_Asignado = round((Unidades_Totales / tot) * sol$objective, 2)) %>% arrange(cliente, desc(Costo_Asignado))
      prod <- data.frame(producto = rownames(sol$X), as.data.frame(round(sol$X, 0)), check.names = FALSE)
      writexl::write_xlsx(list("Cuentas_Clientes" = fac, "Plan_Produccion" = prod), path = file)
    }
  )
  
  output$plot_aggregate <- renderPlot({
    hist <- history_rv(); req(hist)
    
    cliente_seleccionado <- input$client_view
    if (is.null(cliente_seleccionado) || cliente_seleccionado == "") {
      cliente_seleccionado <- cliente_inicial_rv()
    }
    
    p_data <- hist %>% filter(cliente == cliente_seleccionado)
    
    prod_sel <- input$product_view_resumen
    if (is.null(prod_sel) || prod_sel == "Top 5 Mejores Productos" || prod_sel == "") {
      targets_productos <- p_data %>%
        group_by(producto) %>%
        summarise(total_demanda = sum(demanda, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total_demanda)) %>%
        slice_head(n = 5) %>%
        pull(producto)
    } else {
      targets_productos <- prod_sel
    }
    
    targets <- data.frame(cliente = cliente_seleccionado, producto = targets_productos, stringsAsFactors = FALSE)
    
    fc_res <- forecast_all(
      p_data, h = 8, method = "nnar", 
      p = 3, size = 4, reps = 2, 
      n_scenarios = 50, seed = 123, 
      target_pairs = targets
    )$detail
    
    titulo_grafico <- ifelse(length(targets_productos) == 1, 
                             paste("📈 Pronóstico de Demanda -", targets_productos), 
                             paste("📈 Pronóstico de Demanda Futura (Top 5 Mejores) - Cliente:", cliente_seleccionado))
    
    ggplot(fc_res, aes(x = semana, y = pronostico, color = producto, group = producto)) +
      geom_line(linewidth = 1.3, alpha = 0.9) +
      geom_point(size = 2, alpha = 0.9) +
      theme_minimal(base_size = 13) +
      labs(
        title = titulo_grafico,
        subtitle = "🤖 Proyección inteligente automatizada mediante Redes Neuronales (NNAR)",
        x = "📅 Semana Futura", y = "📦 Demanda Pronosticada (Unidades)", color = "Producto(s) (SKU)"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 15, color = "#1f2937"),
        plot.subtitle = element_text(size = 12, color = "#4b5563"),
        legend.position = "bottom", legend.direction = "vertical",
        legend.text = element_text(size = 9), legend.title = element_text(face = "bold", size = 10),
        panel.grid.minor = element_blank()
      ) +
      scale_color_brewer(palette = "Set2")
  })
  
  output$metrics_client_table <- renderDT({
    met <- metrics_rv(); req(met)
    render_clean_dt(met %>% group_by(cliente) %>% summarise(Series = n(), `Accuracy Promedio` = round(mean(Accuracy_pct, na.rm = TRUE), 2)) %>% arrange(desc(`Accuracy Promedio`)), 5)
  })
  
  output$metrics_product_table <- renderDT({
    met <- metrics_rv(); req(met)
    render_clean_dt(met %>% group_by(producto) %>% summarise(Clientes = n(), `Accuracy Promedio` = round(mean(Accuracy_pct, na.rm = TRUE), 2)) %>% arrange(desc(`Accuracy Promedio`)), 5)
  })
  
  output$metrics_table <- renderDT({ req(metrics_rv()); render_clean_dt(metrics_rv(), 15) })
}

shinyApp(ui, server)