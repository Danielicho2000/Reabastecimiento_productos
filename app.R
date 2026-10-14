# Aplicacion Shiny modular: demanda por cliente-producto + forecast + CLSP
# Ejecutar desde la carpeta del proyecto con: shiny::runApp()

library(shiny)
library(DT)
library(ggplot2)
library(tidyr)

source("data.R")
source("forecast.R")
source("optimization.R")
source("results.R")

# -------------------------------------------------------------
# 10. HTML "MODELO" con variables, parámetros, formulación, IA y datasets
# -------------------------------------------------------------
modelo_html <- r"---(
<style>
  .desc-box { background:#f7f9fb; border-left:4px solid #2c7fb8;
              padding:12px 16px; margin-bottom:16px; border-radius:4px; }
  .desc-box h4 { margin-top:0; }
  .note { background:#fff8e1; border-left:4px solid #ffb300;
          padding:10px 14px; border-radius:4px; margin-bottom:14px; }
  table.desc { width:100%; border-collapse:collapse; margin-bottom:18px; }
  table.desc th, table.desc td {
    border:1px solid #d0d7de; padding:6px 10px; text-align:left;
    vertical-align:top; font-size:13.5px; }
  table.desc th { background:#eaeff5; }
  table.desc code { background:#eef2f7; padding:1px 5px; border-radius:3px; }
  h3.section { color:#2c7fb8; margin-top:26px; }
  .math-box { background:#f7f9fb; border-left:4px solid #2c7fb8;
              padding:14px 18px; border-radius:4px; margin-bottom:16px;
              overflow-x:auto; }
</style>

<!-- ============================= -->
<div class='desc-box'>
  <h4>CLSP-TW semanal &mdash; con pron&oacute;stico por IA (NNAR)</h4>
  <p><b>Pipeline completo:</b>
  historia por cliente &rarr; pron&oacute;stico NNAR (IA) &rarr;
  agregaci&oacute;n por producto &rarr; stock de seguridad y punto de reorden
  &rarr; planeaci&oacute;n de lotes con ventanas de tiempo (CLSP-TW).</p>
  <p>Modelo base: Brahimi, Dauz&egrave;re-P&eacute;r&egrave;s &amp; Najid (2006),
  <i>Operations Research</i>. <b>Unidad de tiempo: 1 semana.</b></p>
</div>

<!-- ============================================================ -->
<h3 class='section'>1. Variables de decisi&oacute;n del CLSP-TW</h3>
<table class='desc'>
  <tr><th style='width:15%'>S&iacute;mbolo</th>
      <th>Descripci&oacute;n</th>
      <th style='width:22%'>Tipo</th></tr>
  <tr><td>\(X_{it}\)</td>
      <td>Unidades del producto \(i\) producidas en la semana \(t\).</td>
      <td>Continua \(\ge 0\)</td></tr>
  <tr><td>\(I_{it}\)</td>
      <td>Inventario del producto \(i\) al final de la semana \(t\).</td>
      <td>Continua \(\ge 0\)</td></tr>
  <tr><td>\(Y_{it}\)</td>
      <td>Variable binaria de setup: 1 si en la semana \(t\) se produce
      el producto \(i\); 0 en caso contrario.</td>
      <td>Binaria \(\{0,1\}\)</td></tr>
</table>

<!-- ============================================================ -->
<h3 class='section'>2. Par&aacute;metros del CLSP-TW</h3>
<table class='desc'>
  <tr><th style='width:15%'>S&iacute;mbolo</th><th>Descripci&oacute;n</th><th style='width:22%'>Unidad</th></tr>
  <tr><td>\(p_{it}\)</td><td>Costo unitario de producci&oacute;n.</td><td>u.m. / unidad</td></tr>
  <tr><td>\(h_{it}\)</td><td>Costo unitario de inventario por semana.</td><td>u.m. / (unidad&middot;sem)</td></tr>
  <tr><td>\(s_{it}\)</td><td>Costo fijo de setup al producir \(i\) en la semana \(t\).</td><td>u.m.</td></tr>
  <tr><td>\(\sigma_i\)</td><td>Consumo unitario de capacidad por unidad de producto.</td><td>recurso / unidad</td></tr>
  <tr><td>\(C_t\)</td><td>Capacidad total disponible en la semana \(t\).</td><td>recurso / semana</td></tr>
  <tr><td>\(d_{ist}\)</td><td>Demanda del producto \(i\) liberada en la semana \(s\)
      con vencimiento en la semana \(t\) (\(s \le t\)).</td><td>unidades</td></tr>
  <tr><td>\(D_{it}\)</td><td>Demanda agregada con vencimiento en \(t\):
      \(D_{it} = \sum_{s=1}^{t} d_{ist}\).</td><td>unidades</td></tr>
  <tr><td>\(M\)</td><td>Constante Big-M: \(M = \sum_{i,t} D_{it}\).</td><td>unidades</td></tr>
  <tr><td>\(N\)</td><td>N&uacute;mero de productos.</td><td>&mdash;</td></tr>
  <tr><td>\(T\)</td><td>Horizonte de planeaci&oacute;n en semanas.</td><td>semanas</td></tr>
  <tr><td>\(K\)</td><td>N&uacute;mero de clientes.</td><td>&mdash;</td></tr>
</table>

<!-- ============================================================ -->
<h3 class='section'>3. Formulaci&oacute;n matem&aacute;tica del CLSP-TW</h3>
<div class='math-box'>
$$
\begin{aligned}
\min \quad
& \sum_{i=1}^{N}\sum_{t=1}^{T}
  \bigl(\, s_{it}\,Y_{it} + p_{it}\,X_{it} + h_{it}\,I_{it} \,\bigr) \\[6pt]
\text{s.a.} \quad
& I_{i,\,t-1} + X_{it} \;=\; D_{it} + I_{it}
  && \forall\, i,\,t \quad \text{(balance)} \\[2pt]
& X_{it} \;\le\; M \cdot Y_{it}
  && \forall\, i,\,t \quad \text{(setup)} \\[2pt]
& \sum_{k=1}^{t} X_{ik} \;\le\;
  \sum_{k=1}^{t}\sum_{l=k}^{T} d_{ikl}
  && \forall\, i,\,t \quad \text{(release)} \\[2pt]
& \sum_{k=t_1}^{t_2} X_{ik} \;\ge\;
  \sum_{k=t_1}^{t_2}\sum_{l=k}^{t_2} d_{ikl}
  && \forall\, i,\; t_1 \le t_2 \quad \text{(due)} \\[2pt]
& \sum_{i=1}^{N} \sigma_i\, X_{it} \;\le\; C_t
  && \forall\, t \quad \text{(capacidad)} \\[2pt]
& X_{it},\; I_{it} \;\ge\; 0, \quad Y_{it} \in \{0,1\}
  && \forall\, i,\,t
\end{aligned}
$$
</div>

<table class='desc'>
  <tr><th style='width:20%'>Restricci&oacute;n</th><th>Interpretaci&oacute;n</th></tr>
  <tr><td>Balance</td>
      <td>El inventario previo m&aacute;s lo producido cubre la demanda con
      vencimiento en \(t\) m&aacute;s el inventario final.</td></tr>
  <tr><td>Setup</td>
      <td>Si \(X_{it} > 0\) entonces \(Y_{it} = 1\); no se produce sin
      arrancar la l&iacute;nea.</td></tr>
  <tr><td>Release</td>
      <td>No se puede producir antes de la semana \(s\) en la que la demanda
      se libera.</td></tr>
  <tr><td>Due</td>
      <td>En cualquier intervalo \([t_1, t_2]\) debe haberse producido al
      menos la demanda que vence dentro de ese intervalo.</td></tr>
  <tr><td>Capacidad</td>
      <td>El consumo total de recurso en la semana \(t\) no excede \(C_t\).</td></tr>
</table>

<!-- ============================================================ -->
<h3 class='section'>4. Pron&oacute;stico por IA &mdash; NNAR</h3>
<div class='note'>
  <b>NNAR</b> (Neural Network AutoRegressive) es una red neuronal de una capa
  oculta que aprende la din&aacute;mica de la serie a partir de sus lags
  recientes y estacionales. <b>No exige estacionariedad</b>: puede capturar
  tendencia y estacionalidad directamente.
</div>

<div class='math-box'>
$$
\hat y_t \;=\; f\!\left(y_{t-1},\, y_{t-2},\, \dots,\, y_{t-p},\;
                       y_{t-s},\, y_{t-2s},\, \dots \right) + \varepsilon_t
$$
</div>

<div class='math-box'>
$$
f(\mathbf{z}) \;=\; \beta_0 + \sum_{j=1}^{k} \beta_j\,
                   \psi\!\left(\gamma_{j0} + \sum_{m} \gamma_{jm} z_m\right),
\qquad \psi(u) = \frac{1}{1 + e^{-u}}
$$
</div>

<table class='desc'>
  <tr><th style='width:22%'>S&iacute;mbolo</th><th>Descripci&oacute;n</th><th style='width:20%'>Valor t&iacute;pico</th></tr>
  <tr><td>\(p\)</td><td>N&uacute;mero de lags autorregresivos (semanas previas).</td><td>1&ndash;6</td></tr>
  <tr><td>\(P\)</td><td>N&uacute;mero de lags estacionales (aqu&iacute; \(P=1\)).</td><td>1</td></tr>
  <tr><td>\(k\)</td><td>N&uacute;mero de neuronas en la capa oculta (<code>size</code>).</td><td>2&ndash;20</td></tr>
  <tr><td>\(s\)</td><td>Periodo estacional (52 semanas = ciclo anual).</td><td>13, 26, 52</td></tr>
  <tr><td>\(\psi\)</td><td>Funci&oacute;n de activaci&oacute;n log&iacute;stica.</td><td>&mdash;</td></tr>
  <tr><td>\(reps\)</td><td>Repeticiones con inicializaciones aleatorias;
      se promedian (bagging) para reducir varianza.</td><td>5&ndash;50</td></tr>
</table>

<!-- ============================================================ -->
<h3 class='section'>5. Stock de seguridad y punto de reorden</h3>
<div class='math-box'>
$$
\begin{aligned}
z      &\;=\; \Phi^{-1}(\text{Nivel de servicio}) \\[4pt]
SS_i   &\;=\; z \cdot \sigma_i^{\text{resid}} \cdot \sqrt{L} \\[4pt]
ROP_i  &\;=\; \bar d_i \cdot L \;+\; SS_i
\end{aligned}
$$
</div>

<table class='desc'>
  <tr><th style='width:24%'>S&iacute;mbolo</th><th>Descripci&oacute;n</th><th style='width:22%'>Unidad</th></tr>
  <tr><td>\(\sigma_i^{\text{resid}}\)</td>
      <td>Desviaci&oacute;n est&aacute;ndar de los residuos del pron&oacute;stico.</td>
      <td>unidades / semana</td></tr>
  <tr><td>\(\bar d_i\)</td>
      <td>Demanda media pronosticada.</td><td>unidades / semana</td></tr>
  <tr><td>\(L\)</td>
      <td>Lead time.</td><td>semanas</td></tr>
  <tr><td>\(z\)</td>
      <td>Cuantil normal est&aacute;ndar del nivel de servicio
      (\(z = 1.645\) para 95%).</td><td>&mdash;</td></tr>
  <tr><td>\(SS_i\)</td><td>Stock de seguridad.</td><td>unidades</td></tr>
  <tr><td>\(ROP_i\)</td><td>Punto de reorden.</td><td>unidades</td></tr>
</table>

<p><b>Agregaci&oacute;n por producto:</b>
\(\sigma_i^{\text{agg}} = \sqrt{\sum_{k} \bigl(\sigma_{k,i}^{\text{resid}}\bigr)^{2}}\)
asumiendo independencia entre clientes.</p>

<!-- ============================================================ -->
<h3 class='section'>6. Clientes por tipo de actividad</h3>
<table class='desc'>
  <tr><th style='width:20%'>Actividad</th>
      <th>Productos t&iacute;picos</th>
      <th style='width:30%'>Patr&oacute;n estacional</th></tr>
  <tr><td>Polic&iacute;a</td>
      <td>Desinfectante, papel higi&eacute;nico, jab&oacute;n</td>
      <td>Pico en operativos (sem 40&ndash;52)</td></tr>
  <tr><td>Hospital</td>
      <td>Desinfectante, jab&oacute;n, toallas</td>
      <td>Estable, ligero pico invernal</td></tr>
  <tr><td>Centro comercial</td>
      <td>Papel higi&eacute;nico, toallas, bolsas</td>
      <td>Pico Navidad (sem 48&ndash;52) y verano</td></tr>
  <tr><td>Club deportivo</td>
      <td>Jab&oacute;n, desinfectante, toallas</td>
      <td>Pico de temporada (sem 10&ndash;30)</td></tr>
  <tr><td>Colegio</td>
      <td>Papel higi&eacute;nico, jab&oacute;n, toallas</td>
      <td>Ca&iacute;da en vacaciones (sem 1&ndash;6, 24&ndash;32)</td></tr>
  <tr><td>Restaurante</td>
      <td>Detergente, desinfectante, bolsas</td>
      <td>Pico fin de a&ntilde;o</td></tr>
  <tr><td>Oficina</td>
      <td>Papel higi&eacute;nico, jab&oacute;n</td>
      <td>Estable</td></tr>
  <tr><td>Industria</td>
      <td>Detergente, desinfectante, bolsas</td>
      <td>Estable</td></tr>
</table>

<!-- ============================================================ -->
<h3 class='section'>7. Base de datos &mdash; 3 CSV de entrada</h3>
<table class='desc'>
  <tr><th style='width:20%'>Archivo</th>
      <th>Columnas</th>
      <th style='width:28%'>Unidad / Rango</th></tr>
  <tr>
    <td><code>costos.csv</code></td>
    <td><code>producto</code>, <code>semana</code>, <code>p_it</code>,
        <code>h_it</code>, <code>s_it</code>, <code>sigma_i</code></td>
    <td>u.m., u.m./unidad, unidades de recurso</td>
  </tr>
  <tr>
    <td><code>ventanas.csv</code></td>
    <td><code>producto</code>, <code>release</code>, <code>due</code>,
        <code>demanda</code></td>
    <td>release/due = n&deg; de semana; demanda en unidades</td>
  </tr>
  <tr>
    <td><code>capacidad.csv</code></td>
    <td><code>semana</code>, <code>C_t</code></td>
    <td>unidades de recurso / semana</td>
  </tr>
</table>

<h4>Descripci&oacute;n campo por campo</h4>
<table class='desc'>
  <tr><th style='width:20%'>Campo</th><th>Descripci&oacute;n</th></tr>
  <tr><td><code>producto</code></td>
      <td>Identificador del &iacute;tem (ej. <code>Jabon_liquido</code>).</td></tr>
  <tr><td><code>semana</code></td>
      <td>N&uacute;mero entero de semana dentro del horizonte (1&hellip;T).</td></tr>
  <tr><td><code>p_it</code></td>
      <td>Costo unitario de producci&oacute;n del producto en esa semana.</td></tr>
  <tr><td><code>h_it</code></td>
      <td>Costo de mantener una unidad en inventario una semana.</td></tr>
  <tr><td><code>s_it</code></td>
      <td>Costo fijo de setup (arranque de l&iacute;nea) en esa semana.</td></tr>
  <tr><td><code>sigma_i</code></td>
      <td>Recurso consumido por unidad producida (constante por producto).</td></tr>
  <tr><td><code>release</code></td>
      <td>Primera semana en la que la demanda puede producirse.</td></tr>
  <tr><td><code>due</code></td>
      <td>&Uacute;ltima semana en la que la demanda debe estar producida
      (<code>release &le; due</code>).</td></tr>
  <tr><td><code>demanda</code></td>
      <td>Cantidad demandada en esa ventana.</td></tr>
  <tr><td><code>C_t</code></td>
      <td>Capacidad total de recurso disponible en la semana.</td></tr>
</table>

<!-- ============================================================ -->
<h3 class='section'>8. Salidas del sistema</h3>
<table class='desc'>
  <tr><th style='width:24%'>Salida</th><th>Contenido</th></tr>
  <tr><td><code>tbl_clientes</code></td>
      <td>Cat&aacute;logo de clientes con su tipo de actividad.</td></tr>
  <tr><td><code>plot_cli_hist</code></td>
      <td>Historia semanal del cliente seleccionado, por producto.</td></tr>
  <tr><td><code>plot_fc</code></td>
      <td>Historia + ajuste + pron&oacute;stico NNAR con banda de
      confianza del 95%, por producto.</td></tr>
  <tr><td><code>tbl_orders</code></td>
      <td>Modelo elegido (NNAR / SARIMA / Holt / SES / MA) y AICc por
      cliente-producto.</td></tr>
  <tr><td><code>tbl_ss_rop_cli</code></td>
      <td>SS y ROP por cliente-producto.</td></tr>
  <tr><td><code>tbl_ss_rop_prod</code></td>
      <td>SS y ROP agregados por producto (suma de varianzas).</td></tr>
  <tr><td><code>tbl_dagg</code></td>
      <td>Demanda agregada pronosticada (base del CLSP-TW).</td></tr>
  <tr><td><code>tbl_X, tbl_I, tbl_Y</code></td>
      <td>Producci&oacute;n, inventario y setups por producto-semana.</td></tr>
  <tr><td><code>tot_cost</code></td>
      <td>Valor &oacute;ptimo de la funci&oacute;n objetivo.</td></tr>
  <tr><td><code>plot_cap, plot_X, plot_I</code></td>
      <td>Utilizaci&oacute;n de capacidad, plan de producci&oacute;n e
      inventario semanal.</td></tr>
</table>

<!-- ============================================================ -->
<div class='desc-box'>
  <h4>Referencia</h4>
  <p>Brahimi, N., Dauz&egrave;re-P&eacute;r&egrave;s, S., &amp; Najid, N. M. (2006).
  <i>Capacitated Multi-Item Lot-Sizing Problems with Time Windows</i>.
  <i>Operations Research</i>, 54(5), 951&ndash;967.</p>
</div>
)---"



ui <- fluidPage(
  titlePanel("Planificacion semanal de insumos: pronostico + CLSP"),
  sidebarLayout(
    sidebarPanel(
      h4("Datos"),
      numericInput("n_clients", "Numero de clientes", 8, min = 2, max = 20),
      numericInput("n_history", "Historia (semanas)", 104, min = 52, max = 260),
      numericInput("seed", "Semilla", 123, min = 1),
      actionButton("generate", "Generar historia", class = "btn-default"),
      helpText("Demo: la historia inicial es sintetica. Puedes cargar datos reales en la pestaña Datos."),

      hr(),
      h4("Pronostico"),
      selectInput("method", "Metodo", choices = c(
        "NNAR (red neuronal)" = "nnar",
        "SARIMA automatico" = "sarima",
        "Holt" = "holt",
        "Suavizado exponencial simple" = "ses",
        "Media movil" = "ma"
      ), selected = "nnar"),
      numericInput("horizon", "Horizonte de pronostico (semanas)", 8, min = 2, max = 26),
      conditionalPanel("input.method == 'nnar'",
        sliderInput("nnar_p", "Rezagos p", min = 1, max = 8, value = 3, step = 1),
        sliderInput("nnar_size", "Neuronas ocultas", min = 2, max = 10, value = 4, step = 1),
        sliderInput("nnar_reps", "Repeticiones", min = 1, max = 10, value = 3, step = 1)
      ),
      numericInput("n_scenarios", "Escenarios simulados", 100, min = 20, max = 500, step = 20),
      actionButton("train", "Entrenar y pronosticar", class = "btn-warning"),

      hr(),
      h4("Optimizacion CLSP"),
      sliderInput("capacity_factor", "Factor de capacidad", min = 1, max = 2, value = 1.25, step = 0.05),
      actionButton("solve", "Resolver modelo entero", class = "btn-primary"),
      br(), br(),
      verbatimTextOutput("status")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen",
          br(),
          h4("Flujo de trabajo"),
          p("1. Generar o cargar historia. 2. Entrenar el pronostico. 3. Simular escenarios. 4. Resolver el plan agregado de produccion."),
          uiOutput("kpis"),
          hr(),
          plotOutput("plot_aggregate", height = "360px")
        ),
        tabPanel("Datos",
          br(),
          p("CSV esperado: cliente, producto, semana, demanda. La columna semana debe ser una fecha reconocible por R."),
          fileInput("csv_file", "Cargar historial CSV", accept = c(".csv")),
          actionButton("use_csv", "Usar CSV cargado"),
          DTOutput("history_table"),
          hr(),
          DTOutput("clients_table")
        ),
        tabPanel("Pronostico por cliente-producto",
          br(),
          fluidRow(
            column(6, selectInput("client_view", "Cliente", choices = NULL)),
            column(6, selectInput("product_view", "Producto", choices = NULL))
          ),
          plotOutput("plot_series", height = "420px"),
          DTOutput("forecast_table")
        ),
        tabPanel("Demanda agregada",
          br(),
          p("Para producir, el CLSP agrega la demanda de todos los clientes por producto y semana futura."),
          plotOutput("plot_aggregate_detail", height = "400px"),
          DTOutput("aggregate_table")
        ),
        tabPanel("Solucion CLSP",
                 br(),
                 uiOutput("solution_kpis"),
                 h4("Produccion X"),
                 DTOutput("production_table"),
                 hr(),
                 h4("Inventario final I"),
                 DTOutput("inventory_table"),
                 hr(),
                 h4("Setups Y"),
                 DTOutput("setup_table"),
                 hr(),
                 # --- NUEVA SECCIÓN ---
                 h4("Asignacion por Cliente (Demanda Satisfecha)"),
                 DTOutput("allocation_table"),
                 hr(),
                 # ---------------------
                 plotOutput("plot_plan", height = "420px")
        ),
        tabPanel("Metricas",
          br(),
          p("La validacion temporal entrena con la parte inicial de cada serie y prueba en el bloque final. No se debe evaluar con datos usados para entrenar."),
          actionButton("validate", "Ejecutar validacion temporal"),
          br(), br(),
          DTOutput("metrics_table")
        ),
        tabPanel("Formulación del Modelo",
                 br(),
                 withMathJax(HTML(modelo_html))
        )
    )
  )
)
)

server <- function(input, output, session) {
  history_rv <- reactiveVal(generate_synthetic_history(
    n_clients = 8, n_weeks = 104, seed = 123
  ))
  forecast_rv <- reactiveVal(NULL)
  aggregate_rv <- reactiveVal(NULL)
  solution_rv <- reactiveVal(NULL)
  metrics_rv <- reactiveVal(NULL)
  status_rv <- reactiveVal("Listo. Genera/carga historia y pulsa Entrenar y pronosticar.")
  
  observeEvent(input$generate, {
    history_rv(generate_synthetic_history(
      n_clients = input$n_clients,
      n_weeks = input$n_history,
      seed = input$seed
    ))
    forecast_rv(NULL)
    aggregate_rv(NULL)
    solution_rv(NULL)
    metrics_rv(NULL)
    status_rv("Historia sintetica generada. Ahora entrena el pronostico.")
  }, ignoreInit = TRUE)
  
  observeEvent(input$use_csv, {
    req(input$csv_file)
    tryCatch({
      raw <- read.csv(input$csv_file$datapath, stringsAsFactors = FALSE)
      history_rv(validate_history(raw))
      forecast_rv(NULL)
      aggregate_rv(NULL)
      solution_rv(NULL)
      metrics_rv(NULL)
      status_rv("CSV cargado y validado.")
    }, error = function(e) {
      status_rv(paste("Error al cargar CSV:", conditionMessage(e)))
    })
  }, ignoreInit = TRUE)
  
  observeEvent(history_rv(), {
    hist <- history_rv()
    updateSelectInput(session, "client_view",
                      choices = sort(unique(hist$cliente)),
                      selected = sort(unique(hist$cliente))[1])
    updateSelectInput(session, "product_view",
                      choices = sort(unique(hist$producto)),
                      selected = sort(unique(hist$producto))[1])
  }, ignoreInit = FALSE)
  
  observeEvent(input$train, {
    hist <- history_rv()
    status_rv("Entrenando modelos y generando escenarios...")
    tryCatch({
      fc <- forecast_all(
        hist, h = input$horizon, method = input$method,
        p = input$nnar_p, size = input$nnar_size, reps = input$nnar_reps,
        n_scenarios = input$n_scenarios, seed = input$seed
      )
      forecast_rv(fc$detail)
      aggregate_rv(fc$aggregate)
      solution_rv(NULL)
      status_rv(paste("Pronostico listo:", nrow(fc$detail),
                      "series cliente-producto-semana."))
    }, error = function(e) {
      status_rv(paste("Error en pronostico:", conditionMessage(e)))
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$solve, {
    req(aggregate_rv())
    status_rv("Resolviendo CLSP con lpSolve...")
    tryCatch({
      sol <- solve_clsp(aggregate_rv(), capacity_factor = input$capacity_factor)
      solution_rv(sol)
      status_rv(if (sol$status == 0) "Optimizacion terminada." else
        paste("El optimizador devolvio estado", sol$status))
    }, error = function(e) {
      status_rv(paste("Error en optimizacion:", conditionMessage(e)))
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$validate, {
    status_rv("Ejecutando validacion temporal...")
    tryCatch({
      metrics_rv(validate_forecasts(history_rv(), method = input$method,
                                    p = input$nnar_p, size = input$nnar_size,
                                    reps = min(input$nnar_reps, 3)))
      status_rv("Validacion terminada.")
    }, error = function(e) {
      status_rv(paste("Error en validacion:", conditionMessage(e)))
    })
  }, ignoreInit = TRUE)
  
  output$status  <- renderText({
    sol <- solution_rv()
    
    if (is.null(sol)) {
      return("Todavía no se ha ejecutado la optimización.")
    }
    
    if (sol$status == 0) {
      paste(
        "Solución óptima encontrada.",
        "Costo:", round(sol$objective, 2)
      )
    } else {
      paste("Estado del solver:", sol$status)
    }
  })
  
  output$history_table <- renderDT({
    datatable(head(history_rv(), 500), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$clients_table <- renderDT({
    hist <- history_rv()
    datatable(unique(hist[, c("cliente", "actividad")]), options = list(pageLength = 10))
  })
  
  output$kpis <- renderUI({
    hist <- history_rv()
    fc <- forecast_rv()
    agg <- aggregate_rv()
    fluidRow(
      column(4, wellPanel(h4("Clientes"), h2(length(unique(hist$cliente))))),
      column(4, wellPanel(h4("Productos"), h2(length(unique(hist$producto))))),
      column(4, wellPanel(h4("Pronostico"), h2(if (is.null(fc)) "Pendiente" else "Listo")))
    )
  })
  
  output$plot_series <- renderPlot({
    hist <- history_rv()
    req(input$client_view, input$product_view)
    past <- hist[hist$cliente == input$client_view &
                   hist$producto == input$product_view, ]
    p <- ggplot(past, aes(x = semana, y = demanda)) +
      geom_line(color = "#2563eb", linewidth = 0.7) +
      labs(title = paste("Historia:", input$client_view, "-", input$product_view),
           x = "Semana", y = "Demanda") + theme_minimal()
    fc <- forecast_rv()
    if (!is.null(fc)) {
      future <- fc[fc$cliente == input$client_view &
                     fc$producto == input$product_view, ]
      if (nrow(future) > 0) {
        p <- p + geom_ribbon(data = future,
                             aes(x = semana, ymin = inferior, ymax = superior),
                             inherit.aes = FALSE, fill = "#93c5fd", alpha = 0.35) +
          geom_line(data = future, aes(x = semana, y = pronostico),
                    inherit.aes = FALSE, color = "#dc2626", linewidth = 0.8)
      }
    }
    p
  })
  
  output$plot_aggregate <- renderPlot({
    agg <- aggregate_rv()
    if (is.null(agg)) {
      hist <- history_rv()
      hist_agg <- aggregate(demanda ~ semana + producto, data = hist, sum)
      ggplot(hist_agg, aes(semana, demanda, color = producto)) +
        geom_line() + theme_minimal() +
        labs(title = "Demanda historica agregada", x = "Semana", y = "Demanda")
    } else {
      ggplot(agg, aes(semana, pronostico, color = producto)) +
        geom_line(linewidth = 0.8) + geom_point(size = 1) +
        theme_minimal() +
        labs(title = "Pronostico agregado por producto", x = "Semana", y = "Demanda")
    }
  })
  
  output$plot_aggregate_detail <- renderPlot({
    req(aggregate_rv())
    ggplot(aggregate_rv(), aes(semana, pronostico, color = producto)) +
      geom_line(linewidth = 0.8) + geom_point(size = 1) +
      theme_minimal() + labs(x = "Semana", y = "Demanda pronosticada")
  })
  
  output$forecast_table <- renderDT({
    fc <- forecast_rv()
    req(fc)
    datatable(fc[fc$cliente == input$client_view &
                   fc$producto == input$product_view, ],
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$aggregate_table <- renderDT({
    req(aggregate_rv())
    datatable(aggregate_rv(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$solution_kpis <- renderUI({
    sol <- solution_rv()
    req(sol)
    if (sol$status != 0) return(tags$p("No se encontro una solucion optima factible."))
    fluidRow(
      column(4, wellPanel(h4("Costo total"), h2(round(sol$objective, 2)))),
      column(4, wellPanel(h4("Semanas"), h2(sol$data$T))),
      column(4, wellPanel(h4("Productos"), h2(sol$data$N)))
    )
  })
  
  output$plot_plan <- renderPlot({
    sol <- solution_rv(); req(sol, sol$status == 0)
    prod <- as.data.frame(as.table(sol$X))
    names(prod) <- c("producto", "periodo", "produccion")
    prod$periodo <- as.integer(gsub("S", "", prod$periodo))
    
    # Gráfico facetado (separado por producto) en lugar de apilado
    ggplot(prod, aes(x = periodo, y = produccion, fill = producto)) +
      geom_col(show.legend = FALSE) + 
      facet_wrap(~ producto, scales = "free_y", ncol = 2) +
      theme_minimal() +
      labs(title = "Plan de Producción por Producto",
           x = "Semana del horizonte", 
           y = "Unidades a producir") +
      theme(strip.text = element_text(size = 12, face = "bold"))
  })
  
  output$production_table <- renderDT({
    sol <- solution_rv(); req(sol, sol$status == 0)
    # Aplicamos round(sol$X, 0) para quitar los decimales en la interfaz
    datatable(data.frame(producto = rownames(sol$X), as.data.frame(round(sol$X, 0)),
                         check.names = FALSE),
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$inventory_table <- renderDT({
    sol <- solution_rv(); req(sol, sol$status == 0)
    # Aplicamos round(sol$I, 0)
    datatable(data.frame(producto = rownames(sol$I), as.data.frame(round(sol$I, 0)),
                         check.names = FALSE),
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$setup_table <- renderDT({
    sol <- solution_rv(); req(sol, sol$status == 0)
    # Aplicamos round(sol$Y, 0) por seguridad, aunque Y ya suele ser entera
    datatable(data.frame(producto = rownames(sol$Y), as.data.frame(round(sol$Y, 0)),
                         check.names = FALSE),
              options = list(pageLength = 10, scrollX = TRUE))
  })
  output$allocation_table <- renderDT({
    sol <- solution_rv(); req(sol, sol$status == 0)
    fc <- forecast_rv(); req(fc)
    
    # Extraemos solo las columnas necesarias
    df <- fc[, c("cliente", "producto", "semana", "pronostico")]
    
    # Redondeamos a enteros
    df$pronostico <- round(pmax(0, df$pronostico))
    
    # Usamos pivot_wider (mucho más estable que reshape)
    df_wide <- pivot_wider(
      df, 
      names_from = semana, 
      values_from = pronostico
    )
    
    # Ordenamos
    df_wide <- df_wide[order(df_wide$cliente, df_wide$producto), ]
    
    datatable(df_wide, rownames = FALSE, 
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$metrics_table <- renderDT({
    met <- metrics_rv()
    req(met)
    datatable(met, options = list(pageLength = 15, scrollX = TRUE))
  })
}

shinyApp(ui, server)
