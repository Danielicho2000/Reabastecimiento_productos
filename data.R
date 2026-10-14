# Datos historicos sinteticos y validacion del formato de entrada.

PRODUCTS <- c(
  "Jabon_liquido", "Detergente", "Desinfectante",
  "Papel_higienico", "Toallas_papel", "Bolsas_basura"
)

ACTIVITY_PROFILES <- list(
  policia = list(base = c(20, 10, 30, 25, 20, 15),
                 amp = c(.15, .10, .20, .15, .10, .10), phase = .75),
  hospital = list(base = c(80, 60, 120, 150, 100, 90),
                  amp = c(.10, .10, .15, .12, .08, .10), phase = .10),
  centro_comercial = list(base = c(60, 40, 50, 90, 80, 70),
                          amp = c(.25, .20, .20, .35, .30, .30), phase = .90),
  club_deportivo = list(base = c(40, 15, 25, 30, 35, 20),
                        amp = c(.30, .20, .30, .25, .30, .20), phase = .30),
  colegio = list(base = c(70, 30, 40, 90, 80, 50),
                 amp = c(.40, .35, .35, .45, .40, .35), phase = .55),
  restaurante = list(base = c(30, 45, 35, 25, 30, 40),
                     amp = c(.15, .20, .15, .20, .20, .20), phase = .85),
  oficina = list(base = c(20, 10, 15, 40, 25, 15),
                 amp = c(.05, .05, .05, .08, .06, .05), phase = .50),
  industria = list(base = c(50, 80, 60, 20, 30, 70),
                   amp = c(.10, .15, .10, .10, .10, .15), phase = .50)
)

build_clients <- function(n_clients = 8, seed = 123) {
  set.seed(seed)
  types <- names(ACTIVITY_PROFILES)
  chosen <- sample(types, n_clients, replace = n_clients > length(types))
  data.frame(
    cliente = sprintf("C%02d", seq_len(n_clients)),
    actividad = chosen,
    stringsAsFactors = FALSE
  )
}

generate_synthetic_history <- function(n_clients = 8, n_weeks = 104, seed = 123) {
  clients <- build_clients(n_clients, seed)
  set.seed(seed + 2000)
  start_date <- as.Date("2022-01-03")
  weeks <- start_date + 7 * (seq_len(n_weeks) - 1)
  rows <- vector("list", n_clients * length(PRODUCTS))
  idx <- 1L

  for (k in seq_len(n_clients)) {
    prof <- ACTIVITY_PROFILES[[clients$actividad[k]]]
    trend <- runif(1, -0.02, 0.08)
    for (i in seq_along(PRODUCTS)) {
      base <- prof$base[i]
      amp <- prof$amp[i]
      vals <- numeric(n_weeks)
      for (t in seq_len(n_weeks)) {
        seasonal <- amp * sin(2 * pi * (t / 52 + prof$phase))
        noise <- rnorm(1, mean = 0, sd = base * 0.10)
        vals[t] <- max(0, round(base * (1 + seasonal) *
                                  (1 + trend * t / 52) + noise))
      }
      rows[[idx]] <- data.frame(
        cliente = clients$cliente[k],
        actividad = clients$actividad[k],
        producto = PRODUCTS[i],
        semana = weeks,
        demanda = vals,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  do.call(rbind, rows)
}

validate_history <- function(data) {
  required <- c("cliente", "producto", "semana", "demanda")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Faltan columnas obligatorias: ", paste(missing_cols, collapse = ", "))
  }
  if (!("actividad" %in% names(data))) data$actividad <- "desconocida"

  data$cliente <- as.character(data$cliente)
  data$producto <- as.character(data$producto)
  data$semana <- as.Date(data$semana)
  data$demanda <- suppressWarnings(as.numeric(data$demanda))

  if (anyNA(data$semana)) stop("La columna semana debe contener fechas validas (YYYY-MM-DD).")
  if (anyNA(data$demanda) || any(!is.finite(data$demanda))) {
    stop("La demanda debe ser numerica y no puede contener NA/Inf.")
  }
  if (any(data$demanda < 0)) stop("La demanda no puede ser negativa.")
  if (anyDuplicated(data[c("cliente", "producto", "semana")])) {
    stop("Hay filas duplicadas para cliente-producto-semana.")
  }

  data <- data[order(data$cliente, data$producto, data$semana), ]
  rownames(data) <- NULL
  data
}
