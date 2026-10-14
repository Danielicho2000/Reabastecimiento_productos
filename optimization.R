# Modelo de lot-sizing capacitado (CLSP) con setup binario.
# La demanda se agrega por producto-semana a partir del forecast cliente-producto.
# Nota: esta version es CLSP base; no modela ventanas release/due de CLSP-TW.

solve_clsp <- function(aggregate_forecast, capacity_factor = 1.25) {
  required <- c("producto", "semana", "pronostico")
  if (!all(required %in% names(aggregate_forecast))) {
    stop("El pronostico agregado debe tener producto, semana y pronostico.")
  }
  products <- unique(aggregate_forecast$producto)
  dates <- sort(unique(aggregate_forecast$semana))
  N <- length(products)
  T <- length(dates)
  if (N < 1 || T < 1) stop("No hay demanda pronosticada para optimizar.")

  D <- matrix(0, nrow = N, ncol = T,
              dimnames = list(products, paste0("S", seq_len(T))))
  for (i in seq_len(N)) {
    for (t in seq_len(T)) {
      row <- aggregate_forecast[
        aggregate_forecast$producto == products[i] &
          aggregate_forecast$semana == dates[t], ]
      if (nrow(row) != 1) stop("Falta o se duplica demanda agregada.")
      D[i, t] <- round(max(0, row$pronostico[1]))
    }
  }

  # Parametros de ejemplo: sustituir por costos/capacidades reales.
  production_cost <- matrix(12, N, T, dimnames = dimnames(D))
  holding_cost <- matrix(1.5, N, T, dimnames = dimnames(D))
  setup_cost <- matrix(35, N, T, dimnames = dimnames(D))
  resource_use <- rep(1, N)
  names(resource_use) <- products
  weekly_load <- as.numeric(crossprod(resource_use, D))
  capacity <- pmax(1, ceiling(weekly_load * capacity_factor))

  # Variables por producto y semana:
  # X: produccion, I: inventario final, Y: binaria de setup.
  n_x <- N * T
  n_i <- N * T
  n_y <- N * T
  nvar <- n_x + n_i + n_y
  idx_x <- function(i, t) (t - 1) * N + i
  idx_i <- function(i, t) n_x + (t - 1) * N + i
  idx_y <- function(i, t) n_x + n_i + (t - 1) * N + i

  objective <- numeric(nvar)
  for (i in seq_len(N)) for (t in seq_len(T)) {
    objective[idx_x(i, t)] <- production_cost[i, t]
    objective[idx_i(i, t)] <- holding_cost[i, t]
    objective[idx_y(i, t)] <- setup_cost[i, t]
  }

  # Restricciones: balance, enlace setup y capacidad.
  n_constraints <- N * T + N * T + T
  mat <- matrix(0, nrow = n_constraints, ncol = nvar)
  rhs <- numeric(n_constraints)
  dir <- character(n_constraints)
  row <- 0L
  big_m <- max(1, sum(D))

  for (i in seq_len(N)) {
    for (t in seq_len(T)) {
      row <- row + 1L
      mat[row, idx_x(i, t)] <- 1
      mat[row, idx_i(i, t)] <- -1
      if (t > 1) mat[row, idx_i(i, t - 1)] <- 1
      rhs[row] <- D[i, t]
      dir[row] <- "="
    }
  }
  for (i in seq_len(N)) {
    for (t in seq_len(T)) {
      row <- row + 1L
      mat[row, idx_x(i, t)] <- 1
      mat[row, idx_y(i, t)] <- -big_m
      rhs[row] <- 0
      dir[row] <- "<="
    }
  }
  for (t in seq_len(T)) {
    row <- row + 1L
    for (i in seq_len(N)) mat[row, idx_x(i, t)] <- resource_use[i]
    rhs[row] <- capacity[t]
    dir[row] <- "<="
  }
#USO DEL MÉTODO BRANCH ABD BOUND 
  result <- lpSolve::lp(
    direction = "min", objective.in = objective,
    const.mat = mat, const.dir = dir, const.rhs = rhs,
    binary.vec = (n_x + n_i + 1):nvar
  )
  message("Estado del solver: ", result$status)
  message("Valor objetivo: ", result$objval)
  message("Mensaje: ", result$message)

  if (result$status != 0) {
    return(list(status = result$status, objective = NA_real_,
                X = NULL, I = NULL, Y = NULL,
                data = list(D = D, capacity = capacity, dates = dates)))
  }

  X <- matrix(result$solution[seq_len(n_x)], nrow = N, ncol = T,
              dimnames = dimnames(D))
  I <- matrix(result$solution[n_x + seq_len(n_i)], nrow = N, ncol = T,
              dimnames = dimnames(D))
  Y <- matrix(round(result$solution[n_x + n_i + seq_len(n_y)]),
              nrow = N, ncol = T, dimnames = dimnames(D))

  list(status = result$status, objective = result$objval, X = X, I = I, Y = Y,
       data = list(D = D, capacity = capacity, 
                   weeks = seq_len(T), 
                   production_cost = production_cost,
                   holding_cost = holding_cost, setup_cost = setup_cost,
                   resource_use = resource_use))
}


