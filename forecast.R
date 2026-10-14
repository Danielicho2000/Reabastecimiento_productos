# Pronostico por serie cliente-producto y simulacion de escenarios.
# La simulacion usa errores residuales; no es una NN probabilistica.

forecast_one_series <- function(y, h, method = "nnar", p = 3, size = 4, reps = 3) {
  y <- as.numeric(y)
  y[!is.finite(y)] <- NA_real_
  if (anyNA(y)) stop("La serie contiene valores faltantes; completalos antes de entrenar.")
  if (length(y) < 12) stop("Cada serie necesita al menos 12 observaciones.")
  y <- pmax(y, 0)

  fit <- NULL
  point <- NULL
  residuals_vec <- numeric(0)

  if (method == "nnar") {
    fit <- tryCatch(
      forecast::nnetar(
        stats::ts(y, frequency = 52),
        p = min(p, max(1, floor(length(y) / 5))),
        P = 0,
        size = size,
        repeats = reps,
        scale.inputs = TRUE
      ),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      f <- forecast::forecast(fit, h = h)
      point <- as.numeric(f$mean)
      residuals_vec <- as.numeric(stats::residuals(fit))
    }
  } else if (method == "sarima") {
    fit <- tryCatch(
      forecast::auto.arima(stats::ts(y, frequency = 52),
                           seasonal = FALSE, stepwise = TRUE,
                           approximation = TRUE),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      f <- forecast::forecast(fit, h = h)
      point <- as.numeric(f$mean)
      residuals_vec <- as.numeric(stats::residuals(fit))
    }
  } else if (method == "holt") {
    fit <- forecast::holt(stats::ts(y), h = h)
    point <- as.numeric(fit$mean)
    residuals_vec <- as.numeric(stats::residuals(fit))
  } else if (method == "ses") {
    fit <- forecast::ses(stats::ts(y), h = h)
    point <- as.numeric(fit$mean)
    residuals_vec <- as.numeric(stats::residuals(fit))
  } else if (method == "ma") {
    k <- min(4, max(2, floor(length(y) / 4)))
    point <- rep(mean(tail(y, k)), h)
    residuals_vec <- y[(k + 1):length(y)] -
      vapply((k + 1):length(y), function(j) mean(y[(j-k):(j-1)]), numeric(1))
  } else {
    stop("Metodo de pronostico desconocido: ", method)
  }

  # Fallback estable cuando NNAR/SARIMA no ajusta por alguna serie.
  if (is.null(point) || any(!is.finite(point))) {
    point <- rep(mean(tail(y, min(8, length(y)))), h)
    residuals_vec <- diff(y)
  }

  residuals_vec <- residuals_vec[is.finite(residuals_vec)]
  residual_sd <- if (length(residuals_vec) >= 2) stats::sd(residuals_vec) else stats::sd(diff(y))
  if (!is.finite(residual_sd)) residual_sd <- 0
  list(point = pmax(as.numeric(point), 0),
       residuals = residuals_vec,
       sigma = residual_sd,
       fitted = if (!is.null(fit)) as.numeric(fit$fitted) else rep(NA_real_, length(y)))
}

forecast_all <- function(history, h = 8, method = "nnar", p = 3, size = 4,
                         reps = 3, n_scenarios = 100, seed = 123) {
  history <- validate_history(history)
  clients <- unique(history$cliente)
  products <- unique(history$producto)
  last_date <- max(history$semana)
  future_dates <- last_date + 7 * seq_len(h)
  detail_rows <- list()
  scenario_rows <- list()
  series_idx <- 1L
  scenario_idx <- 1L
  set.seed(seed)

  for (cli in clients) {
    for (prod in products) {
      s <- history[history$cliente == cli & history$producto == prod, ]
      s <- s[order(s$semana), ]
      if (nrow(s) == 0) next
      # No se fabrican ceros silenciosamente: cada combinacion debe estar completa.
      if (nrow(s) != length(unique(history$semana))) {
        stop("Historia incompleta para ", cli, " / ", prod,
             ". Completa las semanas antes de entrenar.")
      }
      fit <- forecast_one_series(s$demanda, h, method, p, size, reps)
      sig <- fit$sigma
      # Bootstrap residual simple alrededor del pronostico.
      if (length(fit$residuals) >= 5) {
        err <- sample(fit$residuals, size = n_scenarios * h, replace = TRUE)
      } else {
        err <- rnorm(n_scenarios * h, mean = 0, sd = sig)
      }
      sim <- matrix(pmax(rep(fit$point, each = n_scenarios) +
                           matrix(err, nrow = n_scenarios, ncol = h), 0),
                    nrow = n_scenarios, ncol = h)

      detail_rows[[series_idx]] <- data.frame(
        cliente = cli, producto = prod, semana = future_dates,
        pronostico = fit$point,
        inferior = apply(sim, 2, stats::quantile, probs = 0.05, na.rm = TRUE),
        superior = apply(sim, 2, stats::quantile, probs = 0.95, na.rm = TRUE),
        sigma_residual = sig,
        metodo = method,
        stringsAsFactors = FALSE
      )
      for (b in seq_len(n_scenarios)) {
        scenario_rows[[scenario_idx]] <- data.frame(
          cliente = cli, producto = prod, escenario = b,
          semana = future_dates, demanda_simulada = sim[b, ],
          stringsAsFactors = FALSE
        )
        scenario_idx <- scenario_idx + 1L
      }
      series_idx <- series_idx + 1L
    }
  }

  detail <- do.call(rbind, detail_rows)
  scenarios <- do.call(rbind, scenario_rows)
  aggregate <- stats::aggregate(
    pronostico ~ producto + semana, data = detail, FUN = sum
  )
  names(aggregate)[names(aggregate) == "pronostico"] <- "pronostico"

  # Intervalos agregados derivados de escenarios conjuntos.
  agg_scen <- stats::aggregate(
    demanda_simulada ~ producto + escenario + semana,
    data = scenarios, FUN = sum
  )
  agg_low <- stats::aggregate(
    demanda_simulada ~ producto + semana, data = agg_scen,
    FUN = function(z) unname(stats::quantile(z, .05, na.rm = TRUE))
  )
  agg_up <- stats::aggregate(
    demanda_simulada ~ producto + semana, data = agg_scen,
    FUN = function(z) unname(stats::quantile(z, .95, na.rm = TRUE))
  )
  names(agg_low)[3] <- "inferior"
  names(agg_up)[3] <- "superior"
  aggregate <- merge(aggregate, agg_low, by = c("producto", "semana"))
  aggregate <- merge(aggregate, agg_up, by = c("producto", "semana"))

  list(detail = detail, aggregate = aggregate, scenarios = scenarios)
}

validate_forecasts <- function(history, method = "nnar", p = 3, size = 4, reps = 3,
                               test_weeks = 8) {
  history <- validate_history(history)
  results <- list()
  idx <- 1L
  for (cli in unique(history$cliente)) {
    for (prod in unique(history$producto)) {
      s <- history[history$cliente == cli & history$producto == prod, ]
      s <- s[order(s$semana), ]
      if (nrow(s) <= test_weeks + 12) next
      train <- head(s, nrow(s) - test_weeks)
      test <- tail(s, test_weeks)
      f <- forecast_one_series(train$demanda, nrow(test), method, p, size, reps)
      err <- test$demanda - f$point
      results[[idx]] <- data.frame(
        cliente = cli, producto = prod, metodo = method,
        MAE = mean(abs(err)), RMSE = sqrt(mean(err^2)),
        sesgo = mean(err), n_test = length(err),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  if (length(results) == 0) return(data.frame())
  do.call(rbind, results)
}
