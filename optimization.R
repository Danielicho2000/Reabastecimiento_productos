# Modelo de lot-sizing capacitado (CLSP) con setup binario y costos unitarios reales para los 100 SKUs.

solve_clsp <- function(aggregate_forecast, capacity_factor = 1.25) {
  required <- c("producto", "semana", "pronostico")
  if (!all(required %in% names(aggregate_forecast))) {
    stop("El pronostico agregado debe tener producto, semana y pronostico.")
  }
  products <- unique(aggregate_forecast$producto)
  dates <- sort(unique(aggregate_forecast$semana))
  N <- length(products)
  T <- length(dates)
  if (N < 1 || T < 1) stop("No hay demanda para optimizar.")
  
  D <- matrix(0, nrow = N, ncol = T, dimnames = list(products, paste0("S", seq_len(T))))
  for (i in seq_len(N)) {
    for (t in seq_len(T)) {
      row <- aggregate_forecast[aggregate_forecast$producto == products[i] & aggregate_forecast$semana == dates[t], ]
      if (nrow(row) != 1) stop("Falta o se duplica demanda agregada.")
      D[i, t] <- round(max(0, row$pronostico[1]))
    }
  }
  
  # =====================================================================
  # DICCIONARIO COMPLETO DE COSTOS UNITARIOS PARA LOS 100 PRODUCTOS (USD)
  # =====================================================================
  costos_unitarios <- c(
    "Cloro Tradicional 1 Galón" = 2.30,
    "Cloro Industrial Bidón 10L" = 12.00,
    "Desinfectante Multiuso Aroma Lavanda 5L" = 6.50,
    "Desinfectante Multiuso Aroma Pino 5L" = 6.50,
    "Amonio Cuaternario 5ta Generación 1L" = 5.80,
    "Amonio Cuaternario Bidón 20L" = 22.00,
    "Alcohol Antiséptico 70% Galón" = 11.50,
    "Alcohol en Gel Antibacterial 1L" = 3.50,
    "Peróxido de Hidrógeno Industrial 5L" = 14.00,
    "Sanitizador de Superficies de Acero 1L" = 6.80,
    "Limpiador Multiuso con Blanqueador 5L" = 7.50,
    "Pastillas de Cloro Efervescentes (Frasco x100)" = 9.20,
    "Desinfectante en Spray Aerosol 400ml" = 4.20,
    "Elimina Olores Enzimático 1L" = 4.50,
    "Vinagre de Limpieza Concentrado 5L" = 5.50,
    "Detergente Líquido Ropa Industrial 20L" = 28.00,
    "Detergente en Polvo Profesional 10Kg" = 18.50,
    "Jabón Líquido para Manos Aroma Floral 4L" = 8.50,
    "Jabón Líquido Antibacterial para Manos 5L" = 9.80,
    "Lavaplatos Líquido Concentrado Limón 5L" = 6.20,
    "Suavizante de Ropa Aroma Primavera 5L" = 7.00,
    "Quitamanchas Enzimático Ropa 1L" = 5.20,
    "Champú para Alfombras y Tapiz 5L" = 11.00,
    "Lavavajillas en Polvo Industrial 5Kg" = 15.00,
    "Jabón en Barra Multiuso (Caja x24)" = 12.50,
    "Desengrasante de Cocina Pesado 5L" = 10.50,
    "Desengrasante Biodegradable Multiuso 1L" = 4.80,
    "Limpiador de Vidrios y Espejos 5L" = 7.20,
    "Quitaesmalte e Impurezas Industriales 1L" = 6.00,
    "Limpiador de Hornos y Parrillas en Gel 1L" = 6.50,
    "Escoba de Cerdas Suaves con Bastón" = 3.50,
    "Escoba de Cerdas Duras para exteriores" = 4.00,
    "Trapeador / Mopa de Algodón Profesional" = 4.50,
    "Repuesto de Mopa de Microfibra" = 2.80,
    "Balde Escurridor de Plástico con Ruedas 20L" = 18.00,
    "Recogedor de Basura Metálico con Bastón" = 5.00,
    "Paños de Microfibra Multiuso (Pack x10)" = 6.50,
    "Esponja Doble Uso (Esponja/Fibra) x10" = 1.80,
    "Fibras Verdes de Limpieza Pesada (Pack x5)" = 2.20,
    "Virutilla de Acero Fina (Paquete x12)" = 2.50,
    "Cepillo Lava Inodoros con Base" = 3.20,
    "Cepillo de Mano para Fregar Cerdas Duras" = 2.00,
    "Limpiador de Vidrios con Rascador y Esponja" = 5.50,
    "Guantes de Látex Industrial Talla M (Par)" = 1.50,
    "Guantes de Látex Industrial Talla L (Par)" = 1.50,
    "Guantes de Nitrilo Azules (Caja x100)" = 7.50,
    "Atomizador / Spray Gatillo Vacío 500ml" = 1.20,
    "Atomizador / Spray Gatillo Vacío 1000ml" = 1.50,
    "Embudo de Plástico Mediano para Líquidos" = 0.90,
    "Carro de Limpieza Institucional con Bolsas" = 85.00,
    "Papel Higiénico Institucional Jumbo (Fardo x6)" = 18.00,
    "Papel Higiénico Hoja Doble (Paquete x48)" = 14.50,
    "Toallas de Papel Interfoliadas (Fardo x2000)" = 16.00,
    "Toallas de Papel en Rollo para Dispensador (Pack x6)" = 8.50,
    "Servilletas de Papel Dispensador (Pack x500)" = 4.50,
    "Bolsas de Basura Negras 55x70 cm (Paquete x50)" = 3.80,
    "Bolsas de Basura Negras 90x120 cm (Paquete x25)" = 4.80,
    "Bolsas de Basura Industriales Extra Grandes (x10)" = 6.20,
    "Bolsas de Basura Transparentes Reciclaje 55x70" = 4.00,
    "Cofias / Gorros Descartables (Caja x100)" = 4.20,
    "Mascarillas Quirúrgicas Descartables (Caja x50)" = 3.50,
    "Baberos / Mandiles Descartables Plásticos (x50)" = 5.50,
    "Cubrezapatos Descartables de Polietileno (x100)" = 4.80,
    "Cera Acrílica Auto brillo para Pisos 5L" = 12.50,
    "Sellador de Pisos Porosos y Madera 5L" = 14.00,
    "Quitamanchas y Decapante de Cera 5L" = 13.00,
    "Limpiador Aromático para Pisos Flotantes 5L" = 7.00,
    "Detergente Neutro Concentrado para Pisos 5L" = 6.50,
    "Champú para Limpieza de Pisos con Máquina 5L" = 11.50,
    "Antideslizante para Pisos de Cerámica 1L" = 8.00,
    "Limpiador y Lustrador de Muebles en Spray 400ml" = 4.50,
    "Crema Limpiadora para Cuero y Vinil 1L" = 5.80,
    "Aceite de Teca para Muebles de Madera 1L" = 9.50,
    "Limpiador de Pantallas y Electrónica 250ml" = 3.80,
    "Ácido Muriático Industrial Limpieza 1L" = 2.50,
    "Gel Desincrustante para Inodoros 1L" = 4.20,
    "Pastillas Ambientadoras para Mingitorios (Caja)" = 5.00,
    "Ambientador Líquido Aroma Manzana 5L" = 8.00,
    "Ambientador Líquido Aroma Lavanda 5L" = 8.00,
    "Ambientador en Spray Automático Recambio 250ml" = 4.00,
    "Destapa Cañerías Líquido Concentrado 1L" = 4.50,
    "Destapa Cañerías en Gel Alta Visibilidad 1L" = 4.80,
    "Quitazarro y Antisarro Profesional 5L" = 11.00,
    "Blanqueador Óptico Textil 5L" = 9.00,
    "Neutralizador de Olores Mascotas 5L" = 10.00,
    "Mopa Seca atrapapolvo 60cm con estructura" = 16.00,
    "Repuesto Mopa Seca atrapapolvo 60cm" = 7.50,
    "Señalética de Piso Húmedo \"Precaución\"" = 6.00,
    "Pala Plástica Basurero con Tapa Basculante" = 12.00,
    "Dispensador Manual de Jabón Líquido 1L" = 9.50,
    "Dispensador de Papel Higiénico Jumbo" = 14.00,
    "Dispensador de Toallas Interfoliadas" = 15.00,
    "Limpiador de Alfombras en Seco Polvo 1Kg" = 8.50,
    "Quitamanchas de Tinta y Grasa Textil 500ml" = 6.20,
    "Limpiador de Inoxidables y Metales 1L" = 7.00,
    "Gel Antisépticomulti superficies 5L" = 13.50,
    "Paños Descartables faciales (Caja x100)" = 2.20,
    "Rollos de Cocina Absorbentes Industriales (x2)" = 3.50,
    "Hidrolavadora Portátil de Presión Pequeña" = 120.00,
    "Aspiradora de Polvo y Agua Industrial 30L" = 180.00
  )
  
  production_cost <- matrix(0, nrow = N, ncol = T, dimnames = dimnames(D))
  holding_cost <- matrix(0, nrow = N, ncol = T, dimnames = dimnames(D))
  setup_cost <- matrix(35, nrow = N, ncol = T, dimnames = dimnames(D)) # Setup estándar de $35
  resource_use <- rep(1, N)
  names(resource_use) <- products
  
  # Asignamos el costo exacto a cada fila de la matriz
  for (i in seq_len(N)) {
    nombre_prod <- products[i]
    # Si el producto está en el diccionario, usa su costo. Si es desconocido, asume $5.00 por seguridad
    costo_prod <- ifelse(nombre_prod %in% names(costos_unitarios), costos_unitarios[nombre_prod], 5.00)
    
    production_cost[i, ] <- costo_prod
    holding_cost[i, ] <- costo_prod * 0.15 # El costo de almacenamiento es el 15% del costo de fabricación
  }
  
  weekly_load <- as.numeric(crossprod(resource_use, D))
  capacity <- pmax(1, ceiling(weekly_load * capacity_factor))
  
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
  
  n_constraints <- N * T + N * T + T
  mat <- matrix(0, nrow = n_constraints, ncol = nvar)
  rhs <- numeric(n_constraints)
  dir <- character(n_constraints)
  row <- 0L
  big_m <- max(1, sum(D))
  
  for (i in seq_len(N)) {
    for (t in seq_len(T)) {
      row <- row + 1L
      mat[row, idx_x(i, t)] <- 1; mat[row, idx_i(i, t)] <- -1
      if (t > 1) mat[row, idx_i(i, t - 1)] <- 1
      rhs[row] <- D[i, t]; dir[row] <- "="
    }
  }
  for (i in seq_len(N)) {
    for (t in seq_len(T)) {
      row <- row + 1L
      mat[row, idx_x(i, t)] <- 1; mat[row, idx_y(i, t)] <- -big_m
      rhs[row] <- 0; dir[row] <- "<="
    }
  }
  for (t in seq_len(T)) {
    row <- row + 1L
    for (i in seq_len(N)) mat[row, idx_x(i, t)] <- resource_use[i]
    rhs[row] <- capacity[t]; dir[row] <- "<="
  }
  
  result <- lpSolve::lp(
    direction = "min", objective.in = objective,
    const.mat = mat, const.dir = dir, const.rhs = rhs,
    binary.vec = (n_x + n_i + 1):nvar
  )
  
  if (result$status != 0) {
    return(list(status = result$status, objective = NA_real_, X = NULL, I = NULL, Y = NULL, data = list(D = D, capacity = capacity, dates = dates)))
  }
  
  X <- matrix(result$solution[seq_len(n_x)], nrow = N, ncol = T, dimnames = dimnames(D))
  I <- matrix(result$solution[n_x + seq_len(n_i)], nrow = N, ncol = T, dimnames = dimnames(D))
  Y <- matrix(round(result$solution[n_x + n_i + seq_len(n_y)]), nrow = N, ncol = T, dimnames = dimnames(D))
  
  list(status = result$status, objective = result$objval, X = X, I = I, Y = Y,
       data = list(D = D, capacity = capacity, dates = dates,
                   production_cost = production_cost, holding_cost = holding_cost, setup_cost = setup_cost, resource_use = resource_use,
                   costos_unitarios = costos_unitarios))
}