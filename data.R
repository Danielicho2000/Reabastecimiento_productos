# Datos historicos sinteticos y validacion del formato de entrada.

PRODUCTS <- c(
  "Cloro Tradicional 1 Galón", "Cloro Industrial Bidón 10L", "Desinfectante Multiuso Aroma Lavanda 5L",
  "Desinfectante Multiuso Aroma Pino 5L", "Amonio Cuaternario 5ta Generación 1L", "Amonio Cuaternario Bidón 20L",
  "Alcohol Antiséptico 70% Galón", "Alcohol en Gel Antibacterial 1L", "Peróxido de Hidrógeno Industrial 5L",
  "Sanitizador de Superficies de Acero 1L", "Limpiador Multiuso con Blanqueador 5L", "Pastillas de Cloro Efervescentes (Frasco x100)",
  "Desinfectante en Spray Aerosol 400ml", "Elimina Olores Enzimático 1L", "Vinagre de Limpieza Concentrado 5L",
  "Detergente Líquido Ropa Industrial 20L", "Detergente en Polvo Profesional 10Kg", "Jabón Líquido para Manos Aroma Floral 4L",
  "Jabón Líquido Antibacterial para Manos 5L", "Lavaplatos Líquido Concentrado Limón 5L", "Suavizante de Ropa Aroma Primavera 5L",
  "Quitamanchas Enzimático Ropa 1L", "Champú para Alfombras y Tapiz 5L", "Lavavajillas en Polvo Industrial 5Kg",
  "Jabón en Barra Multiuso (Caja x24)", "Desengrasante de Cocina Pesado 5L", "Desengrasante Biodegradable Multiuso 1L",
  "Limpiador de Vidrios y Espejos 5L", "Quitaesmalte e Impurezas Industriales 1L", "Limpiador de Hornos y Parrillas en Gel 1L",
  "Escoba de Cerdas Suaves con Bastón", "Escoba de Cerdas Duras para exteriores", "Trapeador / Mopa de Algodón Profesional",
  "Repuesto de Mopa de Microfibra", "Balde Escurridor de Plástico con Ruedas 20L", "Recogedor de Basura Metálico con Bastón",
  "Paños de Microfibra Multiuso (Pack x10)", "Esponja Doble Uso (Esponja/Fibra) x10", "Fibras Verdes de Limpieza Pesada (Pack x5)",
  "Virutilla de Acero Fina (Paquete x12)", "Cepillo Lava Inodoros con Base", "Cepillo de Mano para Fregar Cerdas Duras",
  "Limpiador de Vidrios con Rascador y Esponja", "Guantes de Látex Industrial Talla M (Par)", "Guantes de Látex Industrial Talla L (Par)",
  "Guantes de Nitrilo Azules (Caja x100)", "Atomizador / Spray Gatillo Vacío 500ml", "Atomizador / Spray Gatillo Vacío 1000ml",
  "Embudo de Plástico Mediano para Líquidos", "Carro de Limpieza Institucional con Bolsas", "Papel Higiénico Institucional Jumbo (Fardo x6)",
  "Papel Higiénico Hoja Doble (Paquete x48)", "Toallas de Papel Interfoliadas (Fardo x2000)", "Toallas de Papel en Rollo para Dispensador (Pack x6)",
  "Servilletas de Papel Dispensador (Pack x500)", "Bolsas de Basura Negras 55x70 cm (Paquete x50)", "Bolsas de Basura Negras 90x120 cm (Paquete x25)",
  "Bolsas de Basura Industriales Extra Grandes (x10)", "Bolsas de Basura Transparentes Reciclaje 55x70", "Cofias / Gorros Descartables (Caja x100)",
  "Mascarillas Quirúrgicas Descartables (Caja x50)", "Baberos / Mandiles Descartables Plásticos (x50)", "Cubrezapatos Descartables de Polietileno (x100)",
  "Cera Acrílica Auto brillo para Pisos 5L", "Sellador de Pisos Porosos y Madera 5L", "Quitamanchas y Decapante de Cera 5L",
  "Limpiador Aromático para Pisos Flotantes 5L", "Detergente Neutro Concentrado para Pisos 5L", "Champú para Limpieza de Pisos con Máquina 5L",
  "Antideslizante para Pisos de Cerámica 1L", "Limpiador y Lustrador de Muebles en Spray 400ml", "Crema Limpiadora para Cuero y Vinil 1L",
  "Aceite de Teca para Muebles de Madera 1L", "Limpiador de Pantallas y Electrónica 250ml", "Ácido Muriático Industrial Limpieza 1L",
  "Gel Desincrustante para Inodoros 1L", "Pastillas Ambientadoras para Mingitorios (Caja)", "Ambientador Líquido Aroma Manzana 5L",
  "Ambientador Líquido Aroma Lavanda 5L", "Ambientador en Spray Automático Recambio 250ml", "Destapa Cañerías Líquido Concentrado 1L",
  "Destapa Cañerías en Gel Alta Visibilidad 1L", "Quitazarro y Antisarro Profesional 5L", "Blanqueador Óptico Textil 5L",
  "Neutralizador de Olores Mascotas 5L", "Mopa Seca atrapapolvo 60cm con estructura", "Repuesto Mopa Seca atrapapolvo 60cm",
  "Señalética de Piso Húmedo \"Precaución\"", "Pala Plástica Basurero con Tapa Basculante", "Dispensador Manual de Jabón Líquido 1L",
  "Dispensador de Papel Higiénico Jumbo", "Dispensador de Toallas Interfoliadas", "Limpiador de Alfombras en Seco Polvo 1Kg",
  "Quitamanchas de Tinta y Grasa Textil 500ml", "Limpiador de Inoxidables y Metales 1L", "Gel Antisépticomulti superficies 5L",
  "Paños Descartables faciales (Caja x100)", "Rollos de Cocina Absorbentes Industriales (x2)", "Hidrolavadora Portátil de Presión Pequeña",
  "Aspiradora de Polvo y Agua Industrial 30L"
)

CLIENTS <- c(
  "CC. EL BOSQUE - LOCAL 45", "HOSPITAL DEL DÍA CENTRAL-QUITO", "EDIFICIO SKY - PLANTA BAJA",
  "RESIDENCIA LOS OLIVOS", "CENTRO LOGÍSTICO SUR", "CC SAN LUIS", "CC SKALA", "CC. EL RECREO",
  "MARIA AUXILIADORA", "CCNU", "CCI", "IESS QUITO SUR", "IESS QUITO NORTE", "MONTERO",
  "CC PURUHA SHOPPING", "CORAL 6 DE DICIEMBRE", "CORAL TUMBACO", "CC VISTA GARDEN", "CC RIVER MALL",
  "SUPERMAXI SAN GABRIEL", "HOSPITAL DEL DIA"
)

# Perfiles sectoriales para estructurar el comportamiento del pronóstico e inventario
ACTIVITY_PROFILES <- list(
  hospital = list(amp_base = 0.15, factor_bodega = 1.8),
  centro_comercial = list(amp_base = 0.30, factor_bodega = 2.0),
  oficina = list(amp_base = 0.08, factor_bodega = 1.0),
  industria = list(amp_base = 0.12, factor_bodega = 1.5),
  general = list(amp_base = 0.20, factor_bodega = 1.2)
)

asignar_perfil <- function(nombre_cliente) {
  nombre <- tolower(nombre_cliente)
  if (grepl("hospital|clinica|salud|medic|iess", nombre)) return("hospital")
  if (grepl("cc\\.|centro comercial|mall|plaza|shop|coral|supermaxi", nombre)) return("centro_comercial")
  if (grepl("edificio|oficina|torre|sky|baja|residencia", nombre)) return("oficina")
  if (grepl("industrial|sur|logistico|fabrica", nombre)) return("industria")
  return("general")
}

generate_synthetic_history <- function(n_clients = NULL, n_weeks = 93, seed = 123) {
  set.seed(seed + 2000)
  start_date <- as.Date("2025-01-01")
  weeks <- start_date + 7 * (seq_len(n_weeks) - 1)
  
  # Si no se especifica un número menor, incluye a TODOS los clientes del vector CLIENTS
  actual_clients <- if (is.null(n_clients)) CLIENTS else CLIENTS[1:min(n_clients, length(CLIENTS))]
  
  rows <- vector("list", length(actual_clients) * length(PRODUCTS))
  idx <- 1L
  
  for (cli in actual_clients) {
    perfil_tipo <- asignar_perfil(cli)
    prof <- ACTIVITY_PROFILES[[perfil_tipo]]
    trend <- runif(1, -0.02, 0.08)
    
    for (prod in PRODUCTS) {
      base <- runif(1, 10, 50)
      amp <- prof$amp_base
      phase <- runif(1, 0, 1)
      
      vals <- numeric(n_weeks)
      for (t in seq_len(n_weeks)) {
        seasonal <- amp * sin(2 * pi * (t / 52 + phase))
        noise <- rnorm(1, mean = 0, sd = base * 0.10)
        vals[t] <- max(0, round(base * (1 + seasonal) * (1 + trend * t / 52) + noise))
      }
      rows[[idx]] <- data.frame(
        cliente = cli,
        actividad = perfil_tipo,
        producto = prod,
        semana = weeks,
        demanda = vals,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  do.call(rbind, rows)
}

validate_history <- function(data, n_weeks = 104, seed = 123) {
  idx_cliente <- grep("Cliente", names(data), ignore.case = TRUE)
  idx_producto <- grep("Producto", names(data), ignore.case = TRUE)
  idx_cantidad <- grep("Cantidad", names(data), ignore.case = TRUE)
  
  if (length(idx_cliente) > 0 && length(idx_producto) > 0 && length(idx_cantidad) > 0 && !("semana" %in% names(data))) {
    col_cliente <- names(data)[idx_cliente[1]]
    col_producto <- names(data)[idx_producto[1]]
    col_cantidad <- names(data)[idx_cantidad[1]]
    
    agg_base <- aggregate(
      data[[col_cantidad]], 
      by = list(cliente = data[[col_cliente]], producto = data[[col_producto]]), 
      FUN = mean, 
      na.rm = TRUE
    )
    names(agg_base)[3] <- "demanda_base"
    
    set.seed(seed)
    start_date <- as.Date("2025-01-01")
    weeks <- start_date + 7 * (seq_len(n_weeks) - 1)
    rows <- vector("list", nrow(agg_base))
    
    for (i in seq_len(nrow(agg_base))) {
      cli <- trimws(as.character(agg_base$cliente[i]))
      prod <- trimws(as.character(agg_base$producto[i]))
      base <- agg_base$demanda_base[i]
      if (base <= 2) base <- 5 
      
      perfil_tipo <- asignar_perfil(cli)
      prof <- ACTIVITY_PROFILES[[perfil_tipo]]
      
      amp <- prof$amp_base
      phase <- runif(1, 0, 1)
      trend <- runif(1, -0.02, 0.08)
      
      vals <- numeric(n_weeks)
      for (t in seq_len(n_weeks)) {
        seasonal <- amp * sin(2 * pi * (t / 52 + phase))
        noise <- rnorm(1, mean = 0, sd = base * 0.10)
        vals[t] <- max(0, round(base * (1 + seasonal) * (1 + trend * t / 52) + noise))
      }
      
      rows[[i]] <- data.frame(
        cliente = cli,
        actividad = perfil_tipo,
        producto = prod,
        semana = weeks,
        demanda = vals,
        stringsAsFactors = FALSE
      )
    }
    data <- do.call(rbind, rows)
  } else {
    required <- c("cliente", "producto", "semana", "demanda")
    missing_cols <- setdiff(required, names(data))
    if (length(missing_cols) > 0) stop("Faltan columnas obligatorias.")
    
    data$cliente <- trimws(as.character(data$cliente))
    data$producto <- trimws(as.character(data$producto))
    data$semana <- as.Date(data$semana)
    data$demanda <- suppressWarnings(as.numeric(data$demanda))
    
    if (anyNA(data$semana)) stop("La columna semana debe contener fechas validas.")
    if (anyNA(data$demanda) || any(!is.finite(data$demanda))) stop("La demanda debe ser numerica.")
    if (any(data$demanda < 0)) stop("La demanda no puede ser negativa.")
    
    data <- data[order(data$cliente, data$producto, data$semana), ]
    rownames(data) <- NULL
  }
  return(data)
}