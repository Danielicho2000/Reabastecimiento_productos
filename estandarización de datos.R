library(dplyr)
library(lubridate)

# Supongamos que cargaste tu archivo así:
datos_crudos <- read.csv("D:\\POLI\\Noveno Semestre\\Modelo De Riesgos\\Reabastecimiento_productos\\Datos.csv")

datos_modelo <- datos_crudos %>%
  # 1. Convertir Order Date a fecha (ajusta el formato si es mes/día o día/mes)
  # Si es Mes/Día/Año (ej. 11/08/2016 para 8 de noviembre), usa mdy():
  mutate(fecha = mdy(Order.Date)) %>% 
  
  # 2. Redondear al lunes de esa semana
  mutate(semana = floor_date(fecha, unit = "week", week_start = 1)) %>%
  
  # 3. Agrupar por vendedor, producto y semana
  group_by(Retail.Sales.People, Product.Name, semana) %>%
  
  # 4. Sumar la CANTIDAD (Quantity), NO las ventas monetarias
  summarise(demanda = sum(Quantity, na.rm = TRUE), .groups = "drop") %>%
  
  # 5. Renombrar las columnas para que coincidan con tu función validate_history()
  rename(
    cliente = Retail.Sales.People,
    producto = Product.Name
  ) %>%
  # Asegurar que no hay vacíos y organizar
  filter(!is.na(semana)) %>%
  arrange(cliente, producto, semana)

# Guardar el CSV limpio que subirás a tu app Shiny
write.csv(datos_modelo, "datos_listos_para_shiny.csv", row.names = FALSE)