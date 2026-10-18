# Proyecto modular: pronostico de demanda + CLSP en Shiny

## Que incluye

- `app.R`: interfaz Shiny y conexion de modulos.
- `R/data.R`: generacion de historia sintetica y validacion de CSV.
- `R/forecast.R`: pronostico por serie cliente-producto, escenarios por bootstrap residual y validacion temporal.
- `R/optimization.R`: modelo entero mixto de lot-sizing capacitado con setup binario.
- `R/results.R`: funciones auxiliares para resultados.

## Requisitos

R 4.2 o superior recomendado. Instala dependencias en la consola de R:

```r
install.packages(c("shiny", "DT", "ggplot2", "forecast", "lpSolve"))
```

```r
shiny::runApp()
```

## Formato del CSV real

El CSV debe incluir estas columnas:

```text
cliente,producto,semana,demanda
C01,Jabon_liquido,2025-01-06,42
C01,Jabon_liquido,2025-01-13,39
C01,Detergente,2025-01-06,18
```

- `semana`: fecha de inicio de semana, formato `YYYY-MM-DD`.
- `demanda`: unidades consumidas/vendidas en esa semana.
- Debe haber una fila por combinación cliente-producto-semana.
- Las series de cada cliente-producto deben tener el mismo calendario completo. El código no convierte silenciosamente semanas faltantes en cero, porque cero demanda y dato desconocido son cosas distintas.

## Uso de la aplicacion

1. Pulsa **Generar historia** para crear datos sintéticos o carga un CSV y pulsa **Usar CSV cargado**.
2. Elige método y horizonte; pulsa **Entrenar y pronosticar**.
3. Revisa pronóstico por cliente-producto y la demanda agregada por producto.
4. Pulsa **Resolver modelo entero** para obtener producción, inventario y setups.
5. La pestaña **Métricas** ejecuta una validación temporal simple con un bloque final de prueba.

## Importante sobre la interpretación

- La historia que se genera al abrir la app es sintética; sirve para probar el flujo, no para afirmar que el modelo predice demanda real.
- La demanda se conserva por cliente-producto para pronosticar y visualizar. Para la planificación de producción, el código agrega clientes por producto-semana. Si tu MILP debe decidir distribución, transporte o faltantes por cliente, habrá que ampliar las variables y restricciones.
- Los escenarios de demanda se generan mediante remuestreo de residuos del modelo (o ruido normal si hay pocos residuos). Es una aproximación para análisis de incertidumbre, no una red neuronal probabilística.
- `R/optimization.R` implementa un **CLSP capacitado base**, con balance de inventario, setup y capacidad. No es una reproducción completa del CLSP-TW de Brahimi et al.: las ventanas release/due deben añadirse explícitamente a partir de una definición operativa de cuándo se libera y cuándo vence cada demanda.
- Costos de producción, inventario, setup y capacidad son valores de ejemplo. Sustitúyelos por datos reales antes de interpretar el plan económicamente.
- El pronóstico y el MILP usan unidades de demanda continuas. Si las unidades deben ser enteras, añade las restricciones de integralidad correspondientes a producción/inventario según el contexto.

## Arquitectura

`datos -> entrenamiento/forecast por cliente-producto -> escenarios -> agregación por producto-semana -> CLSP -> tablas y gráficos Shiny`
