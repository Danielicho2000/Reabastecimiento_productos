# Funciones auxiliares para formatear salidas y resumenes.
# Se deja separado para poder ampliar exportacion de resultados sin tocar Shiny.

solution_to_long <- function(mat, value_name) {
  if (is.null(mat)) return(data.frame())
  out <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
  names(out) <- c("producto", "periodo", value_name)
  out
}