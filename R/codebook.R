# by Jose Rodas

#' Diccionario de variables (codebook) de un instrumento
#'
#' Trae, para un instrumento de Statia Observa, una fila por pregunta con su
#' nombre tecnico, el texto exacto de la pregunta, el tipo de dato, la escala
#' estadistica (nominal/ordinal/intervalo/razon), el rango de valores validos,
#' las etiquetas de los extremos de escala (para Likert) y las opciones
#' categoricas (para preguntas de seleccion).
#'
#' @param instrumento_key Clave del instrumento, ej. "virtualidad_lluvias_untumbes".
#'   Usa \code{statia_instrumentos()} para ver las claves disponibles.
#' @return Un data.frame ordenado por la posicion de la pregunta en el
#'   cuestionario (columna \code{orden}). Si el instrumento no tiene
#'   diccionario cargado (ej. un cuestionario nuevo que aun no se agrego),
#'   devuelve un data.frame vacio con un aviso (warning), no un error.
#' @examples
#' \dontrun{
#' statia_codebook("virtualidad_lluvias_untumbes")
#' }
#' @export
statia_codebook <- function(instrumento_key) {
  stopifnot(is.character(instrumento_key), length(instrumento_key) == 1, nzchar(instrumento_key))

  resp <- .statia_get(
    "/rest/v1/observa_diccionario_variables",
    list(select = "*", instrumento_key = paste0("eq.", instrumento_key), order = "orden.asc")
  )
  dicc <- jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE)

  if (!NROW(dicc)) {
    warning(
      "No hay diccionario cargado para instrumento_key = '", instrumento_key, "'. ",
      "Si es un cuestionario nuevo, hay que regenerar y cargar el diccionario primero.",
      call. = FALSE
    )
    return(data.frame())
  }
  dicc
}

#' Listar los instrumentos con diccionario disponible
#'
#' @return Vector de texto con las claves de instrumento (instrumento_key)
#'   que ya tienen diccionario cargado en Supabase, ordenadas alfabeticamente.
#' @examples
#' \dontrun{
#' statia_instrumentos()
#' }
#' @export
statia_instrumentos <- function() {
  resp <- .statia_get("/rest/v1/observa_diccionario_variables", list(select = "instrumento_key"))
  dicc <- jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE)
  if (!NROW(dicc)) return(character(0))
  sort(unique(dicc$instrumento_key))
}
