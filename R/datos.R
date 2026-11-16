# by Jose Rodas

#' Descargar y etiquetar las respuestas de una sesion de Statia Observa
#'
#' Trae las respuestas (\code{observa_respuestas}) de una sesion y les pega
#' las etiquetas de variable -- y, para preguntas Likert, las etiquetas de
#' los extremos de la escala -- usando el diccionario de
#' \code{observa_diccionario_variables} (ver \code{statia_codebook()}).
#' Los valores de las respuestas no se modifican en ningun momento; solo se
#' agregan atributos (\code{attr(x, "label")}, \code{attr(x, "labels")},
#' \code{attr(x, "escala")}) a cada columna, igual que al importar un .sav de
#' SPSS con el paquete \code{haven}.
#'
#' @param sesion_id Id (uuid) de la sesion, de la tabla \code{observa_sesiones}.
#' @param solo_validas Si \code{TRUE} (default), excluye respuestas marcadas
#'   como invalidas (trampas de atencion, tiempo de respuesta invalido, etc.).
#' @param incluir_estado Si \code{FALSE} (default), las columnas companeras
#'   \code{<variable>_estado} (el motivo de una excepcion -- "sin_experiencia",
#'   "no_corresponde", etc.) NO se devuelven mezcladas en el data.frame
#'   principal: quedan aparte en \code{attr(datos, "excepciones")}. Esto es a
#'   proposito: esas columnas van a estar vacias (NA) en el 90-99% de las filas
#'   por diseno (solo se llenan para quien uso la excepcion), y si se dejan
#'   mezcladas con las preguntas reales, cualquier reporte generico de valores
#'   perdidos (summary(), naniar, etc.) las confunde con preguntas sin
#'   responder. Pasa \code{TRUE} si de verdad necesitas filtrar/cruzar por el
#'   motivo de excepcion en el mismo data.frame.
#' @param tipificar_ordinal Si \code{TRUE} (default \code{FALSE}, para no
#'   romper scripts existentes que esperan texto/numero crudo), convierte a
#'   \code{factor} ordenado de R cada variable que el diccionario marca como
#'   \code{escala_estadistica == "ordinal"} (ej. \code{ciclo},
#'   \code{interrupciones}), usando el orden real de \code{opciones} del
#'   diccionario -- no el orden alfabetico ni el de aparicion en los datos.
#'   Las variables Likert de este proyecto estan clasificadas como
#'   \code{"intervalo"} en el diccionario (no \code{"ordinal"}), asi que esta
#'   opcion no las toca -- se analizan con sus metodos de siempre (medias,
#'   Cronbach, etc.). Si una variable esta marcada ordinal pero el diccionario
#'   no tiene \code{opciones} (no se puede saber el orden de sus niveles),
#'   se deja sin convertir y se avisa con un \code{warning}.
#' @return Un data.frame con columnas \code{id}, \code{created_at} y una
#'   columna por cada pregunta respondida (sin el prefijo \code{"datos."}
#'   que usa la API cruda; sin las columnas \code{_estado}, salvo que se pida
#'   \code{incluir_estado = TRUE} -- ver \code{attr(datos, "excepciones")}).
#'   Para sesiones tipo bateria (varios cuestionarios combinados, prefijo
#'   \code{b0_}/\code{b1_}/...), se devuelven los datos SIN etiquetas (con un
#'   aviso) porque el diccionario esta indexado por instrumento, no por
#'   prefijo de bateria.
#' @examples
#' \dontrun{
#' statia_configurar("https://rkfwhwgktcafsaitlhbl.supabase.co", "sb_publishable_...")
#' datos <- statia_datos("ID_DE_SESION")
#' attr(datos$satisfaccion_general, "label")
#'
#' # Por que alguien no respondio "edad" (solo si usaste incluir_estado=TRUE,
#' # o mira attr(datos, "excepciones") con el default):
#' attr(datos, "excepciones")
#'
#' # Variables ordinales (ciclo, interrupciones, etc.) como factor ordenado real,
#' # con el orden correcto -- no alfabetico:
#' datos2 <- statia_datos("ID_DE_SESION", tipificar_ordinal = TRUE)
#' levels(datos2$interrupciones)
#' is.ordered(datos2$interrupciones)
#' }
#' @export
statia_datos <- function(sesion_id, solo_validas = TRUE, incluir_estado = FALSE, tipificar_ordinal = FALSE) {
  stopifnot(is.character(sesion_id), length(sesion_id) == 1, nzchar(sesion_id))

  resp <- .statia_get(
    "/rest/v1/observa_respuestas",
    list(select = "*", sesion_id = paste0("eq.", sesion_id))
  )
  crudo <- jsonlite::fromJSON(httr2::resp_body_string(resp), flatten = TRUE)
  if (!NROW(crudo)) stop("Sin respuestas para sesion_id = '", sesion_id, "'.", call. = FALSE)

  if (isTRUE(solo_validas) && "validez" %in% names(crudo)) {
    crudo <- crudo[is.na(crudo$validez) | crudo$validez == "valida", , drop = FALSE]
    if (!NROW(crudo)) stop("Todas las respuestas de esta sesion quedaron excluidas por 'solo_validas = TRUE'. ",
                            "Prueba statia_datos(sesion_id, solo_validas = FALSE).", call. = FALSE)
  }

  cols_encuesta <- grep("^datos\\.", names(crudo), value = TRUE)
  datos <- crudo[, c("id", "created_at", cols_encuesta), drop = FALSE]
  names(datos) <- sub("^datos\\.", "", names(datos))

  # Separar las columnas "_estado" del data.frame principal (ver incluir_estado arriba).
  # Solo cuenta como "_estado" la que tiene una pregunta hermana real en los mismos datos
  # (ej. "edad_estado" junto a "edad") -- asi nunca se confunde con una pregunta que de
  # casualidad termine en "_estado" por su propio nombre.
  cols_estado <- grep("_estado$", names(datos), value = TRUE)
  cols_estado <- cols_estado[sub("_estado$", "", cols_estado) %in% names(datos)]
  excepciones <- NULL
  if (length(cols_estado)) {
    excepciones <- datos[, c("id", cols_estado), drop = FALSE]
    if (!isTRUE(incluir_estado)) datos <- datos[, !(names(datos) %in% cols_estado), drop = FALSE]
  }

  ses_resp <- .statia_get(
    "/rest/v1/observa_sesiones",
    list(select = "instrumento_key", id = paste0("eq.", sesion_id))
  )
  ses <- jsonlite::fromJSON(httr2::resp_body_string(ses_resp), flatten = TRUE)
  instr_key <- if (NROW(ses)) ses$instrumento_key[1] else NA_character_

  if (is.na(instr_key) || instr_key == "_bateria") {
    warning(
      "Sesion tipo bateria (o sin instrumento_key identificable): el diccionario ",
      "por instrumento no aplica directo porque cada pregunta tiene un prefijo ",
      "b0_/b1_/b2_... segun el orden en que se armo esa bateria en particular. ",
      "Se devuelven los datos SIN etiquetas.",
      call. = FALSE
    )
    attr(datos, "instrumento") <- instr_key
    if (!is.null(excepciones)) attr(datos, "excepciones") <- excepciones
    return(datos)
  }

  dicc <- statia_codebook(instr_key)
  if (NROW(dicc)) {
    for (i in seq_len(nrow(dicc))) {
      v <- dicc$variable[i]
      if (!(v %in% names(datos))) next

      attr(datos[[v]], "label") <- dicc$etiqueta[i]
      if (!is.na(dicc$escala_estadistica[i])) attr(datos[[v]], "escala") <- dicc$escala_estadistica[i]

      # Etiquetas de los extremos de la escala (solo Likert: tiene etiqueta_valor_min/max
      # y un rango tipo "1-5" del que sacamos los numeros reales de los extremos,
      # en vez de asumir siempre 1 y 5 -- por si algun instrumento futuro usa otra escala).
      tiene_extremos <- !is.na(dicc$etiqueta_valor_min[i]) && !is.na(dicc$etiqueta_valor_max[i])
      if (tiene_extremos && !is.na(dicc$rango[i])) {
        nums <- suppressWarnings(as.numeric(regmatches(dicc$rango[i], gregexpr("[0-9]+", dicc$rango[i]))[[1]]))
        if (length(nums) >= 2) {
          vals <- c(nums[1], nums[length(nums)])
          names(vals) <- c(dicc$etiqueta_valor_min[i], dicc$etiqueta_valor_max[i])
          attr(datos[[v]], "labels") <- vals
        }
      }
    }

    # Tipificar ordinales (ver tipificar_ordinal arriba) -- pasada aparte, DESPUES de que
    # cada columna ya tiene sus atributos label/escala, para poder reponerselos tras el
    # factor() (factor() crea un vector nuevo y pierde cualquier atributo previo).
    if (isTRUE(tipificar_ordinal)) {
      for (i in seq_len(nrow(dicc))) {
        if (is.na(dicc$escala_estadistica[i]) || dicc$escala_estadistica[i] != "ordinal") next
        v <- dicc$variable[i]
        if (!(v %in% names(datos))) next
        niveles <- dicc$opciones[[i]]
        if (is.null(niveles) || !length(niveles)) {
          warning(
            "'", v, "' esta marcada como ordinal en el diccionario pero no tiene 'opciones' ",
            "declaradas (no se conoce el orden real de sus niveles) -- se deja sin convertir.",
            call. = FALSE
          )
          next
        }
        lab    <- attr(datos[[v]], "label")
        escala <- attr(datos[[v]], "escala")
        datos[[v]] <- factor(datos[[v]], levels = niveles, ordered = TRUE)
        attr(datos[[v]], "label")  <- lab
        attr(datos[[v]], "escala") <- escala
      }
    }
  }

  attr(datos, "instrumento") <- instr_key
  if (!is.null(excepciones)) attr(datos, "excepciones") <- excepciones
  datos
}
