# by Jose Rodas

#' Diagnostico de calidad de datos (valores faltantes) de una sesion
#'
#' Clasifica cada pregunta real de un instrumento (nunca una pregunta inventada: usa el
#' texto y las reglas ya guardadas en el diccionario, \code{statia_codebook()}) y cuenta sus
#' valores NA en tres categorias legitimas -- excepcion registrada (\code{<variable>_estado}),
#' version anterior del cuestionario (\code{vigente_desde} en el diccionario) u opcional por
#' diseno actual -- mas una cuarta, "sin explicacion registrada", que requiere revision humana.
#' Nunca elimina, completa ni transforma ninguna respuesta: es un reporte de solo lectura que
#' se construye sobre \code{statia_datos()} y \code{statia_codebook()}, sin duplicar la logica
#' de traer o etiquetar datos.
#'
#' @param sesion_id Id (uuid) de la sesion. No funciona para sesiones tipo bateria
#'   (\code{instrumento_key = "_bateria"}): el diccionario esta indexado por instrumento, no
#'   por el prefijo de esa bateria en particular.
#' @param solo_validas Igual que en \code{statia_datos()}: si \code{TRUE} (default), el
#'   diagnostico de valores faltantes se calcula solo sobre las respuestas no marcadas como
#'   invalidas/sospechosas.
#' @return Un \code{data.frame}, una fila por pregunta real, con columnas \code{variable},
#'   \code{tipo_pregunta} ("obligatoria" / "obligatoria_con_excepcion" / "opcional"),
#'   \code{admite_excepcion}, \code{n_na}, \code{n_excepcion}, \code{n_version_anterior},
#'   \code{n_revisar} y \code{usar_para_casos_completos} (logico: \code{FALSE} para cualquier
#'   variable que nunca deberia usarse para descartar filas completas de la base). Trae ademas
#'   tres atributos informativos, nunca una transformacion de los datos:
#'   \code{attr(x, "totales_respuestas")} (recibidas/validas/sospechosas/invalidas),
#'   \code{attr(x, "casos_completos")} (cuantas filas quedarian si se exigiera 0 NA en todas
#'   las preguntas obligatorias sin excepcion) y \code{attr(x, "aviso_filtrado")} (que
#'   variables causarian esa perdida, de mayor a menor).
#' @examples
#' \dontrun{
#' diag <- statia_diagnostico("ID_DE_SESION")
#' diag[diag$n_na > 0, ]
#' attr(diag, "casos_completos")
#' attr(diag, "aviso_filtrado")
#' }
#' @export
statia_diagnostico <- function(sesion_id, solo_validas = TRUE) {
  stopifnot(is.character(sesion_id), length(sesion_id) == 1, nzchar(sesion_id))

  ses_resp <- .statia_get(
    "/rest/v1/observa_sesiones",
    list(select = "instrumento_key", id = paste0("eq.", sesion_id))
  )
  ses <- jsonlite::fromJSON(httr2::resp_body_string(ses_resp), flatten = TRUE)
  instr_key <- if (NROW(ses)) ses$instrumento_key[1] else NA_character_
  if (is.na(instr_key) || instr_key == "_bateria") {
    stop(
      "Diagnostico no disponible para sesiones de bateria (varios cuestionarios combinados): ",
      "el diccionario esta indexado por instrumento, no por el prefijo b0_/b1_/... de esta ",
      "bateria en particular.",
      call. = FALSE
    )
  }

  # Totales crudos de la sesion (recibidas/validas/sospechosas/invalidas) -- se necesitan ANTES
  # del filtro de statia_datos(), que ya descarta columnas que no son de la encuesta.
  cruda <- .statia_get(
    "/rest/v1/observa_respuestas",
    list(select = "validez", sesion_id = paste0("eq.", sesion_id))
  )
  val <- jsonlite::fromJSON(httr2::resp_body_string(cruda), flatten = TRUE)
  validez_col <- if (NROW(val) && "validez" %in% names(val)) val$validez else character(0)
  totales <- c(
    recibidas   = NROW(val),
    validas     = sum(is.na(validez_col) | validez_col == "valida"),
    sospechosas = sum(validez_col == "sospechosa", na.rm = TRUE),
    invalidas   = sum(validez_col == "invalida", na.rm = TRUE)
  )

  # incluir_estado=TRUE: este diagnostico SI necesita las columnas "_estado" mezcladas
  # (statia_datos() las separa por defecto -- ver su documentacion -- precisamente para que
  # el resto de analisis no las confunda con preguntas reales sin responder).
  datos <- statia_datos(sesion_id, solo_validas = solo_validas, incluir_estado = TRUE)
  dicc  <- statia_codebook(instr_key)

  # Una pregunta es "tecnica" (campo _estado, no una pregunta real) si su nombre termina en
  # "_estado" Y existe una fila hermana en el diccionario con ese mismo prefijo sin el sufijo.
  es_tecnica <- grepl("_estado$", dicc$variable) & (sub("_estado$", "", dicc$variable) %in% dicc$variable)
  preguntas <- dicc[!es_tecnica, , drop = FALSE]

  filas <- lapply(seq_len(nrow(preguntas)), function(i) {
    v <- preguntas$variable[i]
    if (!(v %in% names(datos))) {
      return(data.frame(
        variable = v, tipo_pregunta = NA_character_, admite_excepcion = NA,
        n_na = NA_integer_, n_excepcion = NA_integer_, n_version_anterior = NA_integer_,
        n_revisar = NA_integer_, usar_para_casos_completos = NA,
        stringsAsFactors = FALSE
      ))
    }
    estado_col <- paste0(v, "_estado")
    admite_exc <- estado_col %in% dicc$variable
    opcional_actual <- isTRUE(preguntas$opcional[i])
    vigente <- preguntas$vigente_desde[i]
    tiene_vigente <- !is.null(vigente) && !is.na(vigente)

    columna <- datos[[v]]
    es_na <- is.na(columna) | (is.character(columna) & !is.na(columna) & columna == "")
    n_na <- sum(es_na, na.rm = TRUE)
    n_exc <- 0L; n_ant <- 0L; n_rev <- 0L
    if (n_na > 0 && any(es_na)) {
      idx <- which(es_na)
      tiene_estado_col <- admite_exc && estado_col %in% names(datos)
      for (j in idx) {
        if (tiene_estado_col && !is.na(datos[[estado_col]][j])) { n_exc <- n_exc + 1L; next }
        # Comparacion de texto ISO8601: PostgREST siempre devuelve timestamptz normalizado a
        # UTC con el mismo formato de offset, asi que el orden de caracteres coincide con el
        # orden cronologico -- no hace falta parsear fechas para esta comparacion.
        if (tiene_vigente && !is.null(datos$created_at) && !is.na(datos$created_at[j]) &&
            as.character(datos$created_at[j]) < as.character(vigente)) {
          n_ant <- n_ant + 1L; next
        }
        if (opcional_actual) next
        n_rev <- n_rev + 1L
      }
    }
    tipo <- if (admite_exc) "obligatoria_con_excepcion" else if (opcional_actual) "opcional" else "obligatoria"
    data.frame(
      variable = v, tipo_pregunta = tipo, admite_excepcion = admite_exc,
      n_na = n_na, n_excepcion = n_exc, n_version_anterior = n_ant, n_revisar = n_rev,
      usar_para_casos_completos = !(admite_exc || opcional_actual || n_rev > 0),
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, filas)
  rownames(resultado) <- NULL

  # Aviso de filtrado global: casos 100% completos solo exigiendo las preguntas "obligatoria"
  # puras (sin excepcion, no opcionales) -- esas nunca deben usarse para esto, ver arriba.
  estrictas <- resultado$variable[!is.na(resultado$tipo_pregunta) & resultado$tipo_pregunta == "obligatoria"]
  if (length(estrictas)) {
    incompleta <- Reduce(`|`, lapply(estrictas, function(v) is.na(datos[[v]])))
    n_completos <- sum(!incompleta)
    perdida <- vapply(estrictas, function(v) sum(is.na(datos[[v]])), integer(1))
    aviso <- data.frame(variable = estrictas, filas_perdidas = perdida, stringsAsFactors = FALSE)
    aviso <- aviso[order(-aviso$filas_perdidas), , drop = FALSE]
    rownames(aviso) <- NULL
  } else {
    n_completos <- nrow(datos)
    aviso <- data.frame(variable = character(0), filas_perdidas = integer(0))
  }

  attr(resultado, "totales_respuestas") <- totales
  attr(resultado, "casos_completos") <- c(n_completos = n_completos, n_total = nrow(datos))
  attr(resultado, "aviso_filtrado") <- aviso
  resultado
}
