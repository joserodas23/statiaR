# by Jose Rodas
#
# Conexion a Supabase para todas las funciones del paquete. Se guarda en
# variables de entorno de la sesion (no en disco), asi statia_configurar()
# se corre una vez al abrir RStudio y el resto de funciones no piden la URL
# ni la clave en cada llamada.

#' Configurar la conexion a Statia Observa
#'
#' Guarda la URL del proyecto y la clave publicable (anon/publishable) de
#' Supabase para el resto de la sesion de R. Si se llama sin argumentos,
#' utiliza la configuracion oficial por defecto de Statia Observa.
#' Usa SIEMPRE la clave publicable, nunca la service role -- las tablas que
#' lee este paquete (respuestas y diccionario de variables) estan protegidas
#' por RLS y optimizadas para lectura publica docente/investigacion.
#'
#' @param url URL del proyecto de Supabase, ej. "https://xxxx.supabase.co".
#'   Por defecto usa la URL oficial de Statia Observa o la variable de entorno STATIA_URL.
#' @param key Clave publicable (anon/publishable) de Supabase.
#'   Por defecto usa la clave publica oficial de Statia Observa o la variable de entorno STATIA_KEY.
#' @return NULL (invisible). Se llama por su efecto de guardar la configuracion.
#' @examples
#' \dontrun{
#' # Opcion 1: Con valores por defecto de Statia Observa
#' statia_configurar()
#'
#' # Opcion 2: Con parametros explicitos
#' statia_configurar("https://rkfwhwgktcafsaitlhbl.supabase.co", "sb_publishable_...")
#' }
#' @export
statia_configurar <- function(url = NULL, key = NULL) {
  if (is.null(url) || !nzchar(url)) {
    url <- Sys.getenv("STATIA_URL")
    if (!nzchar(url)) url <- "https://rkfwhwgktcafsaitlhbl.supabase.co"
  }
  if (is.null(key) || !nzchar(key)) {
    key <- Sys.getenv("STATIA_KEY")
    if (!nzchar(key)) key <- "sb_publishable_Td1jsjzst-fcqGv8jGZ_ww_D_Jzbj6_"
  }
  stopifnot(is.character(url), nzchar(url), is.character(key), nzchar(key))
  Sys.setenv(STATIA_URL = url, STATIA_KEY = key)
  invisible(NULL)
}

.statia_url <- function() {
  url <- Sys.getenv("STATIA_URL")
  if (!nzchar(url)) {
    url <- "https://rkfwhwgktcafsaitlhbl.supabase.co"
    Sys.setenv(STATIA_URL = url)
  }
  url
}

.statia_key <- function() {
  key <- Sys.getenv("STATIA_KEY")
  if (!nzchar(key)) {
    key <- "sb_publishable_Td1jsjzst-fcqGv8jGZ_ww_D_Jzbj6_"
    Sys.setenv(STATIA_KEY = key)
  }
  key
}

# Helper interno: una llamada GET a la API REST de Supabase (PostgREST), con
# los headers de autenticacion ya armados. `query` es una lista de parametros
# de la URL (select, filtros estilo "columna=eq.valor", order, etc.).
.statia_get <- function(path, query) {
  key <- .statia_key()
  req <- httr2::request(paste0(.statia_url(), path))
  req <- httr2::req_headers(req, apikey = key, Authorization = paste("Bearer", key))
  req <- httr2::req_url_query(req, !!!query)
  httr2::req_perform(req)
}
