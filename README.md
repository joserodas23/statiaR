# statiaR

**statiaR** es un paquete de R diseñado para recuperar datos de **Statia Observa** con sus etiquetas de variable y de valor ya incorporadas — de forma idéntica a una base importada desde SPSS (.sav) con haven, pero conectado en tiempo real a Supabase.

---

## 🚀 Instalación

Para instalar statiaR desde RStudio, ejecuta en tu consola de R:

`
# 1. Asegúrate de tener instalado el paquete 'remotes'
if (!requireNamespace(" remotes\, quietly = TRUE)) {
 install.packages(\remotes\)
}

# 2. Instala statiaR directamente desde GitHub
remotes::install_github(\joserodas23/statiaR\)
`

---

## 📖 Guía Rápida de Uso

`
library(statiaR)

# 1. Configurar conexión (se ejecuta una sola vez por sesión de R)
# Por defecto se conecta automáticamente a Statia Observa:
statia_configurar()

# (Opcional) Si tu docente te indica credenciales personalizadas:
# statia_configurar(url = \https://xxxx.supabase.co\, key = \sb_publishable_...\)

# 2. Consultar instrumentos disponibles con diccionario cargado
instrumentos <- statia_instrumentos()
print(instrumentos)

# 3. Explorar el libro de códigos (codebook / diccionario de variables)
codebook <- statia_codebook(\virtualidad_lluvias_untumbes\)
View(codebook)

# 4. Descargar y etiquetar respuestas de una sesión autorizada
# Reemplaza 'ID_DE_SESION' con el código UUID proporcionado por el docente
datos <- statia_datos(\38608301-f414-4201-8c72-981d3f03ac30\)
View(datos)

# 5. Ver etiquetas de variable y de valor
attr(datos, \label\)
# [1] \¿Qué tan satisfecho está con las clases virtuales actuales?\

attr(datos, \labels\)
# Muy insatisfecho Muy satisfecho 
# 1 5 

# 6. Diagnóstico de valores faltantes (NA)
diag <- statia_diagnostico(\38608301-f414-4201-8c72-981d3f03ac30\)
print(diag)
attr(diag, \casos_completos\)
attr(diag, \aviso_filtrado\)

# 7. Convertir variables ordinales a factores ordenados reales de R
datos_ord <- statia_datos(\38608301-f414-4201-8c72-981d3f03ac30\, tipificar_ordinal = TRUE)
is.ordered(datos_ord)
levels(datos_ord)
`

---

## 🛠️ Resumen de Funciones

| Función | Descripción |
|---|---|
| statia_configurar(url, key) | Establece la conexión con el servidor. Si no se especifican argumentos, usa los valores oficiales por defecto. |
| statia_datos(sesion_id, solo_validas = TRUE, incluir_estado = FALSE, tipificar_ordinal = FALSE) | Descarga las respuestas de una sesión y vincula las etiquetas del diccionario. |
| statia_codebook(instrumento_key) | Recupera el diccionario de variables (preguntas, escalas, tipos y opciones) de un instrumento. |
| statia_instrumentos() | Devuelve un vector con las claves de instrumentos disponibles. |
| statia_diagnostico(sesion_id, solo_validas = TRUE) | Realiza un diagnóstico pedagógico y metodológico de valores perdidos (NA). |

---

## 🔒 Privacidad y Ética de Datos

- **Anonimización:** Los datos descargados a través de statia_datos() no contienen identificadores personales (nombres, DNI, correos ni datos de contacto).
- **Acceso por Sesión:** Las consultas se realizan mediante el identificador único (sesion_id) asignado para fines académicos por el docente.
- **Integridad:** Las funciones operan en modo de solo lectura (HTTP GET) y no modifican las respuestas en el repositorio central.

---

## 👤 Autor

**José Luis Rodas Cobos** 
Docente de Psicoestadística II 
Email: joserds25@gmail.com