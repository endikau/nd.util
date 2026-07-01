#' nd_app
#'
#' Seitengerüst (Container) für interaktive Apps, z. B. Shiny. Ersetzt den
#' bisherigen Aufruf `nd_page(.page_type = "app", .navbar = NULL, .main = ...)`.
#'
#' Geladen werden dieselben gemeinsamen Assets wie im `head` des Hugo-Themes
#' (`nd.site/_hugo/themes/endikau_theme/layouts/partials/head.html`) – jedoch
#' ohne Site-Chrome: kein Headroom, kein TOC, kein Masonry, kein Matomo-Tracking
#' und kein Wordcloud (wordcloud2/d3/d3-cloud). iframe-resizer wird bewusst als
#' **child** geladen, da eine App typischerweise selbst im iframe eingebettet
#' ist. Alle Pfade liegen unter `assets/...` und setzen voraus, dass dieses
#' Verzeichnis bereitsteht (in Shiny via
#' `shiny::addResourcePath("assets", <nd_assets>/dist)`).
#'
#' @param ... App-Inhalt; wird direkt in den `<body>` gelegt. Die Breite regelt
#'   beim Einbetten als iframe der iframe-resizer (child), daher kein eigener
#'   Layout-Container.
#' @param .title Seitentitel (`<title>`).
#' @param .lang Sprachcode für das `<html>`-Element.
#' @param .head Optionale, app-spezifische `<head>`-Inhalte (z. B. eigene
#'   Styles/Skripte), als Tag oder `tagList`.
#'
#' @return Ein `<html>`-`shiny.tag`.
#' @export
#'
#' @examples
#' NULL
nd_app <- function(
  ...,
  .title = "EnDiKaU",
  .lang = "de",
  .head = NULL
){

  tags$html(
    lang = .lang,
    `scroll-behavior` = "smooth",
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      tags$title(.title),

      # Styles: kompiliertes Site-Stylesheet + Schriftarten.
      tags$link(href = "assets/css/nd_site.css", rel = "stylesheet"),
      tags$link(href = "assets/fonts/fonts.css", rel = "stylesheet"),

      # Kern-Bibliotheken (jquery vor allem, was darauf aufbaut; bootstrap vor
      # popover.js, das bootstrap.Popover nutzt).
      tags$script(src = "assets/vendor/jquery/js/jquery.min.js"),
      tags$script(src = "assets/vendor/bootstrap/js/bootstrap.bundle.min.js"),
      tags$script(src = "assets/vendor/fontawesome/js/all.min.js"),

      # twemoji: Emoji-Ersetzung wie im Site-head.
      tags$script(src = "assets/vendor/twemoji/js/twemoji.js"),
      tags$style(
        "img.emoji{cursor:pointer;height:1em;width:1em;margin:0 .05em 0 .1em;vertical-align:-0.1em;}"
      ),
      tags$script(htmltools::HTML(stringi::stri_c(
        "(function(){var b='assets/vendor/twemoji/';",
        "function p(e){if(window.twemoji){twemoji.parse(e,",
        "{base:b,folder:'svg',ext:'.svg'});}}",
        "p(document);window.addEventListener('load',function(){p(document);});",
        "})();"
      ))),

      # Popover-Engine (template-basierte Popovers).
      tags$script(src = "assets/js/popover.js"),

      # Navbar-Höhenanpassung (no-op ohne Navbar). TOC bewusst ausgelassen.
      tags$script(src = "assets/js/navbar_height.js"),

      # Glide (Karussell) für nd_carousel().
      tags$link(href = "assets/vendor/glide/css/glide.core.css", rel = "stylesheet"),
      tags$link(href = "assets/vendor/glide/css/glide.theme.css", rel = "stylesheet"),
      tags$script(src = "assets/vendor/glide/js/glide.js"),

      # iframe-resizer als CHILD (App läuft eingebettet im iframe).
      tags$script(src = "assets/vendor/iframe-resizer/js/iframe-resizer.child.js"),

      # Keep-alive: Hält die Shiny-WebSocket-Verbindung aktiv, damit ein
      # vorgelagerter Reverse-Proxy sie bei Inaktivität nicht als idle
      # schließt ("Disconnected from the server"). Läuft im iframe der App;
      # beendet sich damit automatisch, sobald der/die Nutzer:in wegnavigiert
      # oder den Tab schließt (iframe wird abgebaut) – die Session wird dann
      # regulär inaktiv.
      tags$script(htmltools::HTML(stringi::stri_c(
        "(function(){setInterval(function(){",
        "if(window.Shiny&&Shiny.setInputValue){",
        "Shiny.setInputValue('.clientKeepAlive',Date.now(),{priority:'event'});",
        "}},30000);})();"
      ))),

      # App-spezifische Ergänzungen.
      .head
    ),
    tags$body(
      class = "py-0",
      style = "background-color: var(--bs-body-bg);",
      ...
    )
  )

}
