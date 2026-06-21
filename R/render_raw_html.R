#' Title
#'
#' @param .html ...
#'
#' @returns ...
#' @export
#'
#' @examples
#' NULL
render_raw_html <- function(.html) {
  .html |>
    (\(..x) if (is.character(..x)) htmltools::HTML(..x) else ..x)() |>
    htmltools::renderTags(indent = FALSE) |>
    purrr::chuck("html") |>
    (\(..x) {
      stringi::stri_c(
        "{{{< raw_html >}}}",
        "```{=html}",
        ..x,
        "```",
        "{{{< /raw_html >}}}",
        sep = "\n"
      )
    })() |>
    knitr::asis_output()
}
