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
    purrr::map_chr(as.character) |>
    stringi::stri_c(collapse = "\n") |>
    (\(.str) {
      stringi::stri_c(
        "{{{< raw_html >}}}",
        "```{=html}",
        .str,
        "```",
        "{{{< /raw_html >}}}",
        sep = "\n"
      )
    })() |>
    knitr::asis_output()
}
