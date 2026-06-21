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
    purrr::modify_if(is.character, htmltools::HTML)
    htmltools::renderTags(indent = FALSE) |> 
    purrr::chuck("html") |> 
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
