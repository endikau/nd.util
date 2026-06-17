#' nd_popover
#'
#' @param ... ...
#' @param .title ...
#' @param .id ...
#'
#' @returns ...
#' @export
#'
#' @examples
#' NULL
nd_popover <- function(..., .title="Mehr Informationen", .id=NULL){

  .dots <- list(...)

  if(is.null(.id)){
    .id <- stringi::stri_c(
      "tip-", digest::digest(list(.dots, .title, Sys.time(), stats::runif(1)), algo="crc32c")
    )
  }

  htmltools::tagList(
    tags$a(
      tabindex="0",
      class="btn btn-sm btn-primary text-light",
      role="button",
      `aria-label`=.title,
      `data-bs-toggle`="popover",
      `data-bs-template-id`=.id,
      `data-bs-title`=.title,
      icon_fa("fa-solid fa-circle-info")
    ),
    tags$template(id=.id, !!!.dots)
  )

}
