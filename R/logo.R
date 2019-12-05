#' The carot logo, using ASCII or Unicode characters
#'
#' Use [crayon::strip_style()] to get rid of the colors.
#'
#' @param unicode Whether to use Unicode symbols. Default is `TRUE`
#'   on UTF-8 platforms.
#'
#' @md
#' @export
#' @examples
#' carot_logo()

carot_logo <- function(unicode = l10n_info()$`UTF-8`) {
  logo <- c(
    "                                                                  ",
    "  .oooooo.         .o.       ooooooooo.             ooooooooooooo ",
    " d8P'  `Y8b       .888.      `888   `Y88.           8'   888   `8 ",
    "888              .8\"888.      888   .d88'  .ooooo.       888      ",
    "888             .8' `888.     888ooo88P'  d88' `88b      888      ",
    "888            .88ooo8888.    888`88b.    888   888      888      ",
    "`88b    ooo   .8'     `888.   888  `88b.  888   888      888      ",
    " `Y8bood8P'  o88o     o8888o o888o  o888o `Y8bod8P'     o888o     ",
    "                                                                  "
  )
  structure(crayon::blue(logo), class = "carot_logo")
}

#' @export

print.carot_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}
