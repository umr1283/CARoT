# nocov start

core <- c("ggplot2", "tibble", "tidyr", "readr", "purrr", "dplyr", "stringr", "forcats", "MiSTr", "rain", "NACHO")

.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  crayon::num_colors(TRUE)
  msg(paste(carot_logo(), collapse = "\n"))
  carot_attach()

  if (!"package:conflicted" %in% search()) {
    x <- carot_conflicts()
    msg(carot_conflict_message(x), startup = TRUE)
  }

}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

core_unloaded <- function() {
  search <- paste0("package:", core)
  core[!search %in% search()]
}

# Attach the package from the same package library it was
same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  do.call(
    "library",
    list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
  )
}

carot_attach <- function() {
  to_load <- core_unloaded()
  if (length(to_load) == 0)
    return(invisible())

  msg(
    cli::rule(
      left = crayon::bold("Attaching packages"),
      right = paste0("CARoT ", package_version("CARoT"))
    ),
    startup = TRUE
  )

  versions <- vapply(to_load, package_version, character(1))
  packages <- paste0(
    crayon::green(cli::symbol$tick), " ", crayon::blue(format(to_load)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  suppressPackageStartupMessages(
    lapply(to_load, same_library)
  )

  invisible()
}

package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- crayon::red(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}



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



#' Conflicts between the carot and other packages
#'
#' This function lists all the conflicts between packages in the carot
#' and other packages that you have loaded.
#'
#' There are four conflicts that are deliberately ignored: \code{intersect},
#' \code{union}, \code{setequal}, and \code{setdiff} from dplyr. These functions
#' make the base equivalents generic, so shouldn't negatively affect any
#' existing code.
#'
#' @export
#' @examples
#' carot_conflicts()
carot_conflicts <- function() {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- purrr::set_names(envs)
  objs <- invert(lapply(envs, ls_env))

  conflicts <- purrr::keep(objs, ~ length(.x) > 1)

  tidy_names <- paste0("package:", carot_packages())
  conflicts <- purrr::keep(conflicts, ~ any(.x %in% tidy_names))

  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)

  structure(conflict_funs, class = "carot_conflicts")
}

carot_conflict_message <- function(x) {
  if (length(x) == 0) return("")

  header <- cli::rule(
    left = crayon::bold("Conflicts"),
    right = "carot_conflicts()"
  )

  pkgs <- x %>% purrr::map(~ gsub("^package:", "", .))
  others <- pkgs %>% purrr::map(`[`, -1)
  other_calls <- purrr::map2_chr(
    others, names(others),
    ~ paste0(crayon::blue(.x), "::", .y, "()", collapse = ", ")
  )

  winner <- pkgs %>% purrr::map_chr(1)
  funs <- format(paste0(crayon::blue(winner), "::", crayon::green(paste0(names(x), "()"))))
  bullets <- paste0(
    crayon::red(cli::symbol$cross), " ", funs,
    " masks ", other_calls,
    collapse = "\n"
  )

  paste0(header, "\n", bullets)
}

#' @export
print.carot_conflicts <- function(x, ..., startup = FALSE) {
  cli::cat_line(carot_conflict_message(x))
}

#' @importFrom magrittr %>%
confirm_conflict <- function(packages, name) {
  # Only look at functions
  objs <- packages %>%
    purrr::map(~ get(name, pos = .)) %>%
    purrr::keep(is.function)

  if (length(objs) <= 1)
    return()

  # Remove identical functions
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1)
    return()

  packages
}

ls_env <- function(env) {
  x <- ls(pos = env)
  if (identical(env, "package:dplyr")) {
    x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
  }
  x
}

msg <- function(..., startup = FALSE) {
  if (startup) {
    if (!isTRUE(getOption("carot.quiet"))) {
      packageStartupMessage(text_col(...))
    }
  } else {
    message(text_col(...))
  }
}

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)

}

#' List all packages in the carot
#'
#' @param include_self Include carot in the list?
#' @export
#' @examples
#' carot_packages()
carot_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("CARoT")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "CARoT")
  }

  names
}

invert <- function(x) {
  if (length(x) == 0) return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(
    paste0(...),
    crayon::make_style(grDevices::grey(level), grey = TRUE)
  )
}

# nocov end
