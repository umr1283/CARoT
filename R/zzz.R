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
