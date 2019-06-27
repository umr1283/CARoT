#' qc_plink
#'
#' @param input_directory A `character`. The path to the VCF files.
#' @param output_directory A `character`. The path to the output directory.
#' @param cohort_name A `character`. The name of the studied cohort / population.
#' @param output_file A `character`. The name of the html file produced.
#' @param title A `character`. The report's title. Default is `paste(array, "Array Quality-Control")`.
#' @param author_name A `character`. The author's name to be printed in the report.
#'     Default is `CARoT`.
#' @param author_affiliation A `character`. The affiliation to be printed in the report.
#'     Default is `NULL`.
#' @param author_email A `character`. The email to be printed in the report.
#'     Default is `NULL`.
#' @param cache A `logical`. Should the R code be cached?
#'     Default is `FALSE`.
#' @param show_code A `logical`. Should the R code be printed in the report?
#'     Default is `FALSE`.
#' @param n_cores A `numeric`. The number of CPUs to use to estimate the ethnicity.
#'     Default is `1`.
#' @param dpi A `numeric`. The value for dpi when plotting the data.
#'     Default is `120`.
#' @param gg_fontsize A `numeric`. Value for the font size. Default is `12`.
#' @param encoding A `character`. The encoding to be used for the html report.
#'     Default is `"UTF-8"`.
#' @param ... Parameters to pass to `rmarkdown::render()`.
#'
#' @return NULL
#' @export
qc_plink <- function(
  input_directory = NULL,
  output_directory = NULL,
  cohort_name = "CARoT",
  output_file = paste0(cohort_name, "_imputation_QC.html"),
  title = "Post-Imputation Quality-Control",
  author_name = "CARoT",
  author_affiliation = NULL,
  author_email = NULL,
  cache = FALSE,
  show_code = FALSE,
  n_cores = 1,
  dpi = 120,
  gg_fontsize = 12,
  encoding = "UTF-8",
  ...
) {
  message_prefix <- "[CARoT] "

  message(message_prefix, "Quality-Control started ...")
  message(message_prefix, "Note: it can take from one to two hours.")

  file.copy(
    from = system.file("rmarkdown", "qc_pimpute.Rmd", package = "CARoT"),
    to = paste0(tempdir(), "/qc_pimpute.Rmd"),
    overwrite = TRUE
  )

  rmarkdown::render(
    input = paste0(tempdir(), "/qc_impute.Rmd"),
    output_file = output_file,
    output_dir = output_directory,
    encoding = encoding,
    params = list(
      input_directory = input_directory,
      output_directory = output_directory,
      cohort_name = cohort_name,
      title = title,
      author_name = author_name,
      author_affiliation = author_affiliation,
      author_email = author_email,
      cache = cache,
      show_code = show_code,
      n_cores = n_cores,
      dpi = dpi,
      gg_fontsize = gg_fontsize
    ),
    ...
  )

  message(message_prefix, "Quality-Control ended.")

  message(
    paste(
      paste("  ",
        utils::capture.output(
          fs::dir_tree(path = normalizePath(output_directory), recurse = FALSE)
        )
      ),
      collapse = "\n"
    )
  )

  invisible()
}
