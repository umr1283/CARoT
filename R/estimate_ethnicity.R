#' estimate_ethnicity
#'
#' @param cohort_name A `character`. A name to describe the studied population compared to 1,000 Genomes.
#' @param input_vcfs A `character`. A path to one or several VCFs file.
#' @param input_type A `character`. Either `"array"` or `"sequencing"`.
#' @param output_directory A `character`. The path where the data and figures is written.
#' @param ref1kg_vcfs A `character`. A path to the reference VCFs files (i.e., 1,000 Genomes sequencing data).
#' @param ref1kg_population A `character`. A file which describe samples and their ethnicity.
#' @param ref1kg_maf A `numeric`. MAF threshold for SNPs in 1,000 Genomes
#' @param splitted_by_chr A `logical`. Is the VCFs files splitted by chromosome?
#' @param quality_tag A `character`. Name of the imputation quality tag for `"array"`.
#' @param quality_threshold A `numeric`. The threshold to keep/discard SNPs based on their imputation quality.
#' @param n_cores An `integer`. The number of CPUs to use to estimate the ethnicity.
#' @param bin_path A `list(character)`. A list giving the binary path of
#'   `vcftools`, `bcftools`, `bgzip`, `tabix` and `plink1.9`.
#'
#' @return A `data.frame`.
#' @export
estimate_ethnicity <- function(
  cohort_name,
  input_vcfs,
  input_type,
  output_directory,
  ref1kg_vcfs,
  ref1kg_population,
  ref1kg_maf = 0.05,
  splitted_by_chr = TRUE,
  quality_tag = "INFO",
  quality_threshold = 0.9,
  n_cores = 6,
  bin_path = list(
    vcftools = "/usr/bin/vcftools",
    bcftools = "/usr/bin/bcftools",
    bgzip = "/usr/bin/bgzip",
    tabix = "/usr/bin/tabix",
    plink1.9 = "/usr/bin/plink1.9"
  )
) {
  message_prefix <- "[CARoT] "

  if (!all(sapply(bin_path, file.exists))) {
    stop(
      message_prefix,
      paste0(
        "No binary found for the following tools: ",
        glue::glue_collapse(
          x = paste0(
            names(bin_path[!sapply(bin_path, file.exists)]),
            ' ("', bin_path[!sapply(bin_path, file.exists)], '")'
          ),
          sep = ", ",
          last = " and "
        ),
        "!"
      )
    )
  }

  list_input <- check_input(input = input_vcfs)
  list_ref <- check_input(input = ref1kg_vcfs)

  if (!input_type%in%c("array", "sequencing")) {
    stop(message_prefix, '"input_type" must be either "array" or "sequencing"!')
  }

  if (input_type=="sequencing" & length(list_ref)!=1) {
    stop(
      message_prefix, 'A unique vcf file ("ref1kg_vcfs") must be provided ',
      'with `input_type = "sequencing"`!'
    )
  }
  if (input_type=="array" & !splitted_by_chr & length(list_ref)!=1) {
    stop(
      message_prefix, 'A unique vcf file ("ref1kg_vcfs") must be provided ',
      'with `input_type = "array"` & `splitted_by_chr = FALSE`!'
    )
  }

  ######################
  ### Formating VCFs ###
  ######################
  message(message_prefix, "Formating VCFs ...")
  switch(
    EXPR = input_type,
    "array" = {
      if (splitted_by_chr) {
        format_array_chr(
          cohort_name = cohort_name,
          input_vcfs = list_input,
          output_directory = output_directory,
          ref1kg_vcfs = list_ref,
          ref1kg_maf = ref1kg_maf,
          quality_tag = quality_tag,
          quality_threshold = quality_threshold,
          n_cores = n_cores,
          bin_path = bin_path
        )
      } else {
        format_array_all(
          cohort_name = cohort_name,
          input_vcfs = list_input,
          output_directory = output_directory,
          ref1kg_vcfs = list_ref,
          ref1kg_maf = ref1kg_maf,
          quality_tag = quality_tag,
          quality_threshold = quality_threshold,
          bin_path = bin_path
        )
      }
    },
    "sequencing" = {
      format_sequencing(
        cohort_name = cohort_name,
        input_vcfs = list_input,
        output_directory = output_directory,
        ref1kg_vcfs = list_ref,
        ref1kg_maf = ref1kg_maf,
        bin_path = bin_path
      )
    }
  )


  ######################
  ### Performing PCA ###
  ######################
  compute_pca(
    cohort_name = cohort_name,
    input_plink = paste0(output_directory, "/all"),
    output_directory = output_directory,
    ref1kg_population = ref1kg_population
  )

}


#' check_input
#'
#' @param input A `character`. A file path.
#'
#' @keywords internal
check_input <- function(input) {
  message_prefix <- "[CARoT] "

  name <- deparse(substitute(input))
  if (!fs::is_dir(input) & !fs::is_file(input)) {
    stop(
      message_prefix, 'A valid "', name, '" must be provided, ',
      'either a directory (with VCF files) or a vcf file!'
    )
  }
  if (fs::is_dir(input)) {
    list_input <- list.files(path = input, pattern = ".vcf.gz$", full.names = TRUE)
  } else {
    list_input <- input
  }
  if (!all(grepl(".vcf.gz$", list_input))) {
    stop(message_prefix, 'VCF files must be compressed using bgzip!')
  }
  if (length(list_input)==0) {
    stop(
      message_prefix, 'A valid "', name, '" must be provided, ',
      'either a directory (with VCF files) or a vcf file!'
    )
  }
  invisible(list_input)
}


#' format_vcf
#'
#' @inheritParams estimate_ethnicity
#' @param ichr A `character` or `numeric`. The chromosome identifier.
#'
#' @keywords internal
format_vcf <- function(
  input_vcfs,
  ref1kg_vcfs,
  ref1kg_maf,
  ichr,
  quality_tag,
  quality_threshold,
  output_directory,
  bin_path
) {
  temp_directory <- paste0(tempdir(), "/chr", ichr)
  invisible(sapply(
    X = paste0(temp_directory, c("/study", "/ref", "/isec")),
    FUN = dir.create,
    recursive = TRUE, showWarnings = FALSE, mode = '0777'
  ))
  output_study <- paste0(temp_directory, "/study/filtered_", basename(input_vcfs))
  output_ref <- paste0(temp_directory, "/ref/filtered_", basename(ref1kg_vcfs))
  if (is.character(ichr)) {
    output_merge <- paste0(output_directory, "/chr", ichr, "_merged.vcf.gz")
  } else {
    output_merge <- paste0(output_directory, sprintf("/chr%02d_merged.vcf.gz", ichr))
  }

  out_cmd <- system(
    ignore.stdout = FALSE, intern = TRUE, wait = TRUE, ignore.stderr = FALSE,
    command = paste(
      if (!is.null(quality_tag)) {
        paste(bin_path[["vcftools"]],
          "--gzvcf", input_vcfs,
          "--get-INFO", quality_tag,
          "--out", gsub("filtered_", "excluded_", output_study),
          "&&",
          'awk \'{if($5<', quality_threshold, ') print $1"\t"$2}\'',
          paste0(gsub("filtered_", "excluded_", output_study), ".INFO"),
          ">", paste0(gsub("filtered_", "excluded_", output_study), ".exclude"),
          "&&"
        )
      },
      bin_path[["vcftools"]],
      "--gzvcf", input_vcfs,
      if (!is.null(quality_tag)) {
        paste0(
          "--exclude-positions ", gsub("filtered_", "excluded_", output_study), ".exclude"
        )
      },
      "--remove-indels",
      "--remove-filtered-all",
      "--max-missing-count 1",
      "--recode",
      "--recode-INFO-all",
      "--stdout",
      "|", bin_path[["bgzip"]], "-c >", output_study,
      "&&",
      bin_path[["tabix"]], "-p vcf", output_study,
      "&&",
      bin_path[["vcftools"]],
      "--gzvcf", ref1kg_vcfs,
      "--maf", ref1kg_maf,
      "--recode",
      "--stdout",
      "|", bin_path[["bgzip"]], "-c >", output_ref,
      "&&",
      bin_path[["tabix"]], "-p vcf", output_ref,
      "&&",
      bin_path[["bcftools"]], "isec",
      "--collapse none",
      "--nfiles=2",
      output_study,
      output_ref,
      "--output-type z",
      "--prefix", paste0(temp_directory, "/isec"),
      "&&",
      bin_path[["bcftools"]], "merge --merge none",
      paste0(temp_directory, "/isec/0000.vcf.gz"),
      paste0(temp_directory, "/isec/0001.vcf.gz"),
      "--output-type z",
      "--output", output_merge,
      "&&",
      bin_path[["tabix"]], "-p vcf", output_merge
    )
  )

  unlink(x = temp_directory, recursive = TRUE)

  invisible()
}


#' merge_vcf
#'
#' @inheritParams estimate_ethnicity
#'
#' @keywords internal
merge_vcf <- function(input_vcfs, bin_path) {
  if (length(input_vcfs) > 1) {
    output_temp <- paste0(tempdir(), "/samples_merged.vcf.gz")
    vcf_list <- paste0(tempdir(), "/samples_merged.txt")
    cat(input_vcfs, sep = "\n", file = vcf_list)
    system(
      intern = TRUE, wait = TRUE,
      command = paste(
        bin_path[["bcftools"]], "merge --merge none",
        " --file-list", vcf_list,
        "--output-type z",
        "--output", output_temp,
        "&&",
        bin_path[["tabix"]], "-p vcf", output_temp
      )
    )
    unlink(vcf_list)
  } else {
    output_temp <- input_vcfs
  }

  output_temp
}

#' format_array_chr
#'
#' @inheritParams estimate_ethnicity
#'
#' @keywords internal
format_array_chr <- function(
  cohort_name,
  input_vcfs,
  output_directory,
  ref1kg_vcfs,
  ref1kg_maf,
  quality_tag,
  quality_threshold,
  n_cores,
  bin_path
) {
  out <- parallel::mclapply(
    X = 1:22,
    mc.preschedule = FALSE,
    mc.cores = min(parallel::detectCores(), n_cores),
    mc_input_vcfs = input_vcfs,
    mc_ref1kg_vcfs = ref1kg_vcfs,
    mc_ref1kg_maf = ref1kg_maf,
    mc_quality_tag = quality_tag,
    mc_quality_threshold = quality_threshold,
    mc_output_directory = output_directory,
    FUN = function(
      ichr,
      mc_input_vcfs,
      mc_ref1kg_vcfs,
      mc_ref1kg_maf,
      mc_quality_tag,
      mc_quality_threshold,
      mc_output_directory
    ) {
      ipattern <- paste0("^[^0-9]*chr", ichr, "[^0-9]+.*vcf.gz$")
      iinput_vcfs <- mc_input_vcfs[grep(pattern = gsub("chr", "", ipattern), x = basename(mc_input_vcfs))]
      iref1kg_vcfs <- mc_ref1kg_vcfs[grep(pattern = ipattern, x = basename(mc_ref1kg_vcfs))]

      format_vcf(
        input_vcfs = iinput_vcfs,
        ref1kg_vcfs = iref1kg_vcfs,
        ref1kg_maf = mc_ref1kg_maf,
        ichr = ichr,
        quality_tag = mc_quality_tag,
        quality_threshold = mc_quality_threshold,
        output_directory = mc_output_directory,
        bin_path = bin_path
      )
  })


  temp_file <- tempfile(fileext = ".merge")
  cat(
    list.files(path = output_directory, pattern = "_merged.vcf.gz$", full.names = TRUE),
    sep = "\n",
    file = temp_file
  )
  system(
    intern = TRUE, wait = TRUE,
    command = paste(
      bin_path[["bcftools"]], "concat",
      "--file-list", temp_file,
      "--allow-overlaps",
      "--output-type z",
      "--output", paste0(output_directory, "/all.vcf.gz")
    )
  )
  unlink(x = temp_file, recursive = TRUE)


  system(
    intern = TRUE, wait = TRUE,
    command = paste(
      bin_path[["plink1.9"]],
      "--vcf", paste0(output_directory, "/all.vcf.gz"),
      "--snps-only",
      "--maf", 0.01,
      "--hwe 0.0001",
      "--geno 0.1",
      "--make-bed",
      "--double-id",
      "--out", paste0(output_directory, "/all")
    )
  )

  unlink(
    x = list.files(path = output_directory, pattern = "_merged.vcf.gz", full.names = TRUE),
    recursive = TRUE
  )
  unlink(
    x = list.files(path = output_directory, pattern = "all.vcf.gz", full.names = TRUE),
    recursive = TRUE
  )

  invisible()
}


#' format_array_all
#'
#' @inheritParams estimate_ethnicity
#'
#' @keywords internal
format_array_all <- function(
  cohort_name,
  input_vcfs,
  output_directory,
  ref1kg_vcfs,
  ref1kg_maf,
  quality_tag,
  quality_threshold,
  bin_path
) {
  format_vcf(
    input_vcfs = input_vcfs,
    ref1kg_vcfs = ref1kg_vcfs,
    ref1kg_maf = ref1kg_maf,
    ichr = "ALL",
    quality_tag = quality_tag,
    quality_threshold = quality_threshold,
    output_directory = output_directory,
    bin_path = bin_path
  )

  system(
    intern = TRUE, wait = TRUE,
    command = paste(
      bin_path[["plink1.9"]],
      "--vcf", list.files(path = output_directory, pattern = "_merged.vcf.gz$", full.names = TRUE),
      "--snps-only",
      "--maf", ref1kg_maf,
      "--hwe 0.0001",
      "--geno 0.1",
      "--make-bed",
      "--double-id",
      "--out", paste0(output_directory, "/all")
    )
  )

  unlink(
    x = list.files(path = output_directory, pattern = "_merged.vcf.gz", full.names = TRUE),
    recursive = TRUE
  )

  invisible()
}


#' format_sequencing
#'
#' @inheritParams estimate_ethnicity
#'
#' @keywords internal
format_sequencing <- function(
  cohort_name,
  input_vcfs,
  output_directory,
  ref1kg_vcfs,
  ref1kg_maf,
  bin_path
) {
  merged_vcfs <- merge_vcf(
    input_vcfs = input_vcfs,
    bin_path = bin_path
  )

  format_vcf(
    input_vcfs = merged_vcfs,
    ref1kg_vcfs = ref1kg_vcfs,
    ref1kg_maf = ref1kg_maf,
    ichr = "ALL",
    quality_tag = NULL,
    quality_threshold = NULL,
    output_directory = output_directory,
    bin_path = bin_path
  )

  system(
    intern = TRUE, wait = TRUE,
    command = paste(
      bin_path[["plink1.9"]],
      "--vcf", list.files(path = output_directory, pattern = "_merged.vcf.gz$", full.names = TRUE),
      "--snps-only",
      "--maf", ref1kg_maf,
      "--hwe 0.0001",
      "--geno 0.1",
      "--make-bed",
      "--double-id",
      "--out", paste0(output_directory, "/all")
    )
  )

  unlink(
    x = list.files(path = output_directory, pattern = "_merged.vcf.gz", full.names = TRUE),
    recursive = TRUE
  )

  invisible()
}


#' compute_pca
#'
#' @inheritParams estimate_ethnicity
#' @param input_plink A `character`. The path to plink format files (i.e., `.bed`, `.bim` and `.fam` files).
#'
#' @keywords internal
compute_pca <- function(cohort_name, input_plink, output_directory, ref1kg_population) {
  message_prefix <- "[CARoT] "

  ######################
  ### Performing PCA ###
  ######################
  message(message_prefix, "Performing PCA ...")

  res_pca <- flashpcaR::flashpca(input_plink, ndim = 10)

  pca_gg <- as.data.frame(res_pca[["projection"]])
  colnames(pca_gg) <- paste0("PC", sprintf("%02d", seq_len(ncol(pca_gg))))

  pca_gg <- dplyr::as_tibble(pca_gg)

  fid_iid <- utils::read.table(paste0(input_plink, ".fam"))[, c(1, 2)]

  if (all.equal(fid_iid[[1]], fid_iid[[2]])) {
    pca_gg[["sample"]] <- fid_iid[[2]]
  } else {
    pca_gg[["sample"]] <- paste(fid_iid[[1]], fid_iid[[2]], sep = "/")
  }

  pca_gg <- dplyr::left_join(
    x = pca_gg,
    y = suppressWarnings(
      readr::read_tsv(
        file = ref1kg_population,
        col_types = readr::cols_only(
          sample = readr::col_character(),
          pop = readr::col_character(),
          super_pop = readr::col_character()
        )
      )
    ),
    by = "sample"
  )
  pca_gg[["cohort"]] <- factor(
    x = ifelse(is.na(pca_gg[["pop"]]), cohort_name, "1,000 Genomes"),
    levels = c(cohort_name, "1,000 Genomes")
  )
  pca_gg[["pop"]] <- ifelse(is.na(pca_gg[["pop"]]), "Unknown", pca_gg[["pop"]])
  pca_gg[["super_pop"]] <- factor(
    x = ifelse(is.na(pca_gg[["super_pop"]] ), cohort_name, pca_gg[["super_pop"]]),
    levels = c(cohort_name, "AFR", "AMR", "EAS", "SAS", "EUR")
  )
  pca_gg <- dplyr::select(.data = pca_gg, "sample", dplyr::everything())

  p_ethni <- ggplot2::ggplot(
    data = pca_gg,
    mapping = ggplot2::aes_string(x = "PC01", y = "PC02", colour = "super_pop")
  ) +
    ggplot2::theme_light(base_size = 12) +
  	ggplot2::geom_hline(yintercept = 0, linetype = 1, size = 0.5, na.rm = TRUE) +
  	ggplot2::geom_vline(xintercept = 0, linetype = 1, size = 0.5, na.rm = TRUE) +
    ggforce::geom_mark_ellipse(mapping = ggplot2::aes_string(fill = "super_pop"), con.cap = 0) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(shape = "super_pop"), na.rm = TRUE) +
  	ggplot2::scale_colour_viridis_d(na.translate = FALSE, drop = FALSE, end = 0.9) +
    ggplot2::scale_fill_viridis_d(na.translate = FALSE, drop = FALSE, end = 0.9) +
    ggplot2::scale_shape_manual(values = c(3, rep(1, 5))) +
    ggplot2::labs(
      shape = NULL,
      colour = NULL,
      fill = NULL,
      caption = paste(
        "SNPs:",
        scales::comma(R.utils::countLines(paste0(input_plink, ".bim")))
      )
    ) +
    ggforce::facet_zoom(
      xlim = range(dplyr::filter(pca_gg, !!dplyr::sym("cohort")==!!cohort_name)[["PC01"]]),
      ylim = range(dplyr::filter(pca_gg, !!dplyr::sym("cohort")==!!cohort_name)[["PC02"]]),
      zoom.size = 0.5,
      horizontal = FALSE
    )

  pca_gg_pred <- pca_gg %>%
    dplyr::filter(!!dplyr::sym("cohort") == !!cohort_name) %>%
    dplyr::mutate(
      super_pop_centre = pca_gg %>%
        dplyr::filter(!!dplyr::sym("cohort") != !!cohort_name) %>%
        dplyr::select(-"cohort") %>%
        dplyr::group_by(!!dplyr::sym("super_pop")) %>%
        dplyr::summarise(
          PC01 = mean(!!dplyr::sym("PC01")),
          PC02 = mean(!!dplyr::sym("PC02"))
        ) %>%
        list()
    ) %>%
    dplyr::group_by(sample) %>%
    dplyr::mutate(
      super_pop_pred = purrr::map(
        .x = !!dplyr::sym("PC01"),
        .y = !!dplyr::sym("PC02"),
        .centre = !!dplyr::sym("super_pop_centre"),
        .f = function(.x, .y, .centre) {
          dist_pop <- sqrt(
            (.x - .centre[[1]][["PC01"]])^2 +
              (.y - .centre[[1]][["PC02"]])^2
          )
          names(dist_pop) <- as.character(.centre[[1]][["super_pop"]])

          out <- data.frame(
            pop_pred = names(dist_pop[which.min(dist_pop)]),
            dist = t(dist_pop)
          )
          colnames(out) <- gsub(".", "_", colnames(out), fixed = TRUE)
          out
        }
      ),
      super_pop_centre = NULL
    ) %>%
    tidyr::unnest() %>%
    dplyr::ungroup()


  #################
  ### Exporting ###
  #################
  message(message_prefix, "Exporting ...")
  ggplot2::ggsave(
    filename = paste0(output_directory, "/", cohort_name, "_ethnicity.tiff"),
    plot = p_ethni,
    width = 6.3,
    height = 4.7 * 1.5,
    units = "in",
    dpi = 300
  )

  invisible(
    readr::write_csv(
      x = pca_gg_pred,
      path = paste0(output_directory, "/", cohort_name, "_ethnicity.csv")
    )
  )
}
