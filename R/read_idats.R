#' Efficiently import idats files mostly using minfi functions.
#'
#' @param directory A `character`. Location of IDAT files, default is the current working directory.
#' @param csv_file A `character`. Path to the sample sheet (csv files) or
#'     name of the sample sheet in `directory`.
#' @param meth_value_type A `character`. Indicates whether you prefer m-values (`"M"`)
#'     or beta-values (`"B"`). Default is `"B"`.
#' @param array_name A `character`. Choose microarray type, eiyther `"450K"` or `"EPIC"`.
#'     Default is `"EPIC"`.
#' @param annotation_version A `character`. Version of the annotation package that should be used.
#'     Default is `"ilm10b4.hg19"` for the `"EPIC"` array
#' @param n_cores An `integer`. The number of cores to use,
#'     i.e., at most how many child processes will be run simultaneously.
#' @param rgSet A `RGChannelSet` object.
#'
#' @inheritParams qc_idats
#'
#' @return A `list`.
#' @export
read_idats <- function(
  directory = getwd(),
  csv_file = "csv$",
  meth_value_type = "B",
  filter_beads = TRUE,
  bead_cutoff = 0.05,
  filter_non_cpg = TRUE,
  filter_snps = TRUE,
  population = NULL,
  filter_multihit = TRUE,
  filter_xy = TRUE,
  detection_pvalues = 0.01,
  filter_callrate = TRUE,
  callrate_samples = 0.99,
  callrate_probes = 1,
  norm_background = "oob",
  norm_dye = "RELIC",
  norm_quantile = "quantile1",
  array_name = c("EPIC", "450k"),
  annotation_version = c("ilm10b4.hg19", "ilmn12.hg19"),
  n_cores = 1,
  rgSet = NULL
) {
  array_name <- array_name[1]
  annotation_version <- annotation_version[1]

  stopifnot(suppressPackageStartupMessages(
    requireNamespace("minfi") &
      switch(
        EXPR = array_name,
        "450k" = requireNamespace("IlluminaHumanMethylation450kmanifest"),
        "EPIC" = requireNamespace("IlluminaHumanMethylationEPICmanifest")
      )
  ))

  message(
    "==============================", "\n",
    "[CARoT] ", "Reading IDAT files ...", "\n",
    "=============================="
  )
  if (is.null(rgSet) | !inherits(rgSet, "RGChannelSet")) {
    sample_sheet <- read_sample_sheet(directory = directory, csv_file = csv_file)
    rgSet <- read_metharray_exp(sample_sheet = sample_sheet)
  }
  rgSet@annotation <- switch(
    EXPR = array_name,
    "450k" = c(array = "IlluminaHumanMethylation450k", annotation = annotation_version),
    "EPIC" = c(array = "IlluminaHumanMethylationEPIC", annotation = annotation_version)
  )

  message(
    "\n", "\n",
    "====================================", "\n",
    "[CARoT] ", "Preprocessing IDAT files ...", "\n",
    "===================================="
  )
  minfi::sampleNames(rgSet) <- rgSet[[1]]

  data_detP <- minfi::detectionP(rgSet)
  data_detP[is.na(data_detP)] <- 1

  message(
    "\n",
    "================================", "\n",
    "[CARoT] ", "Filtering IDAT files ...", "\n",
    "================================"
  )

  if (filter_callrate) {
    good_detection <- data_detP < detection_pvalues

    call_rate_samples <- colSums(good_detection) / nrow(good_detection)
    bad_samples <- names(which(call_rate_samples < callrate_samples))
    message(
      "Filtering samples with call rate below ", scales::percent(callrate_samples), ":\n",
      "  - ", scales::comma(length(bad_samples)), " samples were discarded"
    )

    good_detection <- good_detection[, setdiff(colnames(good_detection), bad_samples)]

    call_rate_cpg <- rowSums(good_detection) / ncol(good_detection)
    bad_cpgs <- names(which(call_rate_cpg < callrate_probes))
    message(
      "Filtering probes with call rate below ", scales::percent(bead_cutoff), ":\n",
      "  - ", scales::comma(length(bad_cpgs)), " probes were discarded"
    )
  } else {
    bad_samples <- NULL
    bad_cpgs <- NULL
  }

  trash <- utils::capture.output({
    mset <- suppressMessages(ENmix::preprocessENmix(
      rgSet = rgSet,
      bgParaEst = norm_background,
      dyeCorr = norm_dye,
      QCinfo = NULL,
      exQCsample = FALSE,
      exQCcpg = FALSE,
      exSample = bad_samples,
      exCpG = bad_cpgs,
      nCores = n_cores
    ))
    mset <- suppressMessages(ENmix::norm.quantile(mdat = mset, method = norm_quantile))
  })

  mset@metadata[["phenotypes"]] <- rgSet %>%
    minfi::pData() %>%
  	as.data.frame() %>%
  	dplyr::mutate(
  	  Sample_ID = as.character(get("Sample_ID")),
  		mean_detection_pvalue = colMeans(data_detP)[get("Sample_ID")],
  		call_rate = (colSums(data_detP < detection_pvalues) / nrow(data_detP))[get("Sample_ID")]
  	) %>%
    S4Vectors::DataFrame()

  if (filter_beads) {
    bc <- get_beadcount(rgSet)
    bc2 <- bc[rowSums(is.na(bc)) < bead_cutoff * (ncol(bc)), ]
    mset_f2 <- mset[minfi::featureNames(mset) %in% rownames(bc2), ]
    message(
      "Filtering probes with a beadcount <3 in at least ", scales::percent(bead_cutoff), " of samples:\n",
      "  - ", scales::comma(dim(mset)[1] - dim(mset_f2)[1]), " probes were discarded"
    )
    mset <- mset_f2
  }

  if (filter_non_cpg) {
    mset_f2 <- minfi::dropMethylationLoci(mset, dropCH = TRUE)
    message(
      "Filtering non-cg probes:\n",
      "  - ", scales::comma(dim(mset)[1] - dim(mset_f2)[1]), " probes were discarded"
    )
    mset <- mset_f2
  }

  if (filter_snps) {
    manifest_hg19 <- switch(
      EXPR = array_name,
      "450k" = {
        get(utils::data("hm450.manifest.hg19", package = "ChAMPdata"))
      },
      "EPIC" = {
        get(utils::data("EPIC.manifest.hg19", package = "ChAMPdata"))
      }
    )

    ref_population <- c(
      "AFR", "EAS", "EUR",
      "SAS", "AMR", "GWD", "YRI", "TSI", "IBS",
      "CHS", "PUR", "JPT", "GIH", "CHB", "STU",
      "ITU", "LWK", "KHV", "FIN", "ESN", "CEU",
      "PJL", "ACB", "CLM", "CDX", "GBR", "BEB",
      "PEL", "MSL", "MXL", "ASW"
    )

    if (is.null(population) || !(population %in% ref_population)) {
      which_population <- which(manifest_hg19$MASK_general)
    } else {
      which_population <- which(manifest_hg19[, paste("MASK_general", population, sep = ".")])
    }
    maskname <- rownames(manifest_hg19)[which_population]
    mset_f2 <- mset[!minfi::featureNames(mset) %in% maskname, ]
    message(
      "Filtering probes with SNPs (Zhou et al., 2016; doi:10.1093/nar/gkw967):\n",
      "  - ", scales::comma(dim(mset)[1] - dim(mset_f2)[1]), " probes were discarded"
    )
    mset <- mset_f2
  }

  if (filter_multihit) {
    multi_hit <- get(utils::data("multi.hit", package = "ChAMPdata"))
    mset_f2 <- mset[!minfi::featureNames(mset) %in% multi_hit$TargetID, ]
    message(
      "Filtering probes that align to multiple locations (Nordlund et al., 2013; doi:10.1186/gb-2013-14-9-r105):\n",
      "  - ", scales::comma(dim(mset)[1] - dim(mset_f2)[1]), " probes were discarded"
    )
    mset <- mset_f2
  }

  if (filter_xy) {
    switch(
      EXPR = array_name,
      "450k" = utils::data("probe.features", package = "ChAMPdata"),
      "EPIC" = utils::data("probe.features.epic", package = "ChAMPdata")
    )
    probe_features <- get("probe.features")
    autosomes <- probe_features[!probe_features$CHR %in% c("X", "Y"), ]
    mset_f2 <- mset[minfi::featureNames(mset) %in% rownames(autosomes), ]
    message(
      "Filtering probes on the X or Y chromosome:\n",
      "  - ", scales::comma(dim(mset)[1] - dim(mset_f2)[1]), " probes were discarded"
    )
    mset <- mset_f2
  }


  if (meth_value_type == "B") {
    methylation_matrix <- minfi::getBeta(mset, "Illumina")
  } else {
    methylation_matrix <- minfi::getM(mset)
  }

  if (min(methylation_matrix, na.rm = TRUE) <= 0) {
    methylation_matrix[methylation_matrix <= 0] <- min(methylation_matrix[methylation_matrix > 0])
  }
  message(
    "Zeros have been replaced with smallest value over zero."
  )
  if (max(methylation_matrix, na.rm = TRUE) >= 1) {
    methylation_matrix[methylation_matrix >= 1] <- max(methylation_matrix[methylation_matrix < 1])
  }
  message(
    "Ones have been replaced with largest value below one."
  )

  message(
    "\n",
    "================================", "\n",
    "[CARoT] ", "Exporting IDAT files ...", "\n",
    "================================"
  )

  message(
    "Data contains:\n",
    "  - ", scales::comma(dim(methylation_matrix)[1]), " probes\n",
    "  - ", scales::comma(dim(methylation_matrix)[2]), " samples\n",
    "  - ", scales::comma(sum(is.na(methylation_matrix))), " missing values"
  )

  colnames(methylation_matrix) <- minfi::pData(mset)[["Sample_ID"]]

  switch(
    EXPR = meth_value_type,
    "B" = {
      mset@metadata[["beta_values"]] <- methylation_matrix
    },
    "M" = {
      mset@metadata[["M_values"]] <- methylation_matrix
    }
  )

  list(mset = mset, rgset = rgSet)
}


#' read_sample_sheet
#'
#' @inheritParams read_idats
#' @param ignore.case A `logical`. A logical value.
#'     If `TRUE`, the directory path is prepended to the file names to give a relative file path.
#'     If `FALSE`, the file names (rather than paths) are returned.
#' @param recursive A `logical`. Should the listing recurse into directories?
#' @param full.names A `logical`. Should pattern-matching be case-insensitive?
#'
#' @keywords internal
read_sample_sheet <- function(
  directory,
  csv_file = "csv$",
  ignore.case = TRUE,
  recursive = TRUE,
  full.names = TRUE
) {
  if (file.exists(suppressWarnings(normalizePath(csv_file)))) {
    list_files <- normalizePath(csv_file)
  } else {
    list_files <- list.files(
      path = directory,
      pattern = csv_file,
      full.names = full.names,
      ignore.case = ignore.case,
      recursive = recursive
    )
    if (length(list_files)>1) {
      warnings("[CARoT] ", "More than one CSV file have been found!")
      list_files <- list.files[1]
      message("[CARoT] ", "File '", list.files, "' will be used.")
    }
  }

  data_header <- grep("^\\[DATA\\]", readLines(list_files), ignore.case = TRUE)
  if (length(data_header) == 0) {
    data_header <- 0
  }
  col_names <- colnames(utils::read.csv(file = list_files, stringsAsFactor = FALSE, skip = data_header, nrows = 1))
  default_cols <- c("Sample_ID", "Sentrix_ID", "Sentrix_Position")

  cols_missing <- default_cols[!default_cols%in%col_names]
  if (length(cols_missing)!=0) {
    stop(
      "[CARoT] ",
      "Sample Sheet must contains the following missing columns:\n",
      "  - ", paste(cols_missing, collapse = "\n  - ")
    )
  }

  sample_sheet <- utils::read.csv(list_files, stringsAsFactor = FALSE, skip = data_header)

  new_cols <- c("Sample_ID", "Slide", "Array", "Sample_Plate", "Sample_Well")
  names(new_cols) <- c("Sample_ID", "Sentrix_ID", "Sentrix_Position", "Sample_Plate", "Sample_Well")
  for (idef in names(new_cols)) {
    sample_sheet[[new_cols[idef]]] <- as.character(sample_sheet[[idef]])
  }

  basenames <- sapply(
    X = paste0(sample_sheet[["Slide"]], "_", sample_sheet[["Array"]], "_Grn.idat"),
    FUN = grep,
    x = list.files(path = directory, recursive = recursive, full.names = TRUE),
    value = TRUE,
    USE.NAMES = FALSE
  )
  sample_sheet[["Basename"]] <- sub("_Grn\\.idat.*", "", basenames, ignore.case = TRUE)

  sample_sheet
}

#' read_metharray
#'
#' @param basenames The `basenames` or `filenames` of the IDAT files.
#'     `basenames` are the filename without the ending `_Grn.idat` or `_Red.idat`.
#'     `filenames` are filenames including `_Grn.idat` or `_Red.idat`.
#'
#' @keywords internal
read_metharray <- function(basenames) {
  basenames <- unique(sub("_[GR][re][nd]\\.idat.*", "", basenames))

  p <- dplyr::progress_estimated(length(basenames)*2+6)

  for (ichannel in c("Grn", "Red")) {
    i_files <- paste0(basenames, "_", ichannel, ".idat")
    names(i_files) <- basename(basenames)
    i_files_exists <- file.exists(i_files)
    if (!all(i_files_exists)) {
      i_filesgz_exists <- file.exists(paste0(i_files, ".gz"))
      if (!all(i_filesgz_exists)) {
        stop(
          # "[CARoT] ",
          "The following specified files do not exist:\n",
          "  - ", paste(i_files[!i_files_exists], collapse = "\n  - ")
        )
      }
      i_files <- paste0(i_files, ".gz")
    }
    i_idats <- lapply(
      X = i_files,
      FUN = function(x) {
        p$pause(0.1)$tick()$print()
        illuminaio::readIDAT(x)
      }
    )
    assign(x = gsub("(^.).*", "\\1_idats", ichannel), value = i_idats)
    p$pause(0.1)$tick()$print()
  }

  common_addresses <- as.character(Reduce("intersect", lapply(
    X = get("G_idats"),
    FUN = function(x) rownames(x$Quants)
  )))

  p$pause(0.1)$tick()$print()

  GreenMean <- do.call("cbind", lapply(
    X = get("G_idats"),
    y = common_addresses,
    FUN = function(x, y) x$Quants[y, "Mean"]
  ))

  p$pause(0.1)$tick()$print()

  RedMean <- do.call("cbind", lapply(
    X = get("R_idats"),
    y = common_addresses,
    FUN = function(x, y) x$Quants[y, "Mean"]
  ))

  p$pause(0.1)$tick()$print()


  GreenSD <- do.call("cbind", lapply(
    X = get("G_idats"),
    y = common_addresses,
    FUN = function(x, y) x$Quants[common_addresses, "SD"]
  ))
  RedSD <- do.call("cbind", lapply(
    X = get("R_idats"),
    y = common_addresses,
    FUN = function(x, y) x$Quants[common_addresses, "SD"]
  ))
  NBeads <- do.call("cbind", lapply(
    X = get("G_idats"),
    y = common_addresses,
    FUN = function(x, y) x$Quants[common_addresses, "NBeads"]
  ))
  out <- minfi::RGChannelSetExtended(
    Red = RedMean,
    Green = GreenMean,
    RedSD = RedSD,
    GreenSD = GreenSD,
    NBeads = NBeads
  )

  rownames(out) <- common_addresses

  p$pause(0.1)$tick()$print()

  out
}

#' read_metharray_exp
#'
#' @inheritParams read_idats
#' @inheritParams read_metharray
#' @inheritParams read_sample_sheet
#'
#' @keywords internal
read_metharray_exp <- function(
  directory = NULL,
  sample_sheet = NULL,
  ignore.case = TRUE,
  recursive = TRUE,
  full.names = TRUE
) {
  if (is.null(sample_sheet)) {
    Grn_files <- list.files(
      path = directory,
      pattern = "_Grn.idat$",
      recursive = recursive,
      ignore.case = ignore.case,
      full.names = full.names
    )
    Red_files <- list.files(
      path = directory,
      pattern = "_Red.idat$",
      recursive = recursive,
      ignore.case = ignore.case,
      full.names = full.names
    )

    if (length(Grn_files) == 0 || length(Red_files) == 0) {
      stop(
        # "[CARoT] ",
        "IDAT files must be provided."
      )
    }

    commonFiles <- intersect(
      sub("_Grn.idat$", "", Grn_files),
      sub("_Red.idat$", "", Red_files)
    )

    if (length(commonFiles) == 0) {
      stop(
        # "[CARoT] ",
        '"Grn" and "Red" idats files must be provided.'
      )
    }

    commonFiles_Grn <- paste0(commonFiles, "_Grn.idat")
    if (!setequal(commonFiles_Grn, Grn_files)) {
      warning(
        # "[CARoT] ",
        "The following files only exists for the green channel:\n",
        "  - ", paste(setdiff(Grn_files, commonFiles_Grn), collapse = "\n  - ")
      )
    }

    commonFiles_Red <- paste0(commonFiles, "_Red.idat")
    if (!setequal(commonFiles_Red, Red_files)) {
       warning(
        # "[CARoT] ",
        "The following files only exists for the red channel:\n",
        "  - ", paste(setdiff(Red_files, commonFiles_Red), collapse = "\n  - ")
      )
    }

    rgSet <- read_metharray(basenames = commonFiles)
  } else {
    if (!"Basename" %in% names(sample_sheet)) {
      stop(
        # "[CARoT] ",
        '"Basename" must be provided as a column of "sample_sheet".'
      )
    }

    if (!is.null(directory)) {
      files <- file.path(directory, basename(sample_sheet$Basename))
    } else {
      files <- sample_sheet$Basename
    }

    rgSet <- read_metharray(basenames = files)

    pD <- sample_sheet
    pD$filenames <- files
    rownames(pD) <- colnames(rgSet)
    SummarizedExperiment::colData(rgSet) <- methods::as(pD, "DataFrame")
  }

  rgSet
}

#' get_beadcount
#'
#' @param x `rgSet`
#'
#' @keywords internal
get_beadcount <- function(x) {
  nb <- minfi::getNBeads(x)
  typeI <- minfi::getProbeInfo(x, type = "I")
  typeII <- minfi::getProbeInfo(x, type = "II")
  locus_names <- minfi::getManifestInfo(x, "locusNames")

  bc_temp <- matrix(
    data = NA_real_,
    ncol = ncol(x),
    nrow = length(locus_names),
    dimnames = list(locus_names, minfi::sampleNames(x))
  )
  bc_temp[typeII$Name, ] <- nb[typeII$AddressA, ]
  bc_temp[typeI$Name, ] <- nb[typeI$AddressB, ]
  bc_temp[typeI$Name, ] <- nb[typeI$AddressA, ]
  bc_temp[which(nb[typeI$AddressA, ] < 3 | nb[typeI$AddressB, ] < 3)] <- NA

  data.frame(bc_temp)
}
