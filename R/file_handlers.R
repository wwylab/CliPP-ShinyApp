library(shiny)
library(dplyr)
library(purrr)

#' Read and validate input files
#' @param snvFile Path to SNV file
#' @param cnaFile Path to CNA file
#' @param purityFile Path to purity file
#' @return List containing validated data or NULL if validation fails
read_and_validate_files <- function(snvFile, cnaFile, purityFile) {
  if (is.null(snvFile) || is.null(cnaFile) || is.null(purityFile)) {
    showNotification("One or more required files are missing.", type = "error")
    return(NULL)
  }

  df_snv <- read.table(snvFile, header = TRUE)
  df_cna <- read.table(cnaFile, header = TRUE)
  purity <- as.numeric(readLines(purityFile, warn = FALSE))

  required_cols <- c("chromosome_index", "position", "ref_count", "alt_count")
  if (!all(required_cols %in% colnames(df_snv))) {
    missing_cols <- required_cols[!required_cols %in% colnames(df_snv)]
    showNotification(paste("The SNV file does not contain the required columns:", paste(missing_cols, collapse = ", ")), type = "error")
    return(NULL)
  }

  list(df_snv = df_snv, df_cna = df_cna, purity = purity)
}

#' Store uploaded files in a structured format
#' @param input Shiny input object
#' @param numSamples Number of samples
#' @param uploadMode Upload mode ("all_at_once" or "individual")
#' @return List containing organized file paths
# ---------------------------------------------------------------------------
# Bulk upload: one file picker for everything, samples inferred from the names
#
# A user selects every file for every sample in one go. Each file is matched to
# a role by its suffix (snv/cna/purity) and to a sample by what precedes that
# suffix, so "TCGA-18-5592.snv.txt" is the SNV file of sample TCGA-18-5592.
# Nothing has to be declared up front -- not the sample count, not the names.
# ---------------------------------------------------------------------------

# role -> the word that must appear before ".txt"
UPLOAD_ROLES <- c(snvFile = "snv", cnaFile = "cna", purityFile = "purity")

#' Split one uploaded filename into its role and its sample stem
#' @return list(role, stem) or NULL when the name matches no role
parse_uploaded_filename <- function(name) {
  base <- basename(name)
  for (role in names(UPLOAD_ROLES)) {
    pattern <- sprintf("^(.*?)[._-]?%s\\.txt$", UPLOAD_ROLES[[role]])
    m <- regmatches(base, regexec(pattern, base, ignore.case = TRUE))[[1]]
    if (length(m) == 2) return(list(role = role, stem = m[[2]]))
  }
  NULL
}

#' Group a flat list of uploaded files into samples
#'
#' @param names  Original filenames, as uploaded
#' @param paths  Temp paths Shiny wrote them to, same order
#' @return list(samples, incomplete, unrecognized) where `samples` holds only
#'   the samples that have all three files. `incomplete` and `unrecognized`
#'   describe what was left out, so the caller can say so instead of failing
#'   silently.
group_uploaded_files <- function(names, paths) {
  parsed <- lapply(names, parse_uploaded_filename)
  unrecognized <- names[vapply(parsed, is.null, logical(1))]

  grouped <- list()
  for (i in seq_along(names)) {
    p <- parsed[[i]]
    if (is.null(p)) next
    # Files named plainly "snv.txt" have no stem; treat them as one sample.
    stem <- if (nzchar(p$stem)) p$stem else "Sample"
    if (is.null(grouped[[stem]])) grouped[[stem]] <- list()
    # First file wins if a role is uploaded twice for one sample.
    if (is.null(grouped[[stem]][[p$role]])) grouped[[stem]][[p$role]] <- paths[[i]]
  }

  samples <- list()
  incomplete <- list()
  for (stem in names(grouped)) {
    have <- grouped[[stem]]
    missing <- setdiff(names(UPLOAD_ROLES), names(have))
    if (length(missing) == 0) {
      samples[[stem]] <- have[names(UPLOAD_ROLES)]
    } else {
      incomplete[[stem]] <- sub("File$", "", missing)
    }
  }

  list(samples = samples, incomplete = incomplete, unrecognized = unrecognized)
}

#' One-line summary of what a bulk selection contained
bulk_upload_summary <- function(grouping) {
  n <- length(grouping$samples)
  parts <- character(0)
  if (n > 0) {
    parts <- c(parts, sprintf("%d sample%s ready: %s", n, if (n == 1) "" else "s",
                              paste(names(grouping$samples), collapse = ", ")))
  }
  if (length(grouping$incomplete) > 0) {
    parts <- c(parts, paste0(
      "Missing files: ",
      paste(sprintf("%s (no %s file)", names(grouping$incomplete),
                    vapply(grouping$incomplete, paste, character(1), collapse = " or ")),
            collapse = "; ")))
  }
  if (length(grouping$unrecognized) > 0) {
    parts <- c(parts, sprintf("Ignored %d file%s that did not end in snv.txt, cna.txt or purity.txt: %s",
                              length(grouping$unrecognized),
                              if (length(grouping$unrecognized) == 1) "" else "s",
                              paste(grouping$unrecognized, collapse = ", ")))
  }
  if (length(parts) == 0) "No files selected yet." else paste(parts, collapse = " · ")
}

store_uploaded_files <- function(input, numSamples, uploadMode) {
  samples <- list()

  for (i in 1:numSamples) {
    sampleName <- input[[paste0("sampleName", i)]]

    if (uploadMode == "all_at_once") {
      files <- input[[paste0("fileInput", i)]]
      if (!is.null(files)) {
        uploadedFiles <- files$datapath
        uploadedNames <- files$name

        samples[[sampleName]] <- list(
          snvFile = uploadedFiles[grepl("snv.txt", uploadedNames)],
          cnaFile = uploadedFiles[grepl("cna.txt", uploadedNames)],
          purityFile = uploadedFiles[grepl("purity.txt", uploadedNames)]
        )
      }
    } else {
      snvFile <- input[[paste0("snvFile", i)]]
      cnaFile <- input[[paste0("cnaFile", i)]]
      purityFile <- input[[paste0("purityFile", i)]]

      if (!is.null(snvFile) && !is.null(cnaFile) && !is.null(purityFile)) {
        samples[[sampleName]] <- list(
          snvFile = snvFile$datapath,
          cnaFile = cnaFile$datapath,
          purityFile = purityFile$datapath
        )
      }
    }
  }

  return(samples)
}

#' Process uploaded sample files
#' @param sampleFiles List containing file paths for SNV, CNA, and purity files
#' @param selectedSample Name of the sample being processed
#' @return List containing processing success status and message
# Above this many SNVs a run is too slow and memory-hungry for the web app;
# CliPP's pairwise kernel grows with the square of the mutation count.
MAX_SNVS_WEB <- 30000
CLIPP_GITHUB <- "https://github.com/wwylab/CliPP"

#' Message shown when an uploaded sample exceeds MAX_SNVS_WEB
too_many_snvs_message <- function(n) {
  sprintf(paste0("This sample has %s SNVs, above the %s limit for CliPP-on-Web. ",
                 "Large samples are much faster with the command-line version of ",
                 "CliPP: %s"),
          format(n, big.mark = ","), format(MAX_SNVS_WEB, big.mark = ","), CLIPP_GITHUB)
}

# ---------------------------------------------------------------------------
# CliPP compiled kernel
#
# CliPP 1.4.0 loads its kernel with ctypes from build/*/CliPP*<platform>*.so,
# matching on R's equivalent of Python's sys.platform ("darwin", "linux").
# A .so built on one platform is invisible to the other, so THE KERNEL MUST BE
# COMPILED ON WHATEVER MACHINE SERVES THE APP:
#
#     cd forCliPP && ./build_clipp.sh          (see forCliPP/README.md)
#
# Without it CliPP exits before producing output and the user sees only
# "CliPP did not generate the expected output files", which says nothing about
# the real cause. These helpers turn that into a clear message instead.
# ---------------------------------------------------------------------------

CLIPP_DIR <- "forCliPP/CliPP"

#' Filesystem-safe run id for a sample
#'
#' CliPP writes its output to forCliPP/CliPP/<sample_id>/, so giving each sample
#' its own id is what lets a batch of samples run without overwriting each
#' other. The id is passed to run_clipp_main.py with -i, so keep it to
#' characters that are safe in both a path and a shell argument.
clipp_run_slug <- function(sample_name) {
  slug <- gsub("[^A-Za-z0-9._-]+", "_", as.character(sample_name))
  slug <- gsub("^[._]+|[._]+$", "", slug)
  if (!nzchar(slug)) slug <- "sample_id"
  substr(slug, 1, 80)
}

#' Directory holding one sample's CliPP output
clipp_run_dir <- function(sample_name, clipp_dir = CLIPP_DIR) {
  file.path(clipp_dir, clipp_run_slug(sample_name))
}

#' Platform tag CliPP's own glob matches on (Python sys.platform)
clipp_platform_tag <- function() {
  if (Sys.info()[["sysname"]] == "Darwin") "darwin" else "linux"
}

#' Path to the compiled CliPP kernel for this platform, or NA if not built
clipp_kernel_path <- function(clipp_dir = CLIPP_DIR) {
  hits <- list.files(
    file.path(clipp_dir, "build"),
    pattern = paste0("^CliPP.*", clipp_platform_tag(), ".*\\.so$"),
    recursive = TRUE, full.names = TRUE
  )
  if (length(hits) == 0) NA_character_ else hits[[which.max(file.mtime(hits))]]
}

clipp_kernel_missing_message <- function() {
  paste0("CliPP's compiled kernel is missing for this platform (",
         clipp_platform_tag(), "). Run 'cd forCliPP && ./build_clipp.sh' on ",
         "this machine and restart the app -- see forCliPP/README.md.")
}

process_sample_files <- function(sampleFiles, selectedSample) {
  snvFile <- sampleFiles$snvFile
  cnaFile <- sampleFiles$cnaFile
  purityFile <- sampleFiles$purityFile

  if (is.null(snvFile) || is.null(cnaFile) || is.null(purityFile)) {
    return(list(
      success = FALSE,
      message = "One or more required files are missing."
    ))
  }

  file_data <- read_and_validate_files(snvFile, cnaFile, purityFile)
  if (is.null(file_data)) {
    return(list(
      success = FALSE,
      message = "Error validating input files."
    ))
  }
  
  n_snvs <- nrow(file_data$df_snv)
  if (n_snvs > MAX_SNVS_WEB) {
    return(list(
      success = FALSE,
      too_large = TRUE,
      n_snvs = n_snvs,
      message = too_many_snvs_message(n_snvs)
    ))
  }

  if (is.na(clipp_kernel_path())) {
    return(list(
      success = FALSE,
      kernel_missing = TRUE,
      message = clipp_kernel_missing_message()
    ))
  }

  slug          <- clipp_run_slug(selectedSample)
  directoryPath <- paste0(file.path(CLIPP_DIR, slug), "/")

  if (!dir.exists(directoryPath)) {
    dir.create(directoryPath, recursive = TRUE, showWarnings = TRUE)
  } else {
    system(paste("rm -rf", shQuote(directoryPath)), intern = TRUE)
    dir.create(directoryPath, recursive = TRUE, showWarnings = FALSE)
  }

  finalResultPath <- file.path(directoryPath, "final_result", "Best_lambda")
  if (!dir.exists(finalResultPath)) {
    dir.create(finalResultPath, recursive = TRUE, showWarnings = FALSE)
  }

  command <- sprintf("python3 forCliPP/CliPP/run_clipp_main.py %s %s %s -i %s",
                     shQuote(snvFile), shQuote(cnaFile), shQuote(purityFile),
                     shQuote(slug))

  cat("Executing command:", command, "\n")

  tryCatch({
    result <- system(command, intern = TRUE)
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error executing CliPP command:", e$message)
    ))
  })

  resultPath <- file.path(directoryPath, "final_result", "Best_lambda")
  matching_file1 <- list.files(resultPath, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
  matching_file2 <- list.files(resultPath, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)

  if (length(matching_file1) == 0 || length(matching_file2) == 0) {
    return(list(
      success = FALSE,
      message = "CliPP did not generate the expected output files."
    ))
  }

  return(list(
    success = TRUE,
    run_dir = file.path(CLIPP_DIR, slug),
    message = "Processing complete."
  ))
}

#' Get current analysis for the selected sample
#' @param sampleFiles Files for the selected sample
#' @return Analysis data structure or NULL if processing fails
get_current_analysis <- function(sampleFiles, run_dir = NULL) {
  if (is.null(sampleFiles) || is.null(sampleFiles$snvFile) || is.null(sampleFiles$cnaFile) || is.null(sampleFiles$purityFile)) {
    return(NULL)
  }

  file_data <- read_and_validate_files(sampleFiles$snvFile, sampleFiles$cnaFile, sampleFiles$purityFile)
  if (is.null(file_data)) {
    return(NULL)
  }

  df_snv <- file_data$df_snv
  df_cna <- file_data$df_cna
  purity <- file_data$purity

  if (is.null(run_dir)) run_dir <- file.path(CLIPP_DIR, "sample_id")
  resultPath <- file.path(run_dir, "final_result", "Best_lambda")

  matching_file1 <- list.files(resultPath, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
  matching_file2 <- list.files(resultPath, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)

  if (length(matching_file1) == 0 || length(matching_file2) == 0) {
    return(NULL)
  }

  df_assigned <- read.table(matching_file1[1], header = TRUE, sep = "\t")
  df_subc <- read.table(matching_file2[1], header = TRUE, sep = "\t")

  df_snvcna <- df_snv %>%
    mutate(total_cn = map2_chr(position, chromosome_index, function(x, y) {
      inds = x >= df_cna$start_position & x <= df_cna$end_position & y == df_cna$chromosome_index
      if (any(inds)) as.character(df_cna$total_cn[which.max(inds)]) else NA
    }))

  df_snvcna$sum_counts <- df_snvcna$ref_count + df_snvcna$alt_count

  mean_rd <- mean(df_snvcna$sum_counts)
  min_rd <- min(df_snvcna$sum_counts)
  max_rd <- max(df_snvcna$sum_counts)

  merged_df <- merge(df_snvcna, df_assigned, by = c("chromosome_index", "position"))
  merged_df['AF'] = merged_df['alt_count'] / (merged_df['sum_counts'])

  merged_df$total_cn <- as.numeric(merged_df$total_cn)
  merged_df$b_i_V <- pmax(1, round(merged_df$AF * (1 / purity) * ((purity * merged_df$total_cn) + (2 * (1 - purity))), 0))
  merged_df$CP_unpenalized <- (merged_df$alt_count * ((1 - purity) * 2 + purity * merged_df$total_cn)) / (merged_df$b_i_V * (merged_df$alt_count + merged_df$ref_count))
  merged_df$CCF_unpenalized <- merged_df$CP_unpenalized / purity

  total_SNVs <- sum(df_subc$num_SNV)

  merged_df <- merged_df %>%
    mutate(Subclonality = ifelse(cluster_index == 0, "Clonal", "Subclonal"),
           ReadDepth = sum_counts)

  subclonality_counts <- merged_df %>%
    count(Subclonality) %>%
    rename(n = n)

  return(list(
    merged_data = merged_df,
    subclonality_counts = subclonality_counts,
    read_depth = list(
      mean = mean_rd,
      min = min_rd,
      max = max_rd
    ),
    purity = purity,
    subclonal_structure = df_subc,
    total_mutations = total_SNVs,
    quality = sample_quality(purity, mean_rd, df_cna)
  ))
}
