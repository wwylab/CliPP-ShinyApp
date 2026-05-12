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
  
  directoryPath <- "forCliPP/CliPP/sample_id/"
  
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
  
  command <- sprintf("python3 forCliPP/CliPP/run_clipp_main.py %s %s %s --filtering 2",
                     shQuote(snvFile), shQuote(cnaFile), shQuote(purityFile))
  
  cat("Executing command:", command, "\n")
  
  tryCatch({
    result <- system(command, intern = TRUE)
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error executing CliPP command:", e$message)
    ))
  })
  
  resultPath <- "forCliPP/CliPP/sample_id/final_result/Best_lambda"
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
    message = "Processing complete."
  ))
}

#' Read and validate input files
#' @param snvFile Path to SNV file
#' @param cnaFile Path to CNA file
#' @param purityFile Path to purity file
#' @return List containing validation results and data
validate_and_read_files <- function(snvFile, cnaFile, purityFile) {
  if (is.null(snvFile) || is.null(cnaFile) || is.null(purityFile)) {
    return(list(
      success = FALSE,
      message = "One or more required files are missing.",
      data = NULL
    ))
  }
  
  df_snv <- tryCatch(
    read.table(snvFile, header = TRUE),
    error = function(e) NULL
  )
  
  df_cna <- tryCatch(
    read.table(cnaFile, header = TRUE),
    error = function(e) NULL
  )
  
  purity <- tryCatch(
    as.numeric(readLines(purityFile, warn = FALSE)),
    error = function(e) NULL
  )
  
  required_cols <- c("chromosome_index", "position", "ref_count", "alt_count")
  if (!all(required_cols %in% colnames(df_snv))) {
    missing_cols <- required_cols[!required_cols %in% colnames(df_snv)]
    return(list(
      success = FALSE,
      message = paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
      data = NULL
    ))
  }
  
  return(list(
    success = TRUE,
    message = "Files read successfully",
    data = list(
      df_snv = df_snv,
      df_cna = df_cna,
      purity = purity
    )
  ))
}

#' Get current analysis for the selected sample
#' @param sampleFiles Files for the selected sample
#' @return Analysis data structure or NULL if processing fails
get_current_analysis <- function(sampleFiles) {
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
  
  resultPath <- "forCliPP/CliPP/sample_id/final_result/Best_lambda"
  
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
    total_mutations = total_SNVs
  ))
}

#' Create VAF plot
#' @param merged_df Merged data frame
#' @param sample_name Sample name
#' @param purity Sample purity
#' @param read_depth Read depth information
#' @param smoothing_factor Smoothing factor for density plot
#' @param color_by Variable to color points by
#' @param subclonality_counts Subclonality counts
#' @return Plotly object
create_vaf_plot <- function(merged_df, sample_name, purity, read_depth, smoothing_factor, color_by, subclonality_counts) {
  color_aesthetic <- if (color_by == "clonality") "Subclonality" else "ReadDepth"
  
  gg1 <- ggplot(merged_df, aes(x = AF)) +
    geom_density(fill = "gray", alpha = .6, adjust = smoothing_factor) +
    geom_point(
      shape = 142,
      size = 5,
      alpha = 0.3,
      aes(
        y = -0.75,
        color = !!sym(color_aesthetic)
      )
    ) +
    xlim(0, 1) +
    ggtitle(paste(sample_name, " - Purity =", round(purity, 3), "      ", 
                  "Mean read depth =", round(read_depth$mean), "      ", 
                  "Range read depth =", read_depth$min, "-", read_depth$max)) +
    ggpubr::theme_pubr() + xlab("VAF") + ylab("Density") + 
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  if ("Clonal" %in% subclonality_counts$Subclonality) {
    n_clonal <- subclonality_counts$n[subclonality_counts$Subclonality == "Clonal"]
    gg1 <- gg1 + annotate("text", x = 0.85, y = -1.65, 
                          label = paste0("n clonal = ", n_clonal), 
                          size = 3, color = "black", vjust = -1.5)
  }
  
  if ("Subclonal" %in% subclonality_counts$Subclonality) {
    n_subclonal <- subclonality_counts$n[subclonality_counts$Subclonality == "Subclonal"]
    gg1 <- gg1 + annotate("text", x = 0.85, y = -2.65, 
                          label = paste0("n subclonal = ", n_subclonal), 
                          size = 3, color = "black", vjust = -1.5)
  }
  
  return(plotly::ggplotly(gg1))
}

#' Create CP plot
#' @param merged_df Merged data frame
#' @param sample_name Sample name
#' @param smoothing_factor Smoothing factor for density plot
#' @param color_by Variable to color points by
#' @param subclonality_counts Subclonality counts
#' @param df_subc Subclonal structure data frame
#' @return Plotly object
create_cp_plot <- function(merged_df, sample_name, smoothing_factor, color_by, subclonality_counts, df_subc) {
  color_aesthetic <- if (color_by == "clonality") "Subclonality" else "ReadDepth"
  
  gg2 <- ggplot(merged_df, aes(x = CP_unpenalized)) +
    geom_density(fill = "seagreen", alpha = .6, adjust = smoothing_factor) +
    geom_point(
      shape = 142,
      size = 5,
      alpha = 0.3,
      aes(
        y = -0.75,
        color = !!sym(color_aesthetic)
      )
    ) +
    xlim(0, 1.5) +
    ggtitle(paste(sample_name, " - Estimated mutation-specific CP")) +
    ggpubr::theme_pubr() + xlab("Estimated mutation-specific CP") + ylab("Density") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    geom_segment(data = df_subc, aes(x = cellular_prevalence, xend = cellular_prevalence, y = 0, yend = 5.7), lty = "dashed")
  
  if ("Clonal" %in% subclonality_counts$Subclonality) {
    n_clonal <- subclonality_counts$n[subclonality_counts$Subclonality == "Clonal"]
    gg2 <- gg2 + annotate("text", x = 1.3, y = -1.65, 
                          label = paste0("n clonal = ", n_clonal), 
                          size = 3, color = "black", vjust = -1.5)
  }
  
  if ("Subclonal" %in% subclonality_counts$Subclonality) {
    n_subclonal <- subclonality_counts$n[subclonality_counts$Subclonality == "Subclonal"]
    gg2 <- gg2 + annotate("text", x = 1.3, y = -2.65, 
                          label = paste0("n subclonal = ", n_subclonal), 
                          size = 3, color = "black", vjust = -1.5)
  }
  
  return(plotly::ggplotly(gg2))
}

#' Prepare and run CliPP for TCGA/PCAWG samples
#' @param sampleDir Directory containing sample files
#' @param prefix Prefix (TCGA or PCAWG)
#' @param selectedSample Selected sample name
#' @return Boolean indicating success
prepareAndRunCliPP <- function(sampleDir, prefix, selectedSample) {
  if (!dir.exists(sampleDir)) {
    dir.create(sampleDir, recursive = TRUE, showWarnings = FALSE)
  }
  
  best_lambda_dir <- file.path(sampleDir, "final_result", "Best_lambda")
  matching_file1 <- list.files(best_lambda_dir, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
  matching_file2 <- list.files(best_lambda_dir, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)
  
  if (length(matching_file1) > 0 && length(matching_file2) > 0) {
    return(TRUE)
  }
  
  www_dir <- file.path(global_www, prefix, selectedSample)
  snvFile <- list.files(www_dir, pattern = "\\.snv\\.txt$", full.names = TRUE)
  cnaFile <- list.files(www_dir, pattern = "\\.cna\\.txt$", full.names = TRUE)
  purityFile <- list.files(www_dir, pattern = "\\.purity\\.txt$", full.names = TRUE)
  
  if (length(snvFile) == 0 || length(cnaFile) == 0 || length(purityFile) == 0) {
    showNotification("Required sample files not found", type = "error")
    return(FALSE)
  }
  
  directoryPath <- "forCliPP/CliPP/sample_id/"
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
  
  command <- sprintf("python3 forCliPP/CliPP/run_clipp_main.py %s %s %s --filtering 2",
                     shQuote(snvFile[1]), shQuote(cnaFile[1]), shQuote(purityFile[1]))
  
  cat("Executing command:", command, "\n")
  
  result <- tryCatch({
    system(command, intern = TRUE)
    TRUE
  }, error = function(e) {
    showNotification(paste("Error running CliPP:", e$message), type = "error")
    FALSE
  })
  
  if (!result) return(FALSE)
  
  resultPath <- "forCliPP/CliPP/sample_id/final_result/Best_lambda"
  resultFiles <- list.files(resultPath, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(resultFiles) == 0) {
    showNotification("No result files generated by CliPP", type = "error")
    return(FALSE)
  }
  
  if (!dir.exists(best_lambda_dir)) {
    dir.create(best_lambda_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  for (file in resultFiles) {
    file.copy(file, file.path(best_lambda_dir, basename(file)), overwrite = TRUE)
  }
  
  matching_file1 <- list.files(best_lambda_dir, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
  matching_file2 <- list.files(best_lambda_dir, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)
  
  if (length(matching_file1) == 0 || length(matching_file2) == 0) {
    showNotification("Failed to generate mutation assignments", type = "error")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Count SNVs in a file
#' @param file_path Path to the SNV file
#' @return Number of SNVs or 0 if file doesn't exist
count_snvs <- function(file_path) {
  if (!file.exists(file_path)) return(0)
  df <- read.table(file_path, header = TRUE)
  nrow(df)
}

#' Reconstruct file_data from preprocess_result directory
#' @param sample_dir dataSource sample directory (contains preprocess_result/)
#' @return file_data list (df_snv, df_cna, purity) or NULL
read_from_preprocess <- function(sample_dir) {
  pre_dir <- file.path(sample_dir, "preprocess_result")
  if (!dir.exists(pre_dir)) return(NULL)

  tryCatch({
    mult  <- read.table(file.path(pre_dir, "multiplicity.txt"), header = FALSE)
    r     <- scan(file.path(pre_dir, "r.txt"),     quiet = TRUE)
    n     <- scan(file.path(pre_dir, "n.txt"),     quiet = TRUE)
    total <- scan(file.path(pre_dir, "total.txt"), quiet = TRUE)
    purity <- as.numeric(readLines(file.path(pre_dir, "purity_ploidy.txt"), warn = FALSE)[1])

    df_snv <- data.frame(
      chromosome_index = mult[[1]],
      position         = as.integer(mult[[2]]),
      alt_count        = as.integer(r),
      ref_count        = as.integer(n - r),
      stringsAsFactors = FALSE
    )

    # Per-mutation CN as point segments so the interval join in process_*_data_internal works
    df_cna <- data.frame(
      chromosome_index = mult[[1]],
      start_position   = as.integer(mult[[2]]),
      end_position     = as.integer(mult[[2]]),
      total_cn         = as.numeric(total),
      stringsAsFactors = FALSE
    )

    list(df_snv = df_snv, df_cna = df_cna, purity = purity)
  }, error = function(e) NULL)
}

#' Read TCGA files from sample directory
#' @param sample_dir Directory containing TCGA sample files
#' @return Validated file data or NULL
read_tcga_files <- function(sample_dir) {
  www_dir   <- sub(global_TCGA_PCAWG, global_www, sample_dir, fixed = TRUE)
  snv_file  <- list.files(www_dir, pattern = "\\.snv\\.txt$",    full.names = TRUE)
  cna_file  <- list.files(www_dir, pattern = "\\.cna\\.txt$",    full.names = TRUE)
  purity_file <- list.files(www_dir, pattern = "\\.purity\\.txt$", full.names = TRUE)

  if (length(snv_file) > 0 && length(cna_file) > 0 && length(purity_file) > 0) {
    return(validate_and_read_files(snv_file[1], cna_file[1], purity_file[1])$data)
  }

  read_from_preprocess(sample_dir)
}

#' Read PCAWG files from sample directory
#' @param sample_dir Directory containing PCAWG sample files
#' @return Validated file data or NULL
read_pcawg_files <- function(sample_dir) {
  www_dir   <- sub(global_TCGA_PCAWG, global_www, sample_dir, fixed = TRUE)
  snv_file  <- list.files(www_dir, pattern = "\\.snv\\.txt$",    full.names = TRUE)
  cna_file  <- list.files(www_dir, pattern = "\\.cna\\.txt$",    full.names = TRUE)
  purity_file <- list.files(www_dir, pattern = "\\.purity\\.txt$", full.names = TRUE)

  if (length(snv_file) > 0 && length(cna_file) > 0 && length(purity_file) > 0) {
    return(validate_and_read_files(snv_file[1], cna_file[1], purity_file[1])$data)
  }

  read_from_preprocess(sample_dir)
}
