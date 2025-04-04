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
  
  # Input data
  df_snv <- read.table(snvFile, header = TRUE)
  df_cna <- read.table(cnaFile, header = TRUE)
  purity <- as.numeric(readLines(purityFile, warn = FALSE))
  
  # Check required columns in SNV data
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
  # Initialize progress indicators
  snvFile <- sampleFiles$snvFile
  cnaFile <- sampleFiles$cnaFile
  purityFile <- sampleFiles$purityFile
  
  # Validate existence of required files
  if (is.null(snvFile) || is.null(cnaFile) || is.null(purityFile)) {
    return(list(
      success = FALSE,
      message = "One or more required files are missing."
    ))
  }
  
  # Validate and read files
  file_data <- read_and_validate_files(snvFile, cnaFile, purityFile)
  if (is.null(file_data)) {
    return(list(
      success = FALSE,
      message = "Error validating input files."
    ))
  }
  
  # Define the directory path
  directoryPath <- "forCliPP/CliPP/sample_id/"
  
  # Check if the directory exists, if not, create it
  if (!dir.exists(directoryPath)) {
    dir.create(directoryPath, recursive = TRUE, showWarnings = TRUE)
  } else {
    # Clear previous files in the directory
    system(paste("rm -rf", shQuote(directoryPath)), intern = TRUE)
    dir.create(directoryPath, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create the output directory structure
  finalResultPath <- file.path(directoryPath, "final_result", "Best_lambda")
  if (!dir.exists(finalResultPath)) {
    dir.create(finalResultPath, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Construct the command exactly as in the original server.R
  command <- sprintf("/usr/local/bin/python3 ./forCliPP/CliPP/run_clipp_main.py %s %s %s",
                     shQuote(snvFile), shQuote(cnaFile), shQuote(purityFile))
  
  # Display the command being executed
  cat("Executing command:", command, "\n")
  
  # Execute the command
  tryCatch({
    result <- system(command, intern = TRUE)
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error executing CliPP command:", e$message)
    ))
  })
  
  # Check if result files were generated
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
  
  # Read the files directly
  file_data <- read_and_validate_files(sampleFiles$snvFile, sampleFiles$cnaFile, sampleFiles$purityFile)
  if (is.null(file_data)) {
    return(NULL)
  }
  
  df_snv <- file_data$df_snv
  df_cna <- file_data$df_cna
  purity <- file_data$purity
  
  resultPath <- "forCliPP/CliPP/sample_id/final_result/Best_lambda"
  
  # Read mutation and subclonal structure assignments
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
  
  # Calculate the sum of ref_count and alt_count
  df_snvcna$sum_counts <- df_snvcna$ref_count + df_snvcna$alt_count
  
  # Compute the mean of the sum_counts column
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
  
  # Add subclonality information
  merged_df <- merged_df %>%
    mutate(Subclonality = ifelse(cluster_index == 0, "Clonal", "Subclonal"),
           ReadDepth = sum_counts)
  
  # Rename the count column to 'n'
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

#' Function to calculate recommended smoothing factor based on number of mutations
#' @param total_mutations Total number of mutations
#' @return Recommended smoothing factor
recommended_smoothing_factor <- function(total_mutations) {
  min_smooth <- 0.2
  max_smooth <- 0.7
  min_mut <- 100
  max_mut <- 10000
  smoothing_factor <- max_smooth - (total_mutations - min_mut) * (max_smooth - min_smooth) / (max_mut - min_mut)
  smoothing_factor <- max(min_smooth, min(max_smooth, smoothing_factor))
  return(round(smoothing_factor, 2))
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
  # Set color aesthetic based on the user input
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
  
  # Create annotations for clonal and subclonal counts
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
  # Set color aesthetic based on the user input
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
  
  # Create annotations for clonal and subclonal counts
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

#' Get VAF plot caption
#' @return HTML for VAF plot caption
get_vaf_plot_caption <- function() {
  HTML("<p> - <b>Variant Allele Frequency (VAF)</b> indicates the proportion of sequencing reads that support a variant allele.<br>
       - <b>Purity</b> reflects the proportion of tumor cells in the sample. For instance, a purity of 0.9 means 90% of the cells are cancerous.<br>
       - <b>Mean read depth</b> is the average number of sequencing reads per SNV, influencing data reliability.<br>
       - <b>Density</b> estimates the probability density function of VAFs based on kernel density estimation (KDE). <br>
  </p>")
}

#' Get CP plot caption
#' @return HTML for CP plot caption
get_cp_plot_caption <- function() {
  HTML("<p> - <b>Cellular Prevalence (CP)</b> the fraction of all cells (both tumor and admixed normal cells) from the sequenced tissue carrying a particular SNV.<br>
       - Annotations on the x-axis indicate whether mutations are <b>clonal</b> (present in all tumor cells) or <b>subclonal</b> (found in a subset).<br>
       - Counts of clonal and subclonal mutations are indicated as <b>n clonal</b> and <b>n subclonal</b> respectively.<br>
  </p>")
}

#' Prepare and run CliPP for TCGA/PCAWG samples
#' @param sampleDir Directory containing sample files
#' @param prefix Prefix (TCGA or PCAWG)
#' @param selectedSample Selected sample name
#' @return Boolean indicating success
prepareAndRunCliPP <- function(sampleDir, prefix, selectedSample) {
  if (!dir.exists(sampleDir)) {
    showNotification(paste("Sample directory not found:", sampleDir), type = "error")
    return(FALSE)
  }
  
  # Check for existing results
  matching_file1 <- list.files(sampleDir, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
  matching_file2 <- list.files(sampleDir, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)
  
  if (length(matching_file1) > 0 && length(matching_file2) > 0) {
    return(TRUE)
  }
  
  # Find input files
  snvFile <- list.files(sampleDir, pattern = "*.snv.txt", full.names = TRUE)
  cnaFile <- list.files(sampleDir, pattern = "*.cna.txt", full.names = TRUE)
  purityFile <- list.files(sampleDir, pattern = "*.purity.txt", full.names = TRUE)
  
  if (length(snvFile) == 0 || length(cnaFile) == 0 || length(purityFile) == 0) {
    showNotification("Required sample files not found", type = "error")
    return(FALSE)
  }
  
  # Set up directory
  directoryPath <- "forCliPP/CliPP/sample_id/"
  if (!dir.exists(directoryPath)) {
    dir.create(directoryPath, recursive = TRUE, showWarnings = TRUE)
  } else {
    system(paste("rm -rf", shQuote(directoryPath)), intern = TRUE)
    dir.create(directoryPath, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Create output directory
  finalResultPath <- file.path(directoryPath, "final_result", "Best_lambda")
  if (!dir.exists(finalResultPath)) {
    dir.create(finalResultPath, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Construct the command
  command <- sprintf("/usr/local/bin/python3 ./forCliPP/CliPP/run_clipp_main.py %s %s %s",
                     shQuote(snvFile[1]), shQuote(cnaFile[1]), shQuote(purityFile[1]))
  
  # Display the command being executed
  cat("Executing command:", command, "\n")
  
  # Execute the command
  result <- tryCatch({
    system(command, intern = TRUE)
    TRUE
  }, error = function(e) {
    showNotification(paste("Error running CliPP:", e$message), type = "error")
    FALSE
  })
  
  if (!result) return(FALSE)
  
  # Copy results to sample directory
  resultPath <- "forCliPP/CliPP/sample_id/final_result/Best_lambda"
  resultFiles <- list.files(resultPath, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(resultFiles) == 0) {
    showNotification("No result files generated by CliPP", type = "error")
    return(FALSE)
  }
  
  for (file in resultFiles) {
    file.copy(file, file.path(sampleDir, basename(file)), overwrite = TRUE)
  }
  
  # Verify results were copied
  matching_file1 <- list.files(sampleDir, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
  matching_file2 <- list.files(sampleDir, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)
  
  if (length(matching_file1) == 0 || length(matching_file2) == 0) {
    showNotification("Failed to generate mutation assignments", type = "error")
    return(FALSE)
  }
  
  return(TRUE)
}
