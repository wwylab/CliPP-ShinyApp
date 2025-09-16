library(dplyr)
library(purrr)

#' Process TCGA data for plotting
#' @param file_data List containing df_snv, df_cna, and purity
#' @param sample_name TCGA sample name for path construction
#' @param global_path Global TCGA PCAWG path
#' @return Processed analysis data or NULL if processing fails
process_tcga_data_internal <- function(file_data, sample_name, global_path) {
  if (is.null(file_data)) return(NULL)

  df_snv <- file_data$df_snv
  df_cna <- file_data$df_cna
  purity <- file_data$purity

  tryCatch({
    result_path <- file.path(global_path, "TCGA", sample_name)
    matching_file1 <- list.files(result_path, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
    matching_file2 <- list.files(result_path, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)

    df_assigned <- read.table(matching_file1[1], header = TRUE, sep = "\t")
    df_subc <- read.table(matching_file2[1], header = TRUE, sep = "\t")

    df_snv_subset <- df_snv %>%
      select(chromosome_index, position)

    matching_rows <- suppressMessages(
      inner_join(
        df_snv_subset,
        df_assigned %>% select(chromosome_index, position)
      )
    )

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

    if (nrow(merged_df) == 0) {
      df_snvcna$chromosome_index <- as.character(df_snvcna$chromosome_index)
      df_assigned$chromosome_index <- as.character(df_assigned$chromosome_index)
      df_snvcna$position <- as.integer(df_snvcna$position)
      df_assigned$position <- as.integer(df_assigned$position)

      merged_df <- left_join(df_snvcna, df_assigned, by = c("chromosome_index", "position"))
    }

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

  }, error = function(e) {
    warning("Error processing TCGA data: ", e$message)
    return(NULL)
  })
}

#' Process PCAWG data for plotting
#' @param file_data List containing df_snv, df_cna, and purity
#' @param sample_name PCAWG sample name for path construction
#' @param global_path Global TCGA PCAWG path
#' @return Processed analysis data or NULL if processing fails
process_pcawg_data_internal <- function(file_data, sample_name, global_path) {
  if (is.null(file_data)) return(NULL)

  df_snv <- file_data$df_snv
  df_cna <- file_data$df_cna
  purity <- file_data$purity

  tryCatch({
    result_path <- file.path(global_path, "PCAWG", sample_name)
    matching_file1 <- list.files(result_path, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
    matching_file2 <- list.files(result_path, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)

    df_assigned <- read.table(matching_file1[1], header = TRUE, sep = "\t",
                              colClasses = c(chromosome_index = "character", position = "integer",
                                             cluster_index = "integer"))
    df_subc <- read.table(matching_file2[1], header = TRUE, sep = "\t")

    df_snv$chromosome_index <- as.character(df_snv$chromosome_index)
    df_snv$position <- as.integer(df_snv$position)

    df_snvcna <- df_snv %>%
      mutate(
        chromosome_index = as.character(chromosome_index),
        total_cn = map2_chr(position, chromosome_index, function(x, y) {
          inds = x >= df_cna$start_position & x <= df_cna$end_position & y == df_cna$chromosome_index
          if (any(inds)) as.character(df_cna$total_cn[which.max(inds)]) else NA
        })
      )

    df_snvcna$sum_counts <- df_snvcna$ref_count + df_snvcna$alt_count
    mean_rd <- mean(df_snvcna$sum_counts)
    min_rd <- min(df_snvcna$sum_counts)
    max_rd <- max(df_snvcna$sum_counts)

    df_assigned$chromosome_index <- as.character(df_assigned$chromosome_index)
    merged_df <- merge(df_snvcna, df_assigned, by = c("chromosome_index", "position"))

    if (nrow(merged_df) == 0) {
      df_snvcna$chromosome_index <- as.character(df_snvcna$chromosome_index)
      df_assigned$chromosome_index <- as.character(df_assigned$chromosome_index)
      df_snvcna$position <- as.integer(df_snvcna$position)
      df_assigned$position <- as.integer(df_assigned$position)

      merged_df <- left_join(df_snvcna, df_assigned, by = c("chromosome_index", "position"))
    }

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

  }, error = function(e) {
    warning("Error processing PCAWG data: ", e$message)
    return(NULL)
  })
}

#' Consolidate driver mutations by sample
#' @param driver_data Driver mutation data frame
#' @param cancer_type Cancer type to filter by
#' @return Consolidated driver mutation data frame or NULL
consolidate_driver_mutations <- function(driver_data, cancer_type) {

  filtered_data <- driver_data[driver_data$cancer == cancer_type, ]

  if (nrow(filtered_data) == 0) {
    return(NULL)
  }

  unique_samples <- unique(filtered_data$sample)
  result <- data.frame(
    sample = character(),
    genes = character(),
    proteins = character(),
    consequences = character(),
    Subclonality = character(),
    VAF_range = character(),
    CP_range = character(),
    mutation_count = integer(),
    stringsAsFactors = FALSE
  )

  for (sample_name in unique_samples) {
    sample_rows <- filtered_data[filtered_data$sample == sample_name, ]

    unique_genes <- paste(unique(sample_rows$gene), collapse = ", ")
    unique_proteins <- paste(unique(sample_rows$protein), collapse = ", ")
    unique_consequences <- paste(unique(sample_rows$consequence), collapse = ", ")

    unique_subclonality <- unique(sample_rows$Subclonality)
    if (length(unique_subclonality) > 1) {
      subclonality_str <- "Mixed"
    } else {
      subclonality_str <- unique_subclonality[1]
    }

    vaf_values <- sample_rows$VAF
    vaf_range <- paste0("[", round(min(vaf_values, na.rm = TRUE), 3), ", ", round(max(vaf_values, na.rm = TRUE), 3), "]")

    cp_values <- sample_rows$CP
    cp_range <- paste0("[", round(min(cp_values, na.rm = TRUE), 3), ", ", round(max(cp_values, na.rm = TRUE), 3), "]")

    mutation_count <- nrow(sample_rows)

    result <- rbind(result, data.frame(
      sample = sample_name,
      genes = unique_genes,
      proteins = unique_proteins,
      consequences = unique_consequences,
      Subclonality = subclonality_str,
      VAF_range = vaf_range,
      CP_range = cp_range,
      mutation_count = mutation_count,
      stringsAsFactors = FALSE
    ))
  }

  return(result)
}