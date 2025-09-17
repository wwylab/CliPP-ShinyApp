library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(ComplexHeatmap)
library(circlize)
library(data.table)
library(grid)

#' Load heatmap data
#' @return Processed correlation data for heatmap visualization
load_heatmap_data <- function() {
  tryCatch({
    global_TCGA_PCAWG <- "dataSource"

    # Load main cohort data (TCGA)
    final_cohort_file <- file.path(global_TCGA_PCAWG, "7279_final_cohort_result.txt")
    if (!file.exists(final_cohort_file)) {
      warning("Final cohort result file not found")
      return(data.frame())
    }

    final_cohort_result <- data.table::fread(final_cohort_file)

    # Calculate correlations for TCGA data
    correlations <- final_cohort_result %>%
      group_by(cancer_type) %>%
      reframe(
        TMB_cor = cor(sML, new_TMB, use = "complete.obs"),
        nrpcc_cor = cor(sML, nrpcc, use = "complete.obs"),
        purity_cor = cor(sML, purity, use = "complete.obs"),
        cohort = "TCGA"
      )

    # Add OCCAMS data if available
    occams_file <- file.path(global_TCGA_PCAWG, "OCCAMS_with_SI.RData")
    if (file.exists(occams_file)) {
      load(occams_file)
      if (exists("OCCAMS_with_SI")) {
        # Filter out rows with NA values for sML
        occams_data <- OCCAMS_with_SI[!is.na(OCCAMS_with_SI$sML), ]
        if (nrow(occams_data) > 0) {
          occams_cor <- data.frame(
            cancer_type = "ESAD (OCCAMS)",
            TMB_cor = cor(occams_data$sML, occams_data$cdsTMB, use = "complete.obs"),
            nrpcc_cor = cor(occams_data$sML, occams_data$nrpcc, use = "complete.obs"),
            purity_cor = cor(occams_data$sML, occams_data$purity, use = "complete.obs"),
            cohort = "OCCAMS"
          )
          correlations <- correlations %>% bind_rows(occams_cor)
        }
      }
    }

    # Add mCRPC data if available - try multiple possible file locations
    mcrpc_files <- c(
      file.path(global_TCGA_PCAWG, "mCRPC_21_summarytable.txt"),
      file.path(global_TCGA_PCAWG, "CliP_PCAWG_summary_table_debug_updated_sMF_TMB.txt")
    )

    for (mcrpc_file in mcrpc_files) {
      if (file.exists(mcrpc_file)) {
        mcrpc_data <- data.table::fread(mcrpc_file)

        # If this is the PCAWG file, filter for mCRPC samples specifically
        if (basename(mcrpc_file) == "CliP_PCAWG_summary_table_debug_updated_sMF_TMB.txt") {
          # For now, use PRAD samples as proxy for mCRPC until specific mCRPC data is available
          mcrpc_subset <- mcrpc_data[mcrpc_data$cancer_type == "PRAD", ]
          if (nrow(mcrpc_subset) > 0) {
            # Filter out NA values and ensure we have valid data for correlations
            valid_subset <- mcrpc_subset[
              !is.na(mcrpc_subset$sMF) &
              !is.na(mcrpc_subset$TMB) &
              !is.na(mcrpc_subset$nrpcc) &
              !is.na(mcrpc_subset$purity) &
              is.finite(mcrpc_subset$sMF) &
              is.finite(mcrpc_subset$TMB) &
              is.finite(mcrpc_subset$nrpcc) &
              is.finite(mcrpc_subset$purity),
            ]

            if (nrow(valid_subset) > 2) {  # Need at least 3 points for correlation
              tmb_cor <- cor(valid_subset$sMF, valid_subset$TMB, use = "complete.obs")
              nrpcc_cor <- cor(valid_subset$sMF, valid_subset$nrpcc, use = "complete.obs")
              purity_cor <- cor(valid_subset$sMF, valid_subset$purity, use = "complete.obs")

              # Only add if correlations are valid (not NA)
              if (!is.na(tmb_cor) && !is.na(nrpcc_cor) && !is.na(purity_cor)) {
                mcrpc_cor <- data.frame(
                  cancer_type = "PRAD (PCAWG)",
                  TMB_cor = tmb_cor,
                  nrpcc_cor = nrpcc_cor,
                  purity_cor = purity_cor,
                  cohort = "mCRPC"
                )
                correlations <- correlations %>% bind_rows(mcrpc_cor)
              }
            }
          }
        } else {
          # Direct mCRPC file
          mcrpc_cor <- data.frame(
            cancer_type = "mCRPC (NCT02113657)",
            TMB_cor = cor(mcrpc_data$sML, mcrpc_data$TMB, use = "complete.obs"),
            nrpcc_cor = cor(mcrpc_data$sML, mcrpc_data$nrpcc, use = "complete.obs"),
            purity_cor = cor(mcrpc_data$sML, mcrpc_data$purity, use = "complete.obs"),
            cohort = "mCRPC"
          )
          correlations <- correlations %>% bind_rows(mcrpc_cor)
        }
        break  # Use first available file
      }
    }

    return(correlations)

  }, error = function(e) {
    warning("Error loading heatmap data: ", e$message)
    return(data.frame())
  })
}

#' Create heatmap plot
#' @param data Data frame with correlation data
#' @param selected_cohorts Vector of cohorts to include
#' @param selected_cancer_types Vector of cancer types to include (optional)
#' @param show_numbers Boolean to show correlation values on heatmap
#' @return ComplexHeatmap object
create_heatmap_plot <- function(data, selected_cohorts = NULL,
                               selected_cancer_types = NULL,
                               show_numbers = TRUE) {

  if (nrow(data) == 0) {
    # Create empty plot with message
    return(grid::grid.newpage())
  }

  # Filter data based on selections
  plot_data <- data

  if (!is.null(selected_cohorts) && length(selected_cohorts) > 0) {
    plot_data <- plot_data %>%
      filter(cohort %in% selected_cohorts)
  }

  if (!is.null(selected_cancer_types) && length(selected_cancer_types) > 0) {
    plot_data <- plot_data %>%
      filter(cancer_type %in% selected_cancer_types)
  }

  if (nrow(plot_data) == 0) {
    return(grid::grid.newpage())
  }

  # Create correlation matrix - ensure all values are finite
  plot_data_clean <- plot_data %>%
    filter(
      is.finite(TMB_cor) & !is.na(TMB_cor),
      is.finite(nrpcc_cor) & !is.na(nrpcc_cor),
      is.finite(purity_cor) & !is.na(purity_cor)
    )

  if (nrow(plot_data_clean) == 0) {
    return(grid::grid.newpage())
  }

  cor_mat <- plot_data_clean %>%
    select(TMB_cor, nrpcc_cor, purity_cor) %>%
    as.matrix() %>%
    t()

  colnames(cor_mat) <- plot_data_clean$cancer_type
  rownames(cor_mat) <- c("TMB", "NRPCC", "Purity")

  # Format numbers for annotation
  labels_mat <- matrix(sprintf("%.2f", cor_mat),
                       nrow = nrow(cor_mat),
                       dimnames = dimnames(cor_mat))

  # Create the heatmap
  ht <- Heatmap(
    cor_mat,
    name = "Correlation",
    col = colorRamp2(c(-1, 0, 1), c("blue", "white", "red")),
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    row_title = "sMF correlations",
    column_names_rot = 45,
    heatmap_legend_param = list(
      title = "Pearson\nCorrelation",
      at = c(-1, -0.5, 0, 0.5, 1),
      labels = c("-1", "-0.5", "0", "0.5", "1")
    ),
    cell_fun = if (show_numbers) {
      function(j, i, x, y, width, height, fill) {
        grid.text(labels_mat[i, j], x, y, gp = gpar(fontsize = 8))
      }
    } else {
      NULL
    }
  )

  return(ht)
}

#' Get available cohorts for heatmap analysis
#' @param data Data frame with correlation data
#' @return Vector of available cohorts
get_available_cohorts_heatmap <- function(data) {
  if (nrow(data) == 0) {
    return(c("TCGA", "OCCAMS", "mCRPC"))
  }

  cohorts <- unique(data$cohort)
  return(sort(cohorts))
}

#' Get available cancer types for heatmap analysis
#' @param data Data frame with correlation data
#' @return Vector of cancer types
get_available_cancer_types_heatmap <- function(data) {
  if (nrow(data) == 0) {
    return(character(0))
  }

  cancer_types <- unique(data$cancer_type)
  return(sort(cancer_types))
}

#' Create summary statistics for heatmap data
#' @param data Data frame with correlation data
#' @param selected_cohorts Vector of cohorts to include
#' @return Data frame with summary statistics
create_heatmap_summary <- function(data, selected_cohorts = NULL) {
  if (nrow(data) == 0) {
    return(data.frame(
      statistic = character(0),
      value = character(0)
    ))
  }

  plot_data <- data
  if (!is.null(selected_cohorts) && length(selected_cohorts) > 0) {
    plot_data <- data %>%
      filter(cohort %in% selected_cohorts)
  }

  # Calculate correlation ranges with better NA handling
  valid_tmb <- plot_data$TMB_cor[!is.na(plot_data$TMB_cor) & is.finite(plot_data$TMB_cor)]
  valid_nrpcc <- plot_data$nrpcc_cor[!is.na(plot_data$nrpcc_cor) & is.finite(plot_data$nrpcc_cor)]
  valid_purity <- plot_data$purity_cor[!is.na(plot_data$purity_cor) & is.finite(plot_data$purity_cor)]

  tmb_range <- if (length(valid_tmb) > 0) range(valid_tmb) else c(0, 0)
  nrpcc_range <- if (length(valid_nrpcc) > 0) range(valid_nrpcc) else c(0, 0)
  purity_range <- if (length(valid_purity) > 0) range(valid_purity) else c(0, 0)

  summary_stats <- data.frame(
    statistic = c(
      "Cancer Types",
      "Cohorts",
      "TMB Correlation Range",
      "NRPCC Correlation Range",
      "Purity Correlation Range"
    ),
    value = c(
      nrow(plot_data),
      length(unique(plot_data$cohort)),
      paste(sprintf("%.2f", tmb_range), collapse = " to "),
      paste(sprintf("%.2f", nrpcc_range), collapse = " to "),
      paste(sprintf("%.2f", purity_range), collapse = " to ")
    )
  )

  return(summary_stats)
}
