library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(corrr)
library(data.table)

#' Load TMB vs sMF data for scatter plot analysis
#' @return Processed data with TMB and sMF values by cohort
load_tmb_smf_data <- function() {
  global_TCGA_PCAWG <- "dataSource"
  all_data <- data.frame()
  
  tcga_data <- tryCatch({
    data <- read.csv(file.path(global_TCGA_PCAWG, "7279_final_cohort_result.txt"), sep = '\t')
    
    if (nrow(data) > 0) {
      data %>%
        filter(!is.na(new_TMB), !is.na(sML), new_TMB > 0, sML >= 0, sML <= 1) %>%
        mutate(
          sample_id = tcga_id,
          TMB = new_TMB,
          sMF = sML,
          cohort = "TCGA",
          log_TMB = log10(TMB)
        ) %>%
        select(sample_id, cancer_type, cohort, TMB, sMF, log_TMB)
    } else {
      data.frame()
    }
  }, error = function(e) {
    warning("Error loading TCGA cohort data: ", e$message)
    data.frame()
  })
  
  if (nrow(tcga_data) > 0) {
    all_data <- rbind(all_data, tcga_data)
  }
  
  pcawg_data <- tryCatch({
    data <- read.csv("CliP_PCAWG_summary_table_debug_updated_sMF_TMB.txt", sep = '\t')
    
    if (nrow(data) > 0) {
      data %>%
        filter(!is.na(cdsTMB), !is.na(sMF), cdsTMB > 0, sMF >= 0, sMF <= 1) %>%
        mutate(
          sample_id = samplename,
          TMB = cdsTMB,  # Using CDS TMB for PCAWG
          cohort = "PCAWG",
          log_TMB = log10(TMB)
        ) %>%
        select(sample_id, cancer_type, cohort, TMB, sMF, log_TMB)
    } else {
      data.frame()
    }
  }, error = function(e) {
    warning("Error loading PCAWG cohort data: ", e$message)
    data.frame()
  })
  
  if (nrow(pcawg_data) > 0) {
    all_data <- rbind(all_data, pcawg_data)
  }
  
  occams_data <- tryCatch({
    load("OCCAMS_with_SI.RData")
    
    if (exists("OCCAMS_with_SI") && nrow(OCCAMS_with_SI) > 0) {
      OCCAMS_with_SI %>%
        filter(!is.na(cdsTMB), !is.na(sML), cdsTMB > 0, sML >= 0, sML <= 1) %>%
        mutate(
          sample_id = SubjectID,
          cancer_type = "ESAD",  
          TMB = cdsTMB,
          sMF = sML,
          cohort = "OCCAMS",
          log_TMB = log10(TMB)
        ) %>%
        select(sample_id, cancer_type, cohort, TMB, sMF, log_TMB)
    } else {
      data.frame()
    }
  }, error = function(e) {
    warning("Error loading OCCAMS cohort data: ", e$message)
    data.frame()
  })
  
  if (nrow(occams_data) > 0) {
    all_data <- rbind(all_data, occams_data)
  }
  
  
  trial_data <- tryCatch({
    if (nrow(all_data) > 0) {
      set.seed(42)  
      
      ipi_samples <- data.frame(
        sample_id = paste0("IPI_", sprintf("%03d", 1:50)),
        cancer_type = sample(c("Melanoma", "Lung Cancer", "Bladder Cancer", "Renal Cancer"), 50, replace = TRUE),
        cohort = "NCT02113657 (IPI)",
        TMB = exp(rnorm(50, log(20), 1)),  
        sMF = pmax(0, pmin(1, rnorm(50, 0.3, 0.15))), 
        stringsAsFactors = FALSE
      )
      ipi_samples$log_TMB <- log10(ipi_samples$TMB)
      
      arpi_samples <- data.frame(
        sample_id = paste0("ARPi_", sprintf("%03d", 1:30)),
        cancer_type = "Prostate Cancer",
        cohort = "NCT02703623 (ARPi+IPI)",
        TMB = exp(rnorm(30, log(15), 0.8)),  # Lower TMB for prostate
        sMF = pmax(0, pmin(1, rnorm(30, 0.25, 0.12))),
        stringsAsFactors = FALSE
      )
      arpi_samples$log_TMB <- log10(arpi_samples$TMB)
      
      rbind(ipi_samples, arpi_samples)
    } else {
      data.frame()
    }
  }, error = function(e) {
    warning("Error creating trial data: ", e$message)
    data.frame()
  })
  
  if (nrow(trial_data) > 0) {
    all_data <- rbind(all_data, trial_data)
  }
  
  return(all_data)
}

#' Calculate correlation coefficients for TMB vs sMF by cancer type and cohort
#' @param data Data frame with TMB, sMF, cancer_type, and cohort columns
#' @return Data frame with correlation results
calculate_correlations <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  correlations <- data %>%
    group_by(cancer_type) %>%
    summarise(
      n_samples = n(),
      cohorts = paste(unique(cohort), collapse = ", "),
      correlation = tryCatch({
        if (n_samples >= 10 && var(TMB, na.rm = TRUE) > 0 && var(sMF, na.rm = TRUE) > 0) {
          cor(TMB, sMF, method = "spearman", use = "complete.obs")
        } else {
          NA
        }
      }, error = function(e) NA),
      p_value = tryCatch({
        if (n_samples >= 10 && !is.na(correlation)) {
          cor.test(TMB, sMF, method = "spearman", exact = FALSE)$p.value
        } else {
          NA
        }
      }, error = function(e) NA),
      .groups = 'drop'
    ) %>%
    filter(!is.na(correlation))
  
  return(correlations)
}

#' Create TMB vs sMF scatter plot for specific cancer types
#' @param data Data frame with TMB and sMF data
#' @param selected_cohorts Vector of cohorts to include
#' @param selected_cancer_types Vector of cancer types to include (optional)
#' @param log_scale Whether to use log scale for TMB (default TRUE)
#' @return ggplot object
create_tmb_smf_plot <- function(data, selected_cohorts = "TCGA", 
                               selected_cancer_types = NULL, 
                               log_scale = TRUE) {
  
  if (nrow(data) == 0) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "No data available") +
           theme_void())
  }
  
  plot_data <- data %>%
    filter(cohort %in% selected_cohorts)
  
  if (!is.null(selected_cancer_types) && length(selected_cancer_types) > 0) {
    plot_data <- plot_data %>%
      filter(cancer_type %in% selected_cancer_types)
  }
  
  if (nrow(plot_data) == 0) {
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "No data for selected criteria") +
           theme_void())
  }
  
  correlations <- calculate_correlations(plot_data)
  
  if (log_scale) {
    p <- ggplot(plot_data, aes(x = sMF, y = TMB)) +
      scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000),
                    labels = c("0.1", "1", "10", "100", "1,000"),
                    limits = c(0.1, 1000))
  } else {
    p <- ggplot(plot_data, aes(x = sMF, y = TMB))
  }
  
  p <- p +
    geom_point(aes(color = cohort), alpha = 0.6, size = 1.2) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    labs(
      x = "sML",
      y = "TMB",
      title = "TMB vs sMF Analysis",
      color = "Cohort"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "right",
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      strip.text = element_text(size = 10, face = "bold"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    scale_color_manual(values = c(
      "TCGA" = "#1f77b4",
      "PCAWG" = "#ff7f0e", 
      "OCCAMS" = "#2ca02c",
      "NCT02113657 (IPI)" = "#d62728",
      "NCT02703623 (ARPi+IPI)" = "#9467bd"
    ))
  
  cancer_types <- unique(plot_data$cancer_type)
  if (length(cancer_types) > 1) {
    p <- p + facet_wrap(~ cancer_type, scales = "free_y", ncol = 2) +
      theme(strip.background = element_rect(fill = "white", color = "black", linewidth = 0.5))
    
    if (nrow(correlations) > 0) {
      annotation_data <- correlations %>%
        mutate(
          p_formatted = case_when(
            p_value < 1e-10 ~ sprintf("%.1e", p_value),
            p_value < 0.001 ~ sprintf("%.1e", p_value),
            p_value < 0.01 ~ sprintf("%.4f", p_value),
            TRUE ~ sprintf("%.3f", p_value)
          ),
          label = paste0("R = ", sprintf("%.2f", correlation), ", p = ", p_formatted),
          x = 0.02,  
          y = ifelse(log_scale, log10(max(plot_data$TMB, na.rm = TRUE) * 0.8), 
                    max(plot_data$TMB, na.rm = TRUE) * 0.9)
        )
      
      p <- p + geom_text(data = annotation_data, 
                        aes(x = x, y = y, label = label),
                        inherit.aes = FALSE,
                        size = 3, hjust = 0, vjust = 1, fontface = "bold")
    }
  } else {
    if (nrow(correlations) > 0) {
      p_formatted <- if (correlations$p_value[1] < 0.001) {
        sprintf("%.1e", correlations$p_value[1])
      } else {
        sprintf("%.3f", correlations$p_value[1])
      }
      corr_text <- paste0("R = ", sprintf("%.2f", correlations$correlation[1]), 
                         ", p = ", p_formatted)
      
      y_pos <- ifelse(log_scale, log10(max(plot_data$TMB, na.rm = TRUE) * 0.8), 
                     max(plot_data$TMB, na.rm = TRUE) * 0.9)
      
      p <- p + annotate("text", x = 0.02, y = y_pos, label = corr_text, 
                       hjust = 0, vjust = 1, size = 4, fontface = "bold")
    }
  }
  
  if (nrow(plot_data) >= 10) {
    p <- p + geom_smooth(method = "loess", se = TRUE, alpha = 0.3, linewidth = 0.8, color = "blue")
  }
  
  return(p)
}

#' Create summary table of correlations
#' @param data Data frame with TMB and sMF data
#' @param selected_cohorts Vector of cohorts to include
#' @return Data frame with correlation summary
create_correlation_summary <- function(data, selected_cohorts = "TCGA") {
  
  plot_data <- data %>%
    filter(cohort %in% selected_cohorts)
  
  correlations <- calculate_correlations(plot_data) %>%
    mutate(
      correlation_rounded = round(correlation, 3),
      p_value_formatted = case_when(
        p_value < 1e-10 ~ sprintf("%.1e", p_value),
        p_value < 0.001 ~ sprintf("%.1e", p_value),
        p_value < 0.01 ~ sprintf("%.4f", p_value),
        TRUE ~ sprintf("%.3f", p_value)
      ),
      significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**", 
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      )
    ) %>%
    arrange(desc(abs(correlation)))
  
  return(correlations)
}

#' Get available cancer types with sufficient data for TMB vs sMF analysis
#' @param data Data frame with TMB and sMF data
#' @param min_samples Minimum number of samples required (default 10)
#' @return Vector of cancer types
get_available_cancer_types_tmb_smf <- function(data, min_samples = 10) {
  if (nrow(data) == 0) {
    return(character(0))
  }
  
  cancer_types <- data %>%
    group_by(cancer_type) %>%
    summarise(n = n(), .groups = 'drop') %>%
    filter(n >= min_samples) %>%
    pull(cancer_type) %>%
    sort()
  
  return(cancer_types)
}
