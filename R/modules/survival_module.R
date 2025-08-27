library(shiny)
library(dplyr)
library(rpart)
library(survival)
library(survminer)
library(ggplot2)
library(ggpubr)
library(plotly)
library(data.table)
library(readxl)

`%>%` <- dplyr::`%>%`

#' Load and prepare survival data
#' @return Processed data with cancer subtypes
load_survival_data <- function() {
  global_TCGA_PCAWG <- "dataSource"
  
  TCGA <- tryCatch({
    read.csv(file.path(global_TCGA_PCAWG, "7279_final_cohort_result.txt"), sep = '\t')
  }, error = function(e) {
    warning("Error loading main cohort data: ", e$message)
    return(data.frame())
  })
  
  if (nrow(TCGA) == 0) {
    return(list(PFI_data = data.frame(), OS_data = data.frame()))
  }
  
  TCGA$PFI.time <- suppressWarnings(as.numeric(TCGA$PFI.time))
  TCGA_PFI <- TCGA[!is.na(TCGA$PFI.time), ]
  TCGA_PFI <- TCGA_PFI[TCGA_PFI$PFI.time > 0, ]
  
  TCGA$OS.time <- as.numeric(TCGA$OS.time)
  TCGA_OS <- TCGA[!is.na(TCGA$OS.time), ]
  TCGA_OS <- TCGA_OS[TCGA_OS$OS.time > 0, ]
  
  IDH1_status <- tryCatch({
    data.table::fread(file.path(global_TCGA_PCAWG, "LGG_IDH_status.csv"))
  }, error = function(e) { data.frame() })
  
  HPV_status <- tryCatch({
    readxl::read_excel(file.path(global_TCGA_PCAWG, "HNSC_HPV_status_CP.xlsx"))
  }, error = function(e) { data.frame() })
  
  if (nrow(IDH1_status) > 0) {
    IDH1_mut <- IDH1_status %>% dplyr::filter(IDH.status == "Mutant")
  } else {
    IDH1_mut <- data.frame(tcga_id = character())
  }
  
  if (nrow(HPV_status) > 0) {
    HPV_neg <- HPV_status %>% dplyr::filter(`PanCan HPV consensus` == "Negative")
  } else {
    HPV_neg <- data.frame(bcr_patient_barcode = character())
  }
  
  TCGA_PFI <- TCGA_PFI %>%
    dplyr::mutate(can_sub = dplyr::case_when(
      tcga_id %in% IDH1_mut$tcga_id ~ "LGG IDH1 mut",
      tcga_id %in% HPV_neg$bcr_patient_barcode ~ "HNSC HPV neg",
      TRUE ~ cancer_type
    ))
  
  CMS_status <- tryCatch({
    data.table::fread(file.path(global_TCGA_PCAWG, "consensus_CMS.csv"))
  }, error = function(e) { data.frame() })
  
  Endometrioid_status <- tryCatch({
    data.table::fread(file.path(global_TCGA_PCAWG, "UCEC_endometrioid_status.csv"))
  }, error = function(e) { data.frame() })
  
  LUAD_smoker_status <- tryCatch({
    data.table::fread(file.path(global_TCGA_PCAWG, "smoking_Status_luad.csv"))
  }, error = function(e) { data.frame() })
  
  LUSC_smoker_status <- tryCatch({
    data.table::fread(file.path(global_TCGA_PCAWG, "smoking_Status_lusc.csv"))
  }, error = function(e) { data.frame() })
  
  BRCA_match <- tryCatch({
    load(file.path(global_TCGA_PCAWG, "BRCA.match.RData"))
    if (exists("BRCA.match")) {
      BRCA.match
    } else {
      data.frame()
    }
  }, error = function(e) { data.frame() })
  
  CMS2 <- if (nrow(CMS_status) > 0) {
    CMS_status %>% dplyr::filter(CMS_final_network_plus_RFclassifier_in_nonconsensus_samples == "CMS2")
  } else { data.frame(sample = character()) }
  
  Endometrioid <- if (nrow(Endometrioid_status) > 0) {
    Endometrioid_status %>% dplyr::filter(Endometrioid == TRUE)
  } else { data.frame(tcga_id = character()) }
  
  LUAD_smokers <- if (nrow(LUAD_smoker_status) > 0) {
    LUAD_smoker_status %>% dplyr::filter(Smoking.Status2 == "Ever smoker == T")
  } else { data.frame(tcga_id = character()) }
  
  LUSC_smokers <- if (nrow(LUSC_smoker_status) > 0) {
    LUSC_smoker_status %>% dplyr::filter(Smoking.Status2 == "Ever smoker == T")
  } else { data.frame(tcga_id = character()) }
  
  TNBC_status <- if (nrow(BRCA_match) > 0) {
    BRCA_match %>% dplyr::filter(Triple.Negative.summary == "Yes")
  } else { data.frame(Sample.short = character()) }
  
  TCGA_OS <- TCGA_OS %>%
    dplyr::mutate(can_sub = dplyr::case_when(
      tcga_id %in% CMS2$sample ~ "CRC CMS2",
      tcga_id %in% Endometrioid$tcga_id ~ "UCEC Endometrioid",
      tcga_id %in% LUAD_smokers$tcga_id ~ "LUAD (smoker)",
      tcga_id %in% LUSC_smokers$tcga_id ~ "LUSC (smoker)",
      tcga_id %in% TNBC_status$Sample.short ~ "BRCA (TNBC)",
      TRUE ~ cancer_type
    ))
  
  return(list(PFI_data = TCGA_PFI, OS_data = TCGA_OS))
}

#' Get available cancer types for PFI analysis
#' @param data_list List containing PFI and OS data
#' @return Vector of cancer types
get_pfi_cancer_types <- function(data_list) {
  cancer_list_PFI <- c("MESO", "LIHC", "LGG IDH1 mut", "KIRP", "KICH", "OV", "PRAD", "SARC", "THCA", "KIRC", "SKCM", "HNSC HPV neg", "GBM")
  available_types <- intersect(cancer_list_PFI, unique(data_list$PFI_data$can_sub))
  return(available_types)
}

#' Get available cancer types for OS analysis
#' @param data_list List containing PFI and OS data
#' @return Vector of cancer types
get_os_cancer_types <- function(data_list) {
  cancer_list_OS <- c("ACC", "BLCA", "BRCA", "CRC CMS2", "BRCA (TNBC)", "LUAD (smoker)", "LUSC (smoker)", "PAAD", "UCEC Endometrioid")
  available_types <- intersect(cancer_list_OS, unique(data_list$OS_data$can_sub))
  return(available_types)
}

#' Create Kaplan-Meier survival plot
#' @param df_data Survival data for specific cancer type
#' @param survival_type Either "PFI" or "OS"
#' @param cancer_type Name of cancer type
#' @return ggplot object
create_km_plot <- function(df_data, survival_type, cancer_type) {
  if (is.null(df_data) || nrow(df_data) == 0) {
    return(
      ggplot() +
        ggtitle(paste("No data available for", cancer_type)) +
        theme_void()
    )
  }
  
  time_col  <- if (identical(survival_type, "PFI")) "PFI.time" else "OS.time"
  event_col <- if (identical(survival_type, "PFI")) "PFI"       else "OS"
  y_label   <- if (identical(survival_type, "PFI")) "Progression-free Probability" else "Overall Survival"
  
  time_col  <- as.character(time_col)
  event_col <- as.character(event_col)
  
  required_cols <- c("sML", time_col, event_col)
  missing_cols  <- setdiff(required_cols, colnames(df_data))
  if (length(missing_cols) > 0) {
    return(
      ggplot() +
        ggtitle(paste("Missing columns for", cancer_type, ":", paste(missing_cols, collapse = ", "))) +
        theme_void()
    )
  }
  
  df <- df_data
  
  df[[time_col]] <- suppressWarnings(as.numeric(df[[time_col]]))
  df[[event_col]] <- as.integer(df[[event_col]] %in% c(1, "1", TRUE, "TRUE"))
  
  needed <- c("sML", as.character(time_col), as.character(event_col))
  df_clean <- df[complete.cases(df[, needed, drop = FALSE]), , drop = FALSE]
  
  if (nrow(df_clean) < 10) {
    return(
      ggplot() +
        ggtitle(paste("Insufficient data for", cancer_type, "(n =", nrow(df_clean), ")")) +
        theme_void()
    )
  }
  
  tryCatch({
    cutoff <- tryCatch({
      surv_formula_split <- as.formula(
        paste0("survival::Surv(", time_col, ", ", event_col, ") ~ sML")
      )
      
      tfit <- rpart::rpart(
        formula = surv_formula_split,
        data    = df_clean,
        control = rpart::rpart.control(minsplit = 5, maxdepth = 1, cp = -1)
      )
      
      if (!is.null(tfit$splits) && nrow(tfit$splits) > 0) {
        tfit$splits[1, "index"]
      } else {
        stats::median(df_clean$sML, na.rm = TRUE)
      }
    }, error = function(e) {
      stats::median(df_clean$sML, na.rm = TRUE)
    })
    
    n_low  <- sum(df_clean$sML <  cutoff, na.rm = TRUE)
    n_high <- sum(df_clean$sML >= cutoff, na.rm = TRUE)
    
    time_formula_part <- paste0(time_col, "/365.25")
    event_formula_part <- event_col
    
    fit_expression <- paste0(
      "survival::survfit(survival::Surv(", time_formula_part, ", ", event_formula_part, ") ~ sML < ", cutoff, ", data = df_clean)"
    )
    fit <- eval(parse(text = fit_expression))
    
    df_clean$group <- ifelse(df_clean$sML >= cutoff, "High sMF", "Low sMF")
    df_clean$group <- factor(df_clean$group, levels = c("Low sMF", "High sMF"))
    
    surv_diff <- survival::survdiff(
      as.formula(paste0("survival::Surv(", time_col, "/365.25, ", event_col, ") ~ (sML >= ", cutoff, ")")),
      data = df_clean
    )
    p_value <- 1 - stats::pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
    p_text <- if (p_value < 0.001) {
      sprintf("p < 0.001")
    } else {
      sprintf("p = %.3f", p_value)
    }
    
    p <- survminer::ggsurvplot(
      fit,
      data       = df_clean,
      xlab       = "Years",
      ylab       = y_label,
      title      = paste("Survival Analysis:", cancer_type),
      pval       = FALSE,
      pval.coord = NULL,
      conf.int   = FALSE,
      xlim       = c(0, 10),
      break.time.by = 2,
      palette    = c("#D6ACC0", "#63A299"),
      legend     = "none"
    )
    
    final_plot <- p$plot +
      theme(
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.key = element_blank(),     
        legend.background = element_blank() 
      ) +
      labs(subtitle = "", caption = "", tag = "") +
      annotate("text", 
               x = 9, y = 0.95,  
               label = paste0("High sMF (≥", sprintf("%.3f", cutoff), ", n=", n_high, ")"),
               hjust = 1, vjust = 1, size = 3, color = "#63A299", fontface = "bold") +
      annotate("text", 
               x = 9, y = 0.90, 
               label = paste0("Low sMF (<", sprintf("%.3f", cutoff), ", n=", n_low, ")"),
               hjust = 1, vjust = 1, size = 3, color = "#D6ACC0", fontface = "bold") +
      annotate("text", 
               x = 9, y = 0.85, 
               label = p_text,
               hjust = 1, vjust = 1, size = 3, fontface = "bold")

    final_plot$labels$subtitle <- NULL
    final_plot$labels$caption <- NULL
    
    return(final_plot)
    
  }, error = function(e) {
    ggplot() +
      ggtitle(paste("Error creating plot for", cancer_type, ":", e$message)) +
      theme_void()
  })
}

#' Convert ggplot to plotly
#' @param gg_plot ggplot object
#' @return plotly object
convert_to_plotly <- function(gg_plot) {
  tryCatch({
    if (!inherits(gg_plot, "ggplot")) {
      return(plot_ly() %>%
        add_annotations(
          text = "Invalid plot object provided",
          showarrow = FALSE
        ))
    }
    
    p <- ggplotly(gg_plot, tooltip = c("x", "y"))
    
    p <- p %>%
      layout(
        showlegend = FALSE  
      )
    
    return(p)
    
  }, error = function(e) {
    plot_ly() %>%
      add_annotations(
        text = paste("Error converting plot to interactive format:", e$message),
        showarrow = FALSE,
        font = list(size = 12)
      )
  })
}
