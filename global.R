# ============================================================================
# LIBRARIES
# ============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(ggpubr)
library(survival)
library(survminer)
library(data.table)
library(readxl)
library(ComplexHeatmap)
library(circlize)
library(grid)

# ============================================================================
# SOURCE MODULES AND UTILITIES
# ============================================================================

source("R/data_processing.R")
source("R/file_handlers.R")
source("R/plotting.R")
source("R/tcga_pcawg_processing.R")
source("R/modules/driver_mutation_module.R")
source("R/modules/tcga_module.R")
source("R/modules/pcawg_module.R")
source("R/modules/survival_module.R")
source("R/modules/tmb_smf_module.R")
source("R/modules/heatmap_module.R")
source("R/modules/phylo_module.R")

# old_path <- Sys.getenv("PATH")
# Sys.setenv(PATH = paste("/usr/local/R/4.2.0/bin", old_path, sep = ":"))
# Sys.setenv(LD_LIBRARY_PATH="/usr/local/gcc/7.0.0/lib64")

# ============================================================================
# CONSTANTS
# ============================================================================

global_TCGA_PCAWG <- "dataSource"
global_www        <- "www"
global_assets     <- "assets"

# Update these three lines when the tutorial video changes
tutorial_video_id   <- "b9yXSX0vxMM"
tutorial_video_date <- "April 2026"
tutorial_video_dur  <- "7 min 23 sec"

tutorial_embed_url  <- paste0(
  "https://www.youtube.com/embed/", tutorial_video_id,
  "?enablejsapi=1&rel=0&modestbranding=1"
)
tutorial_watch_url  <- paste0(
  "https://www.youtube.com/watch?v=", tutorial_video_id
)
tutorial_meta       <- paste0(
  "Recorded ", tutorial_video_date, " · ", tutorial_video_dur
)

addResourcePath(global_assets,     global_assets)
addResourcePath(global_TCGA_PCAWG, global_TCGA_PCAWG)

if (!dir.exists(global_TCGA_PCAWG)) {
  dir.create(global_TCGA_PCAWG, recursive = TRUE, showWarnings = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

DOWNLOAD_CONFIG <- list(
  password = "wwylab_CliPP",
  base_url = "https://odin.mdacc.tmc.edu/~wwang7/CliPP_data_share",
  urls = list(
    simulation = "Sim4kdata.html",
    tcga = "TCGAdata.html",
    pcawg = "PCAWGdata.html"
  )
)

get_download_url <- function(dataset) {
  if (dataset %in% names(DOWNLOAD_CONFIG$urls)) {
    return(paste0(DOWNLOAD_CONFIG$base_url, "/", DOWNLOAD_CONFIG$urls[[dataset]]))
  }
  return(NULL)
}

driver_mutation_data <- tryCatch(
  {
    file_path <- file.path(global_TCGA_PCAWG, "Driver_info.csv")
    if (!file.exists(file_path)) {
      warning("Driver mutation data file not found at: ", file_path)
      data.frame()
    } else {
      df <- read.csv(file_path, stringsAsFactors = FALSE)

      required_cols <- c("tcga_id", "Hugo_Symbol", "cancer_type", "Subclonality",
                         "HGVSp_Short", "Variant_Classification", "VAF", "CP_unpenalized")
      missing_cols <- setdiff(required_cols, colnames(df))

      if (length(missing_cols) > 0) {
        warning("Missing columns in driver data: ", paste(missing_cols, collapse = ", "))
        data.frame()
      } else {
        df$sample      <- df$tcga_id
        df$gene        <- df$Hugo_Symbol
        df$protein     <- df$HGVSp_Short
        df$consequence <- df$Variant_Classification
        df$cancer      <- df$cancer_type

        df$Subclonality <- as.character(df$Subclonality)
        df
      }
    }
  },
  error = function(e) {
    warning("Error loading driver mutation data: ", e$message)
    data.frame()
  }
)

driver_cancer_types <- if (nrow(driver_mutation_data) > 0) {
  sort(unique(driver_mutation_data$cancer_type))
} else {
  character(0)
}

sample_TCGA <- tryCatch(
  {
    file_path <- file.path(global_TCGA_PCAWG, "CliPP_TCGA.tsv")
    if (!file.exists(file_path)) {
      warning("TCGA sample data file not found at: ", file_path)
      data.frame(samplename = character(), cancer_type = character())
    } else {
      read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    }
  },
  error = function(e) {
    warning("Error loading TCGA sample data: ", e$message)
    data.frame(samplename = character(), cancer_type = character())
  }
)

sample_PCAWG <- tryCatch(
  {
    file_path <- file.path(global_TCGA_PCAWG, "CliPP_PCAWG.tsv")
    if (!file.exists(file_path)) {
      warning("PCAWG sample data file not found at: ", file_path)
      data.frame(samplename = character(), cancer_type = character())
    } else {
      read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    }
  },
  error = function(e) {
    warning("Error loading PCAWG sample data: ", e$message)
    data.frame(samplename = character(), cancer_type = character())
  }
)

if (nrow(sample_TCGA) == 0) {
  sample_TCGA <- data.frame(
    samplename = c("TCGA-OR-A5J1", "TCGA-OR-A5J2", "TCGA-OR-A5J6", "TCGA-3L-AA1B", "TCGA-N5-A4RA"),
    cancer_type = c("BLCA", "BLCA", "BLCA", "BLCA", "BLCA"),
    stringsAsFactors = FALSE
  )
}

if (nrow(sample_PCAWG) == 0) {
  sample_PCAWG <- data.frame(
    samplename = c("PCAWG-SA123456", "PCAWG-SA234567", "PCAWG-SA345678"),
    cancer_type = c("LUAD", "LUAD", "LUSC"),
    stringsAsFactors = FALSE
  )
}

local({
  tcga_file <- file.path(global_TCGA_PCAWG, "7279_final_cohort_result.txt")
  pcawg_file <- file.path(
    global_TCGA_PCAWG,
    "CliP_PCAWG_summary_table_debug_updated_sMF_TMB.txt"
  )

  tcga_out <- tryCatch({
    df <- data.table::fread(tcga_file, select = "cancer_type")
    counts <- df[, .N, by = cancer_type][order(-N)]
    data.table::setnames(counts, c("Cancer Type", "Samples"))
    list(n = nrow(df), types = nrow(counts), counts = as.data.frame(counts))
  }, error = function(e) list(n = NA, types = NA, counts = NULL))

  pcawg_out <- tryCatch({
    df <- data.table::fread(pcawg_file, select = "cancer_type")
    counts <- df[, .N, by = cancer_type][order(-N)]
    data.table::setnames(counts, c("Cancer Type", "Samples"))
    list(n = nrow(df), types = nrow(counts), counts = as.data.frame(counts))
  }, error = function(e) list(n = NA, types = NA, counts = NULL))

  dl_n_tcga      <<- if (is.na(tcga_out$n))    "—" else format(tcga_out$n,     big.mark = ",")
  dl_types_tcga  <<- if (is.na(tcga_out$types)) "—" else as.character(tcga_out$types)
  dl_n_pcawg     <<- if (is.na(pcawg_out$n))   "—" else format(pcawg_out$n,    big.mark = ",")
  dl_types_pcawg <<- if (is.na(pcawg_out$types)) "—" else as.character(pcawg_out$types)
  dl_tcga_counts <<- tcga_out$counts
  dl_pcawg_counts <<- pcawg_out$counts
})

cohort_cancer_mapping <- list(
  "TCGA" = c(
    "ACC", "BLCA", "BRCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA",
    "GBM", "HNSC", "KICH", "KIRC", "KIRP", "LAML", "LGG", "LIHC",
    "LUAD", "LUSC", "MESO", "OV", "PAAD", "PCPG", "PRAD", "READ",
    "SARC", "SKCM", "STAD", "TGCT", "THCA", "THYM", "UCEC", "UCS", "UVM"
  ),
  "OCCAMS" = c("ESCC", "EAC"),
  "NCT02113657 (IPI)" = c("Melanoma", "Lung Cancer", "Bladder Cancer", "Renal Cancer"),
  "NCT02703623 (ARPi+IPI)" = c("Prostate Cancer")
)

get_cancer_types_for_cohort <- function(cohort) {
  if (cohort %in% names(cohort_cancer_mapping)) {
    return(cohort_cancer_mapping[[cohort]])
  } else {
    return(character(0))
  }
}

get_available_cohorts <- function() {
  return(names(cohort_cancer_mapping))
}

conditional_select_input <- function(inputId, label, choices = NULL, selected = NULL,
                                     multiple = FALSE, selectize = TRUE, width = NULL,
                                     size = NULL, disabled = FALSE) {
  if (disabled) {
    div(
      style = "opacity: 0.6; pointer-events: none;",
      selectInput(
        inputId = inputId, label = label, choices = choices,
        selected = selected, multiple = multiple, selectize = selectize,
        width = width, size = size
      )
    )
  } else {
    selectInput(
      inputId = inputId, label = label, choices = choices,
      selected = selected, multiple = multiple, selectize = selectize,
      width = width, size = size
    )
  }
}

tmb_smf_data <- tryCatch(
  {
    load_tmb_smf_data()
  },
  error = function(e) {
    warning("Error loading TMB vs sMF data: ", e$message)
    data.frame()
  }
)

if (nrow(tmb_smf_data) > 0) {
  cohort_summary <- tmb_smf_data %>%
    group_by(cohort) %>%
    summarise(n_samples = n(), n_cancer_types = length(unique(cancer_type)), .groups = "drop")
}

heatmap_data <- tryCatch(
  {
    load_heatmap_data()
  },
  error = function(e) {
    warning("Error loading heatmap data: ", e$message)
    data.frame()
  }
)

if (nrow(driver_mutation_data) == 0) {
  driver_mutation_data <- data.frame(
    gene = c("TP53", "KRAS", "PIK3CA", "BRAF", "EGFR"),
    sample = c("TCGA-OR-A5J1", "TCGA-OR-A5J1", "TCGA-OR-A5J2", "TCGA-3L-AA1B", "TCGA-N5-A4RA"),
    cancer_type = c("BLCA", "BLCA", "BLCA", "BLCA", "BLCA"),
    cancer = c("BLCA", "BLCA", "BLCA", "BLCA", "BLCA"),
    Subclonality = c("Clonal", "Subclonal", "Clonal", "Clonal", "Subclonal"),
    protein = c("p.R175H", "p.G12D", "p.E545K", "p.V600E", "p.L858R"),
    consequence = c("Missense", "Missense", "Missense", "Missense", "Missense"),
    VAF = c(0.42, 0.18, 0.38, 0.45, 0.22),
    CP_unpenalized = c(0.84, 0.36, 0.76, 0.90, 0.44),
    stringsAsFactors = FALSE
  )
}
