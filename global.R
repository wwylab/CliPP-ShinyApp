library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(ggpubr)

# Source module and utility files
source("R/modules/driver_mutation_module.R")
source("R/modules/tcga_module.R")
source("R/modules/pcawg_module.R")
source("R/plotting.R")
source("R/data_processing.R")
source("R/file_handlers.R")

#old_path <- Sys.getenv("PATH")
#Sys.setenv(PATH = paste("/usr/local/R/4.2.0/bin", old_path, sep = ":"))
#Sys.setenv(LD_LIBRARY_PATH="/usr/local/gcc/7.0.0/lib64")

# Define global constants and paths
global_TCGA_PCAWG <- "dataSource"

# Make sure required directories exist
if (!dir.exists("data")) {
  dir.create("data", showWarnings = FALSE)
}

if (!dir.exists(global_TCGA_PCAWG)) {
  dir.create(global_TCGA_PCAWG, recursive = TRUE, showWarnings = FALSE)
}

# Define helper functions
`%||%` <- function(x, y) if (is.null(x)) y else x

# Load driver mutation data
driver_mutation_data <- tryCatch({
  # CHANGED: Load driver data from the same location as PCAWG and TCGA data
  file_path <- file.path(global_TCGA_PCAWG, "driver_mut_data.tsv")
  if (!file.exists(file_path)) {
    warning("Driver mutation data file not found at: ", file_path)
    data.frame()
  } else {
    # Read the data
    df <- read.table(file_path,
                     header = TRUE,
                     sep = '\t',  # Changed to tab separator to match other files
                     stringsAsFactors = FALSE)
    
    # Basic validation
    required_cols <- c("gene", "sample", "cancer", "Subclonality")
    missing_cols <- setdiff(required_cols, colnames(df))
    
    if (length(missing_cols) > 0) {
      warning("Missing columns in driver data: ", paste(missing_cols, collapse = ", "))
      data.frame()
    } else {
      # Rename the "cancer" column to "cancer_type" to match other datasets
      if ("cancer" %in% colnames(df) && !("cancer_type" %in% colnames(df))) {
        df$cancer_type <- df$cancer
      }
      
      # Ensure Subclonality is character type, not factor
      df$Subclonality <- as.character(df$Subclonality)
      df
    }
  }
}, error = function(e) {
  warning("Error loading driver mutation data: ", e$message)
  data.frame()
})

# Load TCGA sample data
sample_TCGA <- tryCatch({
  file_path <- file.path(global_TCGA_PCAWG, "CliPP_TCGA.tsv") 
  if (!file.exists(file_path)) {
    warning("TCGA sample data file not found at: ", file_path)
    data.frame(samplename = character(), cancer_type = character())
  } else {
    read.table(file_path, header = TRUE, sep = '\t', stringsAsFactors = FALSE)  # Note: using sep='\t' for TSV files
  }
}, error = function(e) {
  warning("Error loading TCGA sample data: ", e$message)
  data.frame(samplename = character(), cancer_type = character())
})

# Load PCAWG sample data
sample_PCAWG <- tryCatch({
  file_path <- file.path(global_TCGA_PCAWG, "CliPP_PCAWG.tsv") 
  if (!file.exists(file_path)) {
    warning("PCAWG sample data file not found at: ", file_path)
    data.frame(samplename = character(), cancer_type = character())
  } else {
    read.table(file_path, header = TRUE, sep = '\t', stringsAsFactors = FALSE)  # Note: using sep='\t' for TSV files
  }
}, error = function(e) {
  warning("Error loading PCAWG sample data: ", e$message)
  data.frame(samplename = character(), cancer_type = character())
})

# Print loading status to console for debugging
cat("Data loading complete:\n")
cat("- Driver mutations:", nrow(driver_mutation_data), "rows\n")
cat("- TCGA samples:", nrow(sample_TCGA), "rows\n")
cat("- PCAWG samples:", nrow(sample_PCAWG), "rows\n")

# Create sample data if missing
if (nrow(sample_TCGA) == 0) {
  sample_TCGA <- data.frame(
    samplename = c("TCGA-OR-A5J1", "TCGA-OR-A5J2", "TCGA-OR-A5J6", "TCGA-3L-AA1B", "TCGA-N5-A4RA"),
    cancer_type = c("BLCA", "BLCA", "BLCA", "BLCA", "BLCA"),
    stringsAsFactors = FALSE
  )
  cat("Created sample TCGA data with", nrow(sample_TCGA), "samples\n")
}

if (nrow(sample_PCAWG) == 0) {
  sample_PCAWG <- data.frame(
    samplename = c("PCAWG-SA123456", "PCAWG-SA234567", "PCAWG-SA345678"),
    cancer_type = c("LUAD", "LUAD", "LUSC"),
    stringsAsFactors = FALSE
  )
  cat("Created sample PCAWG data with", nrow(sample_PCAWG), "samples\n")
}

# Create example driver mutation data if missing
if (nrow(driver_mutation_data) == 0) {
  driver_mutation_data <- data.frame(
    gene = c("TP53", "KRAS", "PIK3CA", "BRAF", "EGFR"),
    sample = c("TCGA-OR-A5J1", "TCGA-OR-A5J1", "TCGA-OR-A5J2", "TCGA-3L-AA1B", "TCGA-N5-A4RA"),
    cancer_type = c("BLCA", "BLCA", "BLCA", "BLCA", "BLCA"),
    cancer = c("BLCA", "BLCA", "BLCA", "BLCA", "BLCA"),  # Added 'cancer' column to match expected format
    Subclonality = c("Clonal", "Subclonal", "Clonal", "Clonal", "Subclonal"),
    protein = c("p.R175H", "p.G12D", "p.E545K", "p.V600E", "p.L858R"),
    consequence = c("Missense", "Missense", "Missense", "Missense", "Missense"),
    VAF = c(0.42, 0.18, 0.38, 0.45, 0.22),
    CP_unpenalized = c(0.84, 0.36, 0.76, 0.90, 0.44),
    stringsAsFactors = FALSE
  )
  cat("Created sample driver mutation data with", nrow(driver_mutation_data), "mutations\n")
}