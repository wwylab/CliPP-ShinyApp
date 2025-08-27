library(shiny)
library(dplyr)
library(readr)
library(plotly)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(ggpubr)
library(survival)
library(survminer)
library(rpart)
library(data.table)
library(readxl)
library(corrr)

source("R/modules/driver_mutation_module.R")
source("R/modules/tcga_module.R")
source("R/modules/pcawg_module.R")
source("R/modules/survival_module.R")
source("R/modules/tmb_smf_module.R")
source("R/plotting.R")
source("R/data_processing.R")
source("R/file_handlers.R")

# Sylvain
#old_path <- Sys.getenv("PATH")
#Sys.setenv(PATH = paste("/usr/local/R/4.2.0/bin", old_path, sep = ":"))
#Sys.setenv(LD_LIBRARY_PATH="/usr/local/gcc/7.0.0/lib64")

global_TCGA_PCAWG <- "dataSource"

if (!dir.exists("data")) {
  dir.create("data", showWarnings = FALSE)
}

if (!dir.exists(global_TCGA_PCAWG)) {
  dir.create(global_TCGA_PCAWG, recursive = TRUE, showWarnings = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

driver_mutation_data <- tryCatch({
  file_path <- file.path(global_TCGA_PCAWG, "driver_mut_data.tsv")
  if (!file.exists(file_path)) {
    warning("Driver mutation data file not found at: ", file_path)
    data.frame()
  } else {
    df <- read.table(file_path,
                     header = TRUE,
                     sep = '\t',  
                     stringsAsFactors = FALSE)
    
    required_cols <- c("gene", "sample", "cancer", "Subclonality")
    missing_cols <- setdiff(required_cols, colnames(df))
    
    if (length(missing_cols) > 0) {
      warning("Missing columns in driver data: ", paste(missing_cols, collapse = ", "))
      data.frame()
    } else {
      if ("cancer" %in% colnames(df) && !("cancer_type" %in% colnames(df))) {
        df$cancer_type <- df$cancer
      }
      
      df$Subclonality <- as.character(df$Subclonality)
      df
    }
  }
}, error = function(e) {
  warning("Error loading driver mutation data: ", e$message)
  data.frame()
})

sample_TCGA <- tryCatch({
  file_path <- file.path(global_TCGA_PCAWG, "CliPP_TCGA.tsv") 
  if (!file.exists(file_path)) {
    warning("TCGA sample data file not found at: ", file_path)
    data.frame(samplename = character(), cancer_type = character())
  } else {
    read.table(file_path, header = TRUE, sep = '\t', stringsAsFactors = FALSE)  
  }
}, error = function(e) {
  warning("Error loading TCGA sample data: ", e$message)
  data.frame(samplename = character(), cancer_type = character())
})

sample_PCAWG <- tryCatch({
  file_path <- file.path(global_TCGA_PCAWG, "CliPP_PCAWG.tsv") 
  if (!file.exists(file_path)) {
    warning("PCAWG sample data file not found at: ", file_path)
    data.frame(samplename = character(), cancer_type = character())
  } else {
    read.table(file_path, header = TRUE, sep = '\t', stringsAsFactors = FALSE)  
  }
}, error = function(e) {
  warning("Error loading PCAWG sample data: ", e$message)
  data.frame(samplename = character(), cancer_type = character())
})

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

cohort_cancer_mapping <- list(
  "TCGA" = c("ACC", "BLCA", "BRCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA", 
             "GBM", "HNSC", "KICH", "KIRC", "KIRP", "LAML", "LGG", "LIHC", 
             "LUAD", "LUSC", "MESO", "OV", "PAAD", "PCPG", "PRAD", "READ", 
             "SARC", "SKCM", "STAD", "TGCT", "THCA", "THYM", "UCEC", "UCS", "UVM"),
  "OCCAMS" = c("ESCC", "EAC"),
  "NCT02113657 (IPI)" = c("Melanoma", "Lung Cancer", "Bladder Cancer", "Renal Cancer"),
  "NCT02703623 (ARPi+IPI)" = c("Prostate Cancer")
)

# Function to get cancer types for a specific cohort
get_cancer_types_for_cohort <- function(cohort) {
  if (cohort %in% names(cohort_cancer_mapping)) {
    return(cohort_cancer_mapping[[cohort]])
  } else {
    return(character(0))
  }
}

# Function to get all available cohorts
get_available_cohorts <- function() {
  return(names(cohort_cancer_mapping))
}

# Function to create conditional select input that can be deactivated
conditional_select_input <- function(inputId, label, choices = NULL, selected = NULL, 
                                   multiple = FALSE, selectize = TRUE, width = NULL, 
                                   size = NULL, disabled = FALSE) {
  if (disabled) {
    div(
      style = "opacity: 0.6; pointer-events: none;",
      selectInput(inputId = inputId, label = label, choices = choices, 
                 selected = selected, multiple = multiple, selectize = selectize, 
                 width = width, size = size)
    )
  } else {
    selectInput(inputId = inputId, label = label, choices = choices, 
               selected = selected, multiple = multiple, selectize = selectize, 
               width = width, size = size)
  }
}

tmb_smf_data <- tryCatch({
  load_tmb_smf_data()
}, error = function(e) {
  warning("Error loading TMB vs sMF data: ", e$message)
  data.frame()
})

if (nrow(tmb_smf_data) > 0) {
  cohort_summary <- tmb_smf_data %>%
    group_by(cohort) %>%
    summarise(n_samples = n(), n_cancer_types = length(unique(cancer_type)), .groups = 'drop')
} 

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
