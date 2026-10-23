# ============================================================================
# LIBRARIES
# ============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(ggpubr)
library(data.table)

# ============================================================================
# SOURCE MODULES AND UTILITIES
# ============================================================================

source("R/data_processing.R")
source("R/file_handlers.R")
source("R/plotting.R")
source("R/silhouette.R")

# old_path <- Sys.getenv("PATH")
# Sys.setenv(PATH = paste("/usr/local/R/4.2.0/bin", old_path, sep = ":"))
# Sys.setenv(LD_LIBRARY_PATH="/usr/local/gcc/7.0.0/lib64")

# ============================================================================
# CONSTANTS
# ============================================================================

# App version and content date, shown in the footer and on the About page.
# Update these two lines when deploying a new build.
APP_BUILD   <- "build 2.6.1"
APP_UPDATED <- "October 22, 2026"

global_TCGA_PCAWG <- "dataSource"
global_assets     <- "assets"

addResourcePath(global_assets,     global_assets)
addResourcePath(global_TCGA_PCAWG, global_TCGA_PCAWG)

if (!dir.exists(global_TCGA_PCAWG)) {
  dir.create(global_TCGA_PCAWG, recursive = TRUE, showWarnings = FALSE)
}

#' Size of a file in dataSource, formatted for the Downloads page
#'
#' The size labels used to be typed in by hand and drifted badly as the data was
#' rebuilt: the TCGA archive was labelled 372 MB when it had become 43 MB, and
#' the driver table 12 MB when it had become 1.5 MB. Reading the file means the
#' label cannot disagree with the download.
file_size_label <- function(name, dir = global_TCGA_PCAWG) {
  path <- file.path(dir, name)
  if (!file.exists(path)) return("--")
  b <- file.size(path)
  if (b >= 10 * 1048576) sprintf("%.0f MB", b / 1048576)
  else if (b >= 1048576) sprintf("%.1f MB", b / 1048576)
  else sprintf("%.0f KB", b / 1024)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================================
# CLIPP KERNEL CHECK (runs once at startup)
#
# CliPP-on-Web shells out to the CliPP 1.4.0 source tree in forCliPP/CliPP.
# 1.4.0 dlopens its kernel from build/*/CliPP*<platform>*.so, so the kernel has
# to be compiled on the machine that serves the app -- a macOS .so is useless on
# the Linux deployment host and vice versa. Fail loudly in the log at startup
# rather than per-upload, so a bad deploy is obvious before any user hits it.
# ============================================================================

local({
  kernel <- clipp_kernel_path()
  if (is.na(kernel)) {
    warning(immediate. = TRUE, paste0(
      "\n",
      "=========================================================================\n",
      " CliPP KERNEL NOT BUILT FOR THIS PLATFORM (", clipp_platform_tag(), ")\n",
      " CliPP-on-Web uploads will fail until it is compiled HERE:\n",
      "     cd forCliPP && ./build_clipp.sh\n",
      " See forCliPP/README.md.\n",
      "=========================================================================\n"
    ))
  } else {
    message("CliPP kernel: ", kernel)
  }
})

# ============================================================================
# DOWNLOADS TAB DATA
# ============================================================================

driver_mutation_data <- tryCatch(
  {
    file_path <- file.path(global_TCGA_PCAWG, "Driver_info.csv")
    if (!file.exists(file_path)) {
      warning("Driver mutation data file not found at: ", file_path)
      data.frame()
    } else {
      read.csv(file_path, stringsAsFactors = FALSE)
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

local({
  # Both counts come from the same cohort files cohort_cancer_download() matches
  # against, so the Downloads dropdowns can never offer a cancer type with no
  # samples. Earlier versions read separate summary tables here, which drifted
  # from the cohorts and used different cancer-type spellings ("CRC" versus
  # "COAD"/"READ"), so the dropdown listed a type that matched nothing and hid
  # two that existed. Those summary tables are no longer shipped.
  tcga_file  <- file.path(global_TCGA_PCAWG, "CliPP_TCGA.tsv")
  pcawg_file <- file.path(global_TCGA_PCAWG, "CliPP_PCAWG.tsv")

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
