# Below these, subclonal reconstruction is unreliable no matter how clean the
# run looks: too few tumor cells, or too few reads per clonal copy, and the
# clusters are being fitted to noise.
MIN_PURITY <- 0.4
MIN_NRPCC  <- 10

#' Length-weighted average copy number across the CNA segments
sample_ploidy <- function(df_cna) {
  if (is.null(df_cna) || !all(c("start_position", "end_position", "total_cn") %in% names(df_cna))) {
    return(NA_real_)
  }
  len <- as.numeric(df_cna$end_position) - as.numeric(df_cna$start_position)
  cn  <- suppressWarnings(as.numeric(df_cna$total_cn))
  ok  <- is.finite(len) & is.finite(cn) & len > 0
  if (!any(ok)) return(NA_real_)
  sum(len[ok] * cn[ok]) / sum(len[ok])
}

#' Flag samples whose input cannot support a reliable reconstruction
#'
#' nrpcc is the number of reads per clonal copy: mean depth scaled by how much
#' of the sample is tumor and diluted by how many copies that tumor carries.
#' It is the same quantity the TCGA and PCAWG cohort tables report.
#'
#' @return list(purity, ploidy, nrpcc, low_purity, low_nrpcc, flagged)
sample_quality <- function(purity, mean_depth, df_cna) {
  ploidy <- sample_ploidy(df_cna)
  nrpcc  <- if (is.finite(ploidy) && ploidy > 0 && is.finite(mean_depth) && is.finite(purity)) {
    mean_depth * purity / ploidy
  } else {
    NA_real_
  }
  low_purity <- is.finite(purity) && purity < MIN_PURITY
  low_nrpcc  <- is.finite(nrpcc) && nrpcc < MIN_NRPCC
  list(purity = purity, ploidy = ploidy, nrpcc = nrpcc,
       low_purity = low_purity, low_nrpcc = low_nrpcc,
       flagged = low_purity || low_nrpcc)
}

# Where both smoothing sliders start, before a sample is loaded. Once results
# arrive the sliders move to recommended_smoothing_factor() for that sample.
SMOOTHING_DEFAULT <- 0.3

#' Recommended smoothing factor for a sample's mutation count
#'
#' Four bins rather than a formula. Fewer mutations means a noisier kernel
#' density, so small samples need more smoothing to be readable.
#'
#' The fall-off with size is deliberately gentle: R's default bandwidth already
#' shrinks with sample size (proportional to n^-1/5), so `adjust` below ~0.3 on
#' a large sample shrinks it twice over and the curve breaks up into spikes.
#' Checked visually at n = 644 and n = 1,458, where 0.25 and 0.20 were visibly
#' jittery while 0.30 and 0.25 read cleanly.
#'
#' The values were read off CP densities for TCGA samples at each size
#' (n = 55, 182, 644, 1458) rendered at adjust 0.20-0.45. The previous formula
#' ran from 0.7 down to 0.2 over 100-10,000 mutations and recommended ~0.69 for
#' a few hundred mutations, which flattened real structure.
#'
#' @param total_mutations The total number of mutations
#' @return Numeric smoothing factor
recommended_smoothing_factor <- function(total_mutations) {
  n <- suppressWarnings(as.numeric(total_mutations))
  if (length(n) != 1 || is.na(n)) return(SMOOTHING_DEFAULT)

  if (n < 100)  return(0.45)   # sparse: individual mutations show as spikes
  if (n < 500)  return(0.30)   # the usual exome case
  if (n < 1000) return(0.30)
  0.25                          # dense enough that less is needed, but only a little
}
