#' Mean silhouette width of 1D values under the given cluster labels
#'
#' Exact, O(n log n) equivalent of the original pipeline's silhouette_1d():
#' per-cluster sorted cumulative sums give each point's summed absolute
#' distance to every cluster without the O(n^2) pairwise loop. As in the
#' original, a point in a singleton cluster gets a = 0.
#' @param x Numeric vector (per-mutation CCF)
#' @param labels Cluster label per value
#' @return Mean silhouette width, or NA if fewer than 2 clusters or 2 values
silhouette_1d <- function(x, labels) {
  n <- length(x)
  labels <- as.integer(factor(labels))
  K <- length(unique(labels))
  if (K < 2 || n < 2) return(NA_real_)

  dist_to_cluster <- matrix(0, nrow = n, ncol = K)
  for (k in seq_len(K)) {
    v <- sort(x[labels == k])
    m <- length(v)
    cs <- cumsum(v)
    below <- findInterval(x, v)
    sum_below <- ifelse(below > 0, cs[pmax(below, 1)], 0)
    total_dist <- x * below - sum_below + (cs[m] - sum_below) - x * (m - below)
    denom <- ifelse(labels == k, m - 1, m)
    dist_to_cluster[, k] <- ifelse(denom <= 0, 0, total_dist / pmax(denom, 1))
  }

  a <- dist_to_cluster[cbind(seq_len(n), labels)]
  dist_to_cluster[cbind(seq_len(n), labels)] <- Inf
  b <- apply(dist_to_cluster, 1, min)
  s <- ifelse(pmax(a, b) == 0, 0, (b - a) / pmax(a, b))
  mean(s)
}

#' Silhouette score (sil_full) of a CliPP run's clustering
#'
#' Uses CliPP's purity-aware per-mutation cellular prevalence
#' (manuscript Eq. 1 solved for phi), with CliPP's multiplicity (minor.txt):
#'   phi = vaf * (ploidy - purity * ploidy + purity * total) / minor,  ploidy = 2
#'   ccf = phi / purity
#' @param result_dir CliPP output directory (holds preprocess_result/ and final_result/)
#' @param min_mut Minimum number of usable mutations to score
#' @return List with status ("ok", "single_cluster", "too_few_mutations",
#'   "unavailable"), sil_full, n_mutations and n_clusters
compute_silhouette_score <- function(result_dir = "forCliPP/CliPP/sample_id", min_mut = 5) {
  unavailable <- list(status = "unavailable", sil_full = NA_real_,
                      n_mutations = NA_integer_, n_clusters = NA_integer_)

  pre <- file.path(result_dir, "preprocess_result")
  ma_f <- list.files(file.path(result_dir, "final_result", "Best_lambda"),
                     pattern = "^mutation_assignments_lam.*\\.txt$", full.names = TRUE)
  pre_files <- file.path(pre, c("r.txt", "n.txt", "minor.txt", "total.txt"))
  if (length(ma_f) == 0 || !all(file.exists(pre_files))) return(unavailable)

  tryCatch({
    ma      <- read.table(ma_f[1], header = TRUE, sep = "\t")
    r_alt   <- scan(pre_files[1], quiet = TRUE)
    n_total <- scan(pre_files[2], quiet = TRUE)
    minor   <- scan(pre_files[3], quiet = TRUE)
    total   <- scan(pre_files[4], quiet = TRUE)
    if (length(r_alt) != nrow(ma)) return(unavailable)

    pur_f <- file.path(pre, "purity_ploidy.txt")
    purity <- if (file.exists(pur_f))
                as.numeric(readLines(pur_f, n = 1, warn = FALSE)) else 1
    if (is.na(purity) || purity <= 0) purity <- 1

    vaf <- r_alt / n_total
    ploidy <- 2
    phi <- vaf * (ploidy - purity * ploidy + purity * total) / minor
    ccf <- phi / purity

    keep <- is.finite(ccf)
    ccf <- ccf[keep]
    labels <- ma$cluster_index[keep]
    n <- length(ccf)
    K <- length(unique(labels))

    status <- if (n < min_mut) "too_few_mutations" else if (K < 2) "single_cluster" else "ok"
    list(
      status      = status,
      sil_full    = if (status == "ok") silhouette_1d(ccf, labels) else NA_real_,
      n_mutations = n,
      n_clusters  = K
    )
  }, error = function(e) unavailable)
}
