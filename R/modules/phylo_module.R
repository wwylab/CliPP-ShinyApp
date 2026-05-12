# Phylogenetic Tree Module
# Builds a tumor evolution tree from CliPP cluster CCF data.
# Topology rule (CONIPHER-inspired): parent CCF > child CCF.
# Germline root node (CCF = 1.0) is always added above the clonal cluster.

library(dplyr)

# ---- Tree construction --------------------------------------------------

#' Build edge list from cluster CCF summary
#' Uses the Sum Rule to allow branching (siblings) when possible.
build_phylo_edges <- function(cluster_df) {
  if (is.null(cluster_df) || nrow(cluster_df) == 0) return(NULL)

  gl <- data.frame(cluster_id = "GL", ccf = 1.0, n_muts = 0,
                   stringsAsFactors = FALSE)
  df <- rbind(gl, cluster_df)
  df <- df[order(-df$ccf), ]   # descending CCF

  edges <- list()
  current_children_sum <- setNames(rep(0, nrow(df)), df$cluster_id)
  for (i in 2:nrow(df)) {
    child_id <- df$cluster_id[i]
    child_ccf <- df$ccf[i]
    
    # Shallowest valid parent first — favors branching over deep nesting
    potential_parents <- df[df$ccf > child_ccf, ]
    potential_parents <- potential_parents[order(-potential_parents$ccf), ] # largest first
    
    found_parent <- FALSE
    for (j in 1:nrow(potential_parents)) {
      p_id <- potential_parents$cluster_id[j]
      p_ccf <- potential_parents$ccf[j]
      
      if (length(p_ccf) == 0 || length(child_ccf) == 0 || is.na(p_ccf) || is.na(child_ccf)) next
      # +0.01 tolerance for floating-point noise in CliPP CCF estimates
      p_sum <- current_children_sum[p_id]
      if (is.na(p_sum)) p_sum <- 0
      
      if (p_sum + child_ccf <= p_ccf + 0.01) {
        edges[[child_id]] <- data.frame(
          parent        = p_id,
          child         = child_id,
          branch_length = p_ccf - child_ccf,
          stringsAsFactors = FALSE
        )
        current_children_sum[p_id] <- p_sum + child_ccf
        found_parent <- TRUE
        break
      }
    }
    
    # No parent satisfied the sum rule; force nest under the closest ancestor
    if (!found_parent) {
      closest_p <- potential_parents[1, ]
      edges[[child_id]] <- data.frame(
        parent        = closest_p$cluster_id,
        child         = child_id,
        branch_length = closest_p$ccf - child_ccf,
        stringsAsFactors = FALSE
      )
    }
  }

  edge_df <- do.call(rbind, Filter(Negate(is.null), edges))
  edge_df
}

#' Summarise per-cluster CCF from mutation-level data
summarise_clusters <- function(merged_data, df_subc = NULL) {
  # df_subc (subclonal_structure) has pre-aggregated CCF; use it over per-mutation data when available
  if (!is.null(df_subc) && nrow(df_subc) > 0 &&
      "cellular_prevalence" %in% names(df_subc)) {

    n_col <- if ("num_SNV" %in% names(df_subc)) "num_SNV" else NULL
    cluster_id <- if ("cluster_index" %in% names(df_subc)) {
      as.character(df_subc$cluster_index)
    } else {
      as.character(seq_len(nrow(df_subc)))
    }

    out <- data.frame(
      cluster_id = cluster_id,
      ccf        = as.numeric(df_subc$cellular_prevalence),
      n_muts     = if (!is.null(n_col)) as.integer(df_subc[[n_col]]) else 0L,
      stringsAsFactors = FALSE
    )
    return(out[!is.na(out$ccf), ])
  }

  # Fall back to per-mutation merged_data when df_subc is absent
  if (is.null(merged_data) || nrow(merged_data) == 0) return(NULL)

  ccf_col <- if ("CCF_unpenalized" %in% names(merged_data)) "CCF_unpenalized" else
    if ("CP_unpenalized" %in% names(merged_data)) "CP_unpenalized" else NULL
  if (is.null(ccf_col)) return(NULL)

  cluster_col <- if ("cluster_index" %in% names(merged_data)) "cluster_index" else NULL
  if (is.null(cluster_col)) return(NULL)

  out <- merged_data %>%
    dplyr::group_by(cluster_id = as.character(.data[[cluster_col]])) %>%
    dplyr::summarise(
      ccf    = median(.data[[ccf_col]], na.rm = TRUE),
      n_muts = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(ccf)) %>%
    as.data.frame()

  out
}

# ---- Layout Helper ------------------------------------------------------

#' Calculate node positions and attributes for the phylogenetic tree
calculate_phylo_layout <- function(edge_df, cluster_df) {
  if (is.null(edge_df) || nrow(edge_df) == 0) return(NULL)

  all_nodes <- unique(c(edge_df$parent, edge_df$child))
  n_nodes   <- length(all_nodes)

  ccf_lookup <- setNames(as.numeric(cluster_df$ccf), as.character(cluster_df$cluster_id))
  ccf_lookup["GL"] <- 1.0
  
  n_muts_lookup <- setNames(as.integer(cluster_df$n_muts), as.character(cluster_df$cluster_id))
  n_muts_lookup["GL"] <- 0L
  
  y_pos <- ccf_lookup[all_nodes]
  y_pos[is.na(y_pos)] <- 0.5

  x_pos <- setNames(rep(NA_real_, n_nodes), all_nodes)
  x_pos["GL"] <- 0.5

  assign_x <- function(parent, x_center, width) {
    children <- edge_df$child[edge_df$parent == parent]
    if (length(children) == 0) return()
    step <- width / length(children)
    starts <- x_center - width / 2 + step * (seq_along(children) - 0.5)
    for (k in seq_along(children)) {
      x_pos[[children[k]]] <<- starts[k]
      assign_x(children[k], starts[k], step * 0.9)
    }
  }
  assign_x("GL", 0.5, 0.9)
  x_pos[is.na(x_pos)] <- 0.5

  non_gl <- setdiff(all_nodes, "GL")
  pal <- if (length(non_gl) > 0) {
    cols <- grDevices::colorRampPalette(
      c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F",
        "#EDC948","#B07AA1","#FF9DA7","#9C755F","#BAB0AC")
    )(length(non_gl))
    setNames(cols, non_gl)
  } else character(0)
  col_lookup <- c(pal, GL = "#FFFFFF")

  max_muts <- max(c(as.numeric(n_muts_lookup[non_gl]), 1), na.rm = TRUE)
  radius_lookup <- sapply(all_nodes, function(n) {
    nm <- as.numeric(n_muts_lookup[n])
    if (is.na(nm)) nm <- 0
    0.025 + 0.05 * sqrt(nm / max_muts)
  })

  x_pos <- resolve_node_overlaps(x_pos, y_pos, radius_lookup)

  list(
    all_nodes     = all_nodes,
    x_pos         = x_pos,
    y_pos         = y_pos,
    ccf_lookup    = ccf_lookup,
    n_muts_lookup = n_muts_lookup,
    col_lookup    = col_lookup,
    radius_lookup = radius_lookup
  )
}

# ---- Overlap resolution -------------------------------------------------

#' Push nodes apart in x until no circles overlap.
#' y positions (CCF) are never modified.
resolve_node_overlaps <- function(x_pos, y_pos, radius_lookup,
                                  min_gap = 0.02, max_iter = 500) {
  nodes <- names(x_pos)
  n <- length(nodes)
  if (n < 2) return(x_pos)

  for (iter in seq_len(max_iter)) {
    moved <- FALSE
    for (i in seq_len(n - 1)) {
      for (j in seq(i + 1, n)) {
        ni <- nodes[i]; nj <- nodes[j]
        dx <- x_pos[nj] - x_pos[ni]
        dy <- y_pos[nj] - y_pos[ni]
        min_dist <- radius_lookup[ni] + radius_lookup[nj] + min_gap

        if (sqrt(dx^2 + dy^2) < min_dist) {
          # x separation needed given the fixed dy
          needed_dx <- sqrt(max(min_dist^2 - dy^2, 0))
          current_dx <- abs(dx)
          if (current_dx < needed_dx) {
            push <- (needed_dx - current_dx) / 2 + 1e-6
            if (dx >= 0) {
              x_pos[ni] <- x_pos[ni] - push
              x_pos[nj] <- x_pos[nj] + push
            } else {
              x_pos[ni] <- x_pos[ni] + push
              x_pos[nj] <- x_pos[nj] - push
            }
            moved <- TRUE
          }
        }
      }
    }
    if (!moved) break
  }
  x_pos
}

# ---- Plotting -----------------------------------------------------------

#' Draw the phylogenetic tree using base graphics
plot_phylo_tree <- function(edge_df, cluster_df, sample_name = "") {
  layout <- calculate_phylo_layout(edge_df, cluster_df)
  if (is.null(layout)) {
    plot.new()
    text(0.5, 0.5, "No tree could be built", cex = 1.2, col = "grey50")
    return(invisible(NULL))
  }

  all_nodes     <- layout$all_nodes
  x_pos         <- layout$x_pos
  y_pos         <- layout$y_pos
  ccf_lookup    <- layout$ccf_lookup
  n_muts_lookup <- layout$n_muts_lookup
  col_lookup    <- layout$col_lookup
  radius_lookup <- layout$radius_lookup

  old_par <- par(mar = c(2, 2, 3, 4), xpd = TRUE)
  on.exit(par(old_par))

  max_r  <- max(radius_lookup)
  x_pad  <- max_r + 0.12
  xlim   <- c(min(x_pos) - x_pad, max(x_pos) + x_pad + 0.15)

  plot(NULL, xlim = xlim, ylim = c(-0.05, 1.1),
       xlab = "", ylab = "CCF", axes = FALSE,
       main = if (nchar(sample_name) > 0)
         paste("Tumor Phylogenetic Tree —", sample_name)
       else "Tumor Phylogenetic Tree",
       cex.main = 1.5)
  axis(2, at = seq(0, 1, 0.2), las = 1, cex.axis = 1.1)

  for (i in seq_len(nrow(edge_df))) {
    p  <- edge_df$parent[i]
    ch <- edge_df$child[i]
    x0 <- x_pos[p]
    y0 <- y_pos[p]
    x1 <- x_pos[ch]
    y1 <- y_pos[ch]
    segments(x0, y0, x1, y1, col = "grey60", lwd = 1.2, lty = 1)
    text((x0 + x1) / 2 + 0.02, (y0 + y1) / 2,
         labels = round(edge_df$branch_length[i], 2),
         cex = 0.8, col = "grey30", adj = 0)
  }

  for (id in all_nodes) {
    angles <- seq(0, 2 * pi, length.out = 60)
    polygon(x_pos[id] + radius_lookup[id] * cos(angles),
            y_pos[id] + radius_lookup[id] * sin(angles),
            col = col_lookup[id],
            border = if (id == "GL") "grey60" else "black", lwd = 1.5)

    if (id != "GL") {
      text(x_pos[id], y_pos[id], paste0("C", id), cex = 1.0, font = 2)
      text(x_pos[id] + radius_lookup[id] + 0.02, y_pos[id],
           labels = paste0("CCF=", round(ccf_lookup[id], 2)),
           cex = 0.9, col = "grey30", adj = 0)
    } else {
      text(x_pos[id], y_pos[id], "GL", cex = 1.1, font = 2)
    }
  }
}
