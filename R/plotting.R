library(ggplot2)
library(plotly)
library(ggpubr)

fix_plotly_legend <- function(p) {
  tryCatch({
    if (!is.null(p$x$data)) {
      for (i in seq_along(p$x$data)) {
        if (!is.null(p$x$data[[i]]$name)) {
          if (grepl("Clonal", p$x$data[[i]]$name, ignore.case = TRUE) &&
              !grepl("Subclonal", p$x$data[[i]]$name, ignore.case = TRUE)) {
            p$x$data[[i]]$name <- "Clonal"
          } else if (grepl("Subclonal", p$x$data[[i]]$name, ignore.case = TRUE)) {
            p$x$data[[i]]$name <- "Subclonal"
          }
        }
      }
      legend_names    <- sapply(p$x$data, function(x) x$name)
      first_clonal    <- which(legend_names == "Clonal")[1]
      first_subclonal <- which(legend_names == "Subclonal")[1]
      for (i in seq_along(p$x$data)) {
        if (!is.null(p$x$data[[i]]$showlegend) && p$x$data[[i]]$showlegend) {
          if (p$x$data[[i]]$name == "Clonal" &&
              !is.na(first_clonal) && i != first_clonal) {
            p$x$data[[i]]$showlegend <- FALSE
          } else if (p$x$data[[i]]$name == "Subclonal" &&
                     !is.na(first_subclonal) && i != first_subclonal) {
            p$x$data[[i]]$showlegend <- FALSE
          }
        }
      }
    }
  }, error = function(e) {
    warning("Error fixing legend: ", e$message)
  })
  p
}

#' Create annotations for plots based on subclonality counts
#' @param subclonality_counts Data frame containing counts of clonal/subclonal mutations
#' @param x Horizontal position of the annotations
#' @return List of ggplot2 annotation objects
create_annotations <- function(subclonality_counts, x = 0.72) {
  annotations <- list()

  if ("Clonal" %in% subclonality_counts$Subclonality) {
    n_clonal <- subclonality_counts$n[subclonality_counts$Subclonality == "Clonal"]
    if (length(n_clonal) > 0 && n_clonal > 0) {
      annotations <- c(
        annotations,
        annotate("text",
                 x = x,
                 y = -1.65,
                 label = paste0("n clonal = ", n_clonal),
                 size = 5.5,
                 color = "black",
                 vjust = -1.5)
      )
    }
  }

  if ("Subclonal" %in% subclonality_counts$Subclonality) {
    n_subclonal <- subclonality_counts$n[subclonality_counts$Subclonality == "Subclonal"]
    if (length(n_subclonal) > 0 && n_subclonal > 0) {
      annotations <- c(
        annotations,
        annotate("text",
                 x = x,
                 y = -2.65,
                 label = paste0("n subclonal = ", n_subclonal),
                 size = 5.5,
                 color = "black",
                 vjust = -1.5)
      )
    }
  }

  return(annotations)
}

#' Colors for the detailed (per-cluster) view
#' Cluster 0 (clonal) keeps the clonal teal and the first subclone keeps the
#' subclonal salmon, so the detailed view reads as a refinement of the default.
#' @param n Number of clusters
#' @return Character vector of n colors
cluster_palette <- function(n) {
  base <- c("#5cd4d6", "#FFA07A", "#7c3aed", "#2563eb", "#e11d48",
            "#16a34a", "#ca8a04", "#db2777", "#0891b2", "#65a30d")
  if (n <= length(base)) base[seq_len(n)] else grDevices::hcl.colors(n, "Dark 3")
}

#' Normalize clonality labels and add a per-cluster label for the detailed view
#' @param merged_df Processed mutation data
#' @return merged_df with factor Subclonality and Cluster columns
prepare_color_columns <- function(merged_df) {
  if (is.factor(merged_df$Subclonality)) {
    levels(merged_df$Subclonality) <- c("Clonal", "Subclonal")
  } else {
    merged_df$Subclonality <- factor(
      ifelse(
        merged_df$Subclonality == "Clonal" | grepl("Clonal", merged_df$Subclonality),
        "Clonal", "Subclonal"
      ),
      levels = c("Clonal", "Subclonal")
    )
  }

  ids <- sort(unique(merged_df$cluster_index))
  counts <- table(merged_df$cluster_index)[as.character(ids)]
  labels <- sprintf("Cluster %s%s (n=%d)", ids, ifelse(ids == 0, ", clonal", ""), as.integer(counts))
  merged_df$Cluster <- factor(labels[match(merged_df$cluster_index, ids)], levels = labels)
  merged_df
}

#' Point color aesthetic and scales for the chosen coloring
#' @param merged_df Output of prepare_color_columns()
#' @param color_by "clonality" or "readDepth"
#' @param detailed Color by cluster instead of clonal/subclonal (clonality only)
#' @return List with aesthetic column name, scale layers, and whether the
#'   clonal/subclonal legend cleanup applies
point_color_layers <- function(merged_df, color_by, detailed) {
  if (color_by != "clonality") {
    return(list(aesthetic = "ReadDepth", scales = list(), clonality_legend = FALSE))
  }

  if (detailed) {
    levs <- levels(merged_df$Cluster)
    colors <- setNames(cluster_palette(length(levs)), levs)
    return(list(aesthetic = "Cluster",
                scales = list(scale_color_manual(values = colors, name = "Cluster")),
                clonality_legend = FALSE))
  }

  clonal_color <- "#5cd4d6"
  subclonal_color <- "#FFA07A"
  list(
    aesthetic = "Subclonality",
    scales = list(
      scale_color_manual(values = c(clonal_color, subclonal_color), labels = c("Clonal", "Subclonal")),
      scale_fill_manual(values = c(clonal_color, subclonal_color), labels = c("Clonal", "Subclonal"))
    ),
    clonality_legend = TRUE
  )
}

#' Convert to plotly and apply shared layout fixes
#' @param plot ggplot object
#' @param clonality_legend Whether to merge duplicate Clonal/Subclonal legend entries
#' @return plotly object
finish_plotly <- function(plot, clonality_legend) {
  p <- plotly::ggplotly(plot, tooltip = c("x", "text"))

  if (clonality_legend) p <- fix_plotly_legend(p)

  p <- plotly::layout(p, hovermode = "closest")
  # Pin the clonal/subclonal legend to the right white space. theme_pubr() forces it
  # to the top, which newer plotly (>=4.x) renders overlapping the title. layout(legend=)
  # doesn't reliably override ggplotly's generated legend, so overwrite it directly,
  # and reserve right margin so it isn't clipped.
  p$x$layout$legend <- modifyList(
    if (is.null(p$x$layout$legend)) list() else p$x$layout$legend,
    list(orientation = "v", x = 1.02, xanchor = "left", y = 1, yanchor = "top"))
  p$x$layout$margin$r <- if (clonality_legend) 130 else 200

  p
}

#' Create VAF (Variant Allele Frequency) plot
#' @param merged_df Processed mutation data
#' @param sample_name Name of the sample
#' @param purity Purity value
#' @param read_depth List containing mean, min, and max read depth
#' @param smoothing_factor Smoothing factor for density plot
#' @param color_by Whether to color by "clonality" or "readDepth"
#' @param subclonality_counts Data frame with mutation counts
#' @param detailed Color points by CliPP cluster instead of clonal/subclonal
#' @return plotly object
create_vaf_plot <- function(merged_df, sample_name, purity, read_depth,
                            smoothing_factor, color_by = "clonality",
                            subclonality_counts, detailed = FALSE) {

  merged_df <- prepare_color_columns(merged_df)
  colors <- point_color_layers(merged_df, color_by, detailed)

  plot <- suppressWarnings(
    ggplot(merged_df, aes(x = AF)) +
      geom_density(fill = "gray", alpha = .6, adjust = smoothing_factor) +
      suppressWarnings(
        geom_point(shape = 142, size = 5, alpha = 0.3,
                   aes(y = -0.75, color = !!sym(colors$aesthetic),
                       text = paste0("chr", chromosome_index, ":", position)))
      ) +
      xlim(0, 1) +
      ggtitle(paste0(
        sample_name, "\n",
        "Purity ", round(purity, 3),
        "  ·  mean depth ", round(read_depth$mean)
      )) +
      create_annotations(subclonality_counts) +
      ggpubr::theme_pubr(base_size = 16) +
      xlab("VAF") +
      ylab("Density") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            plot.title = element_text(size = 14)) +
      colors$scales
  )

  finish_plotly(plot, colors$clonality_legend)
}

#' Create CP (Cellular Prevalence) or CCF (Cancer Cell Fraction) plot
#' @param merged_df Processed mutation data
#' @param sample_name Name of the sample
#' @param smoothing_factor Smoothing factor for density plot
#' @param color_by Whether to color by "clonality" or "readDepth"
#' @param subclonality_counts Data frame with mutation counts
#' @param df_subc Subclonal structure data
#' @param detailed Color points by CliPP cluster instead of clonal/subclonal
#' @param scale "CP" or "CCF" (CCF = CP / purity)
#' @param purity Purity value; sets the CCF axis so it shows the same mutations as CP
#' @return plotly object
create_cp_plot <- function(merged_df, sample_name, smoothing_factor,
                           color_by = "clonality",
                           subclonality_counts, df_subc,
                           detailed = FALSE, scale = "CP", purity = NULL) {

  merged_df <- prepare_color_columns(merged_df)
  colors <- point_color_layers(merged_df, color_by, detailed)

  cp_max <- 1.5
  if (identical(scale, "CCF")) {
    value_col <- "CCF_unpenalized"
    x_max <- if (!is.null(purity) && is.finite(purity) && purity > 0) cp_max / purity else 2
    axis_label <- "Estimated mutation-specific CCF"
  } else {
    value_col <- "CP_unpenalized"
    x_max <- cp_max
    axis_label <- "Estimated mutation-specific CP"
  }

  plot <- suppressWarnings(
    ggplot(merged_df, aes(x = !!sym(value_col))) +
      geom_density(fill = "#047857", alpha = .6, adjust = smoothing_factor) +
      suppressWarnings(
        geom_point(shape = 142, size = 5, alpha = 0.3,
                   aes(y = -0.75, color = !!sym(colors$aesthetic),
                       text = paste0("chr", chromosome_index, ":", position)))
      ) +
      xlim(0, x_max) +
      ggtitle(paste(sample_name, " -", axis_label)) +
      create_annotations(subclonality_counts, x = 0.72 * x_max) +
      ggpubr::theme_pubr(base_size = 16) +
      xlab(axis_label) +
      ylab("Density") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            plot.title = element_text(size = 14)) +
      colors$scales
      # Penalized cluster centroids (dashed vertical lines from df_subc).
      # Commented out per design decision; re-enable to show CliPP's penalized
      # cluster positions alongside the unpenalized density.
      # +
      # geom_segment(
      #   data = df_subc,
      #   aes(x = cellular_prevalence,
      #       xend = cellular_prevalence,
      #       y = 0,
      #       yend = 5.7),
      #   lty = "dashed"
      # )
  )

  finish_plotly(plot, colors$clonality_legend)
}

detailed_view_caption <- "    - <b>Detailed view</b>: points are colored by CliPP cluster; cluster 0 is the clonal cluster.<br>\n"

#' Get VAF plot caption
#' @param detailed Whether the detailed (per-cluster) view is on
#' @return HTML formatted caption for VAF plot
get_vaf_plot_caption <- function(detailed = FALSE) {
  HTML(paste0("<p>
    - <b>Variant Allele Frequency (VAF)</b> indicates the proportion of sequencing reads that support a variant allele.<br>
    - <b>Purity</b> reflects the proportion of tumor cells in the sample. For instance, a purity of 0.9 means 90% of the cells are cancerous.<br>
    - <b>Mean read depth</b> is the average number of sequencing reads per SNV, influencing data reliability.<br>
    - <b>Density</b> estimates the probability density function of VAFs based on kernel density estimation (KDE). <br>
", if (detailed) detailed_view_caption, "  </p>"))
}

#' Get CP/CCF plot caption
#' @param scale "CP" or "CCF"
#' @param detailed Whether the detailed (per-cluster) view is on
#' @return HTML formatted caption for the CP/CCF plot
get_cp_plot_caption <- function(scale = "CP", detailed = FALSE) {
  measure <- if (identical(scale, "CCF")) {
    "    - <b>Cancer Cell Fraction (CCF)</b> the fraction of tumor cells carrying a particular SNV (cellular prevalence divided by purity).<br>\n"
  } else {
    "    - <b>Cellular Prevalence (CP)</b> the fraction of all cells (both tumor and admixed normal cells) from the sequenced tissue carrying a particular SNV.<br>\n"
  }
  HTML(paste0("<p>
", measure,
"    - Annotations on the x-axis indicate whether mutations are <b>clonal</b> (present in all tumor cells) or <b>subclonal</b> (found in a subset).<br>
    - Counts of clonal and subclonal mutations are indicated as <b>n clonal</b> and <b>n subclonal</b> respectively.<br>
", if (detailed) detailed_view_caption, "  </p>"))
}
