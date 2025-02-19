# Inside observeEvent(input$cancer_type_driver, {
  # ... existing code ...

  if (length(matching_file1) > 0 && length(matching_file2) > 0) {
    df_assigned <- read.table(matching_file1[1], header = TRUE, sep = "\t")
    df_subc <- read.table(matching_file2[1], header = TRUE, sep = "\t")

    # Process mutations as before
    df_snvcna <- df_snv %>%
      mutate(total_cn = map2_chr(position, chromosome_index, function(x, y) {
        inds = x >= df_cna$start_position & x <= df_cna$end_position & y == df_cna$chromosome_index
        if (any(inds)) as.character(df_cna$total_cn[which.max(inds)]) else NA
      }))

    # Calculate metrics
    df_snvcna$sum_counts <- df_snvcna$ref_count + df_snvcna$alt_count
    mean_rd <- mean(df_snvcna$sum_counts)
    min_rd <- min(df_snvcna$sum_counts)
    max_rd <- max(df_snvcna$sum_counts)

    merged_df <- merge(df_snvcna, df_assigned, by = c("chromosome_index", "position"))
    merged_df['AF'] = merged_df['alt_count'] / (merged_df['sum_counts'])

    # Process mutations
    merged_df$total_cn <- as.numeric(merged_df$total_cn)
    merged_df$b_i_V <- pmax(1, round(merged_df$AF * (1 / purity) * ((purity * merged_df$total_cn) + (2 * (1 - purity))), 0))
    merged_df$CP_unpenalized <- (merged_df$alt_count * ((1 - purity) * 2 + purity * merged_df$total_cn)) / (merged_df$b_i_V * (merged_df$alt_count + merged_df$ref_count))
    merged_df$CCF_unpenalized <- merged_df$CP_unpenalized / purity

    # Create processed mutations dataframe
    processed_mutations <- merged_df %>%
      mutate(
        Subclonality = ifelse(cluster_index == 0, "Clonal", "Subclonal"),
        Subclonality = factor(Subclonality, levels = c("Clonal", "Subclonal")),
        ReadDepth = sum_counts,
        IsDriver = FALSE  # Mark as non-driver mutations
      )

    # Get driver mutations and add flag
    driver_mutations <- driver_mutation_data %>%
      filter(cancer == selected_cancer_type) %>%
      mutate(IsDriver = TRUE)  # Mark as driver mutations

    # Combine all mutations
    all_mutations <- processed_mutations %>%
      bind_rows(driver_mutations)

    # Calculate counts
    subclonality_counts <- processed_mutations %>%
      count(Subclonality) %>%
      rename(n = n)

    # Update Plot 1 (VAF Plot)
    output$driverPlot1 <- renderPlotly({
      gg1 <- ggplot(all_mutations, aes(x = AF)) +
        geom_density(fill = "gray", alpha = .6, adjust = input$smoothingFactor1_driver) +
        # Background mutations
        geom_point(
          shape = 142,
          size = 5,
          alpha = 0.3,
          aes(
            y = -0.75,
            color = Subclonality
          )
        ) +
        # Driver mutation points
        geom_point(
          data = driver_mutations,
          aes(x = VAF, y = -0.75, fill = Subclonality),
          shape = 21,
          size = 8,
          color = "black",
          alpha = 0.7
        ) +
        # Driver mutation labels
        geom_text(
          data = driver_mutations,
          aes(x = VAF, y = -1.25, label = gene),
          size = 3,
          angle = 45,
          hjust = 0
        ) +
        xlim(0, 1) +
        ggtitle(paste(
          selected_cancer_type,
          " - Mean read depth =", round(mean_rd),
          "      ", "Range read depth =", min_rd, "-", max_rd
        )) +
        ggpubr::theme_pubr() +
        xlab("VAF") + ylab("Density") +
        scale_color_manual(values = c("Clonal" = "#87CEEB", "Subclonal" = "#FFA07A")) +
        scale_fill_manual(values = c("Clonal" = "#87CEEB", "Subclonal" = "#FFA07A")) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

      ggplotly(gg1)
    })

    # Update Plot 2 (CP Plot)
    output$driverPlot2 <- renderPlotly({
      gg2 <- ggplot(all_mutations, aes(x = CP_unpenalized)) +
        geom_density(fill = "seagreen", alpha = .6, adjust = input$smoothingFactor2_driver) +
        # Background mutations
        geom_point(
          shape = 142,
          size = 5,
          alpha = 0.3,
          aes(
            y = -0.75,
            color = Subclonality
          )
        ) +
        # Driver mutation points
        geom_point(
          data = driver_mutations,
          aes(x = CP_unpenalized, y = -0.75, fill = Subclonality),
          shape = 21,
          size = 8,
          color = "black",
          alpha = 0.7
        ) +
        # Driver mutation labels
        geom_text(
          data = driver_mutations,
          aes(x = CP_unpenalized, y = -1.25, label = gene),
          size = 3,
          angle = 45,
          hjust = 0
        ) +
        xlim(0, 1.5) +
        ggtitle(paste("CP Distribution -", selected_cancer_type)) +
        ggpubr::theme_pubr() +
        xlab("CP") + ylab("Density") +
        scale_color_manual(values = c("Clonal" = "#87CEEB", "Subclonal" = "#FFA07A")) +
        scale_fill_manual(values = c("Clonal" = "#87CEEB", "Subclonal" = "#FFA07A")) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
        geom_segment(
          data = df_subc,
          aes(x = cellular_prevalence, xend = cellular_prevalence, y = 0, yend = 5.7),
          lty = "dashed"
        )

      ggplotly(gg2)
    })
  }
}
