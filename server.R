cohort_cancer_download <- function(cohort, cancer, sample_df, file) {
  tryCatch({
    samples <- sample_df$samplename[sample_df$cancer_type == cancer]
    rows <- lapply(samples, function(s) {
      best_dir <- file.path(
        global_TCGA_PCAWG, cohort, s, "final_result", "Best_lambda"
      )
      mut_file  <- list.files(
        best_dir, pattern = "^mutation_assignments", full.names = TRUE
      )
      subc_file <- list.files(
        best_dir, pattern = "^subclonal_structure", full.names = TRUE
      )
      if (length(mut_file) == 0) return(NULL)
      mut_df <- data.table::fread(mut_file[1])
      if (length(subc_file) > 0) {
        subc_df <- data.table::fread(subc_file[1])
        mut_df  <- merge(
          mut_df,
          subc_df[, c("cluster_index", "cellular_prevalence", "num_SNV")],
          by = "cluster_index", all.x = TRUE
        )
      }
      mut_df$sample <- s
      mut_df
    })
    combined  <- data.table::rbindlist(
      Filter(Negate(is.null), rows), fill = TRUE
    )
    col_order <- intersect(
      c("sample", "chromosome_index", "position",
        "cluster_index", "cellular_prevalence", "num_SNV"),
      names(combined)
    )
    data.table::fwrite(combined[, col_order, with = FALSE], file, sep = "\t")
  }, error = function(e) {
    writeLines(paste("Error generating file:", e$message), file)
  })
}

server <- function(input, output, session) {

  observeEvent(input$main_nav_target, {
    updateTabsetPanel(session, "main_nav", selected = input$main_nav_target)
  })

  output$tcgaCancerCounts <- DT::renderDT({
    req(!is.null(dl_tcga_counts))
    DT::datatable(
      dl_tcga_counts,
      rownames  = FALSE,
      options   = list(
        pageLength = 10,
        dom        = "tp",
        order      = list(list(1, "desc"))
      )
    )
  })

  output$pcawgCancerCounts <- DT::renderDT({
    req(!is.null(dl_pcawg_counts))
    DT::datatable(
      dl_pcawg_counts,
      rownames  = FALSE,
      options   = list(
        pageLength = 10,
        dom        = "tp",
        order      = list(list(1, "desc"))
      )
    )
  })

  plots_ready_clipp <- reactiveVal(FALSE)
  plots_ready_TCGA <- reactiveVal(FALSE)
  plots_ready_PCAWG <- reactiveVal(FALSE)

  tab_states <- reactiveValues(
    clipp_web = FALSE,
    tcga = FALSE,
    pcawg = FALSE,
    driver = FALSE
  )

  observe({
    analysis_data <- tcga_analysis()

    if (!is.null(analysis_data) && plots_ready_TCGA()) {
      output$plot1TCGA <- renderPlotly({
        req(plots_ready_TCGA())
        current_analysis <- tcga_analysis()
        req(!is.null(current_analysis))

        tryCatch(
          {
            vaf_plot <- create_vaf_plot(
              merged_df = current_analysis$merged_data,
              sample_name = input$sample_name_TCGA,
              purity = current_analysis$purity,
              read_depth = list(
                mean = current_analysis$read_depth$mean,
                min = min(current_analysis$merged_data$sum_counts, na.rm = TRUE),
                max = max(current_analysis$merged_data$sum_counts, na.rm = TRUE)
              ),
              smoothing_factor = input$smoothingFactor1_TCGA %||% 0.5,
              color_by = input$colorByTCGA %||% "clonality",
              subclonality_counts = current_analysis$subclonality_counts
            )
            return(vaf_plot)
          },
          error = function(e) {
            return(plotly::plot_ly() %>%
              plotly::add_annotations(
                text = paste("Error:", e$message),
                showarrow = FALSE
              ))
          }
        )
      })

      output$plot2TCGA <- renderPlotly({
        req(plots_ready_TCGA())
        current_analysis <- tcga_analysis()
        req(!is.null(current_analysis))

        tryCatch(
          {
            cp_plot <- create_cp_plot(
              merged_df = current_analysis$merged_data,
              sample_name = input$sample_name_TCGA,
              smoothing_factor = input$smoothingFactor2_TCGA %||% 0.5,
              color_by = input$colorByTCGA %||% "clonality",
              subclonality_counts = current_analysis$subclonality_counts,
              df_subc = current_analysis$subclonal_structure
            )
            return(cp_plot)
          },
          error = function(e) {
            return(plotly::plot_ly() %>%
              plotly::add_annotations(
                text = paste("Error:", e$message),
                showarrow = FALSE
              ))
          }
        )
      })

      shinyjs::show("plot1TCGA")
      shinyjs::show("plot2TCGA")
      shinyjs::show("caption1TCGA")
      shinyjs::show("caption2TCGA")
    }
  })

  observe({
    analysis_data <- pcawg_analysis()

    if (!is.null(analysis_data) && plots_ready_PCAWG()) {
      output$plot1PCAWG <- renderPlotly({
        req(plots_ready_PCAWG())
        current_analysis <- pcawg_analysis()
        req(!is.null(current_analysis))

        tryCatch(
          {
            if (is.null(current_analysis$merged_data) || nrow(current_analysis$merged_data) == 0) {
              return(
                plotly::plot_ly() %>%
                  plotly::add_trace(
                    type = "scatter",
                    mode = "markers",
                    x = c(0),
                    y = c(0),
                    opacity = 0
                  ) %>%
                  plotly::add_annotations(
                    text = "No valid data found for this sample",
                    showarrow = FALSE
                  ) %>%
                  plotly::layout(
                    xaxis = list(title = "VAF"),
                    yaxis = list(title = "Density")
                  )
              )
            }

            vaf_plot <- create_vaf_plot(
              merged_df = current_analysis$merged_data,
              sample_name = input$sample_name_PCAWG,
              purity = current_analysis$purity,
              read_depth = list(
                mean = current_analysis$read_depth$mean,
                min = ifelse(!is.null(current_analysis$merged_data$sum_counts) && length(current_analysis$merged_data$sum_counts) > 0,
                  min(current_analysis$merged_data$sum_counts, na.rm = TRUE), 0
                ),
                max = ifelse(!is.null(current_analysis$merged_data$sum_counts) && length(current_analysis$merged_data$sum_counts) > 0,
                  max(current_analysis$merged_data$sum_counts, na.rm = TRUE), 0
                )
              ),
              smoothing_factor = input$smoothingFactor1_PCAWG %||% 0.5,
              color_by = input$colorByPCAWG %||% "clonality",
              subclonality_counts = current_analysis$subclonality_counts
            )
            return(vaf_plot)
          },
          error = function(e) {
            return(
              plotly::plot_ly() %>%
                plotly::add_trace(
                  type = "scatter",
                  mode = "markers",
                  x = c(0),
                  y = c(0),
                  opacity = 0
                ) %>%
                plotly::add_annotations(
                  text = paste("Error:", e$message),
                  showarrow = FALSE
                ) %>%
                plotly::layout(
                  xaxis = list(title = "VAF"),
                  yaxis = list(title = "Density")
                )
            )
          }
        )
      })

      output$plot2PCAWG <- renderPlotly({
        req(plots_ready_PCAWG())
        current_analysis <- pcawg_analysis()
        req(!is.null(current_analysis))

        tryCatch(
          {
            if (is.null(current_analysis$merged_data) || nrow(current_analysis$merged_data) == 0) {
              return(
                plotly::plot_ly() %>%
                  plotly::add_trace(
                    type = "scatter",
                    mode = "markers",
                    x = c(0),
                    y = c(0),
                    opacity = 0
                  ) %>%
                  plotly::add_annotations(
                    text = "No valid data found for this sample",
                    showarrow = FALSE
                  ) %>%
                  plotly::layout(
                    xaxis = list(title = "Cellular Prevalence"),
                    yaxis = list(title = "Density")
                  )
              )
            }

            cp_plot <- create_cp_plot(
              merged_df = current_analysis$merged_data,
              sample_name = input$sample_name_PCAWG,
              smoothing_factor = input$smoothingFactor2_PCAWG %||% 0.5,
              color_by = input$colorByPCAWG %||% "clonality",
              subclonality_counts = current_analysis$subclonality_counts,
              df_subc = current_analysis$subclonal_structure
            )
            return(cp_plot)
          },
          error = function(e) {
            return(
              plotly::plot_ly() %>%
                plotly::add_trace(
                  type = "scatter",
                  mode = "markers",
                  x = c(0),
                  y = c(0),
                  opacity = 0
                ) %>%
                plotly::add_annotations(
                  text = paste("Error:", e$message),
                  showarrow = FALSE
                ) %>%
                plotly::layout(
                  xaxis = list(title = "Cellular Prevalence"),
                  yaxis = list(title = "Density")
                )
            )
          }
        )
      })

      shinyjs::show("plot1PCAWG")
      shinyjs::show("plot2PCAWG")
      shinyjs::show("caption1PCAWG")
      shinyjs::show("caption2PCAWG")
    }
  })

  output$plotsReadyCliPP <- reactive({
    value <- plots_ready_clipp()
    return(value)
  })
  outputOptions(output, "plotsReadyCliPP", suspendWhenHidden = FALSE)

  output$plotsReadyTCGA <- reactive({
    value <- plots_ready_TCGA()
    return(value)
  })
  outputOptions(output, "plotsReadyTCGA", suspendWhenHidden = FALSE)

  output$plotsReadyPCAWG <- reactive({
    value <- plots_ready_PCAWG()
    return(value)
  })
  outputOptions(output, "plotsReadyPCAWG", suspendWhenHidden = FALSE)

  output$samplesUploaded <- reactive({
    samples_uploaded()
  })
  outputOptions(output, "samplesUploaded", suspendWhenHidden = FALSE)

  output$driverRowSelected <- reactive({
    value <- !is.null(input$driverMutationTable_rows_selected)
    return(value)
  })
  outputOptions(output, "driverRowSelected", suspendWhenHidden = FALSE)

  observeEvent(input$driverMutationTable_rows_selected, {
    tab_states$driver <- !is.null(input$driverMutationTable_rows_selected)
  })

  samples_uploaded <- reactiveVal(FALSE)
  uploaded_samples <- reactiveValues(samples = list())

  # ============================================================================
  # PHYLOGENETIC TREE DIRECT LOADING OBSERVERS
  # ============================================================================

  observeEvent(input$phylo_cancer_tcga, {
    req(input$phylo_cancer_tcga)
    filtered_samples <- sample_TCGA$samplename[sample_TCGA$cancer_type == input$phylo_cancer_tcga]
    updateSelectInput(session, "phylo_sample_tcga", choices = sort(filtered_samples))
  })

  observeEvent(input$phylo_cancer_pcawg, {
    req(input$phylo_cancer_pcawg)
    filtered_samples <- sample_PCAWG$samplename[sample_PCAWG$cancer_type == input$phylo_cancer_pcawg]
    updateSelectInput(session, "phylo_sample_pcawg", choices = sort(filtered_samples))
  })

  observe({
    req(uploaded_samples$samples)
    sample_names <- names(uploaded_samples$samples)
    updateSelectInput(session, "phylo_sample_clipp", choices = sample_names)
  })

  `%||%` <- function(x, y) if (is.null(x)) y else x

  tcga_data <- reactive({
    sample_TCGA
  })

  pcawg_data <- reactive({
    sample_PCAWG
  })

  driver_data <- reactive({
    req(input$cancer_type_driver)
    selected_cancer_type <- input$cancer_type_driver

    if (is.null(selected_cancer_type) || selected_cancer_type == "") {
      return(NULL)
    }

    driver_mutations <- driver_mutation_data[driver_mutation_data$cancer == selected_cancer_type, ]

    if (nrow(driver_mutations) == 0) {
      return(NULL)
    }

    return(driver_mutations)
  })

  driver_data_consolidated <- reactive({
    req(input$cancer_type_driver)
    selected_cancer_type <- input$cancer_type_driver

    if (is.null(selected_cancer_type) || selected_cancer_type == "") {
      return(NULL)
    }

    filtered_data <- driver_mutation_data %>%
      filter(cancer == selected_cancer_type)

    unique_samples <- unique(filtered_data$sample)
    result <- data.frame(
      sample = character(),
      genes = character(),
      proteins = character(),
      consequences = character(),
      Subclonality = character(),
      VAF_range = character(),
      CP_range = character(),
      mutation_count = integer(),
      stringsAsFactors = FALSE
    )

    for (sample_name in unique_samples) {
      sample_rows <- filtered_data[filtered_data$sample == sample_name, ]

      unique_genes <- paste(unique(sample_rows$gene), collapse = ", ")
      unique_proteins <- paste(unique(sample_rows$protein), collapse = ", ")
      unique_consequences <- paste(unique(sample_rows$consequence), collapse = ", ")

      unique_subclonality <- unique(sample_rows$Subclonality)
      if (length(unique_subclonality) > 1) {
        subclonality_status <- "Clonal, Subclonal"
      } else {
        subclonality_status <- unique_subclonality[1]
      }

      vaf_values <- sample_rows$VAF
      cp_values <- sample_rows$CP_unpenalized

      vaf_range <- if (all(is.na(vaf_values))) {
        "NA"
      } else {
        paste(sprintf("%.3f", range(vaf_values, na.rm = TRUE)), collapse = "-")
      }

      cp_range <- if (all(is.na(cp_values))) {
        "NA"
      } else {
        paste(sprintf("%.3f", range(cp_values, na.rm = TRUE)), collapse = "-")
      }

      mutation_count <- nrow(sample_rows)

      result <- rbind(result, data.frame(
        sample = sample_name,
        genes = unique_genes,
        proteins = unique_proteins,
        consequences = unique_consequences,
        Subclonality = subclonality_status,
        VAF_range = vaf_range,
        CP_range = cp_range,
        mutation_count = mutation_count,
        stringsAsFactors = FALSE
      ))
    }

    return(result)
  })

  output$cancer_type_ui_driver <- renderUI({
    selectInput("cancer_type_driver", "Select Cancer Type:", choices = unique(driver_mutation_data$cancer))
  })

  output$gene_name_ui_driver <- renderUI({
    selectInput("gene_name_driver", "Select Gene:", choices = NULL)
  })

  output$driverMutationTable <- DT::renderDataTable({
    cancer_type <- input$cancer_type_driver
    view_mode <- input$view_mode %||% "individual"

    if (is.null(cancer_type) || cancer_type == "") {
      return(DT::datatable(
        data.frame(message = "Please select a cancer type"),
        options = list(dom = "t")
      ))
    }

    if (view_mode == "consolidated") {
      consolidated_data <- driver_data_consolidated()

      if (is.null(consolidated_data) || nrow(consolidated_data) == 0) {
        return(DT::datatable(
          data.frame(message = "No data available for the selected cancer type"),
          options = list(dom = "t")
        ))
      }

      DT::datatable(
        consolidated_data,
        selection = "single",
        options = list(
          pageLength = 10,
          searchHighlight = TRUE,
          autoWidth = FALSE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Subclonality",
          backgroundColor = DT::styleEqual(
            c("Clonal", "Subclonal", "Clonal, Subclonal"),
            c("#C9EDEF", "#FED9D7", "#E6E6FA")
          ),
          color = "black"
        )
    } else {
      driver_mutations <- driver_data()

      if (is.null(driver_mutations) || nrow(driver_mutations) == 0) {
        return(DT::datatable(
          data.frame(message = "No data available for the selected cancer type"),
          options = list(dom = "t")
        ))
      }

      display_columns <- c("gene", "sample", "protein", "consequence", "Subclonality", "VAF", "CP_unpenalized")
      display_data <- driver_mutations[, display_columns]

      DT::datatable(
        display_data,
        selection = "single",
        options = list(
          pageLength = 10,
          searchHighlight = TRUE,
          autoWidth = FALSE
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(c("VAF", "CP_unpenalized"), digits = 3) %>%
        DT::formatStyle(
          "Subclonality",
          backgroundColor = DT::styleEqual(
            c("Clonal", "Subclonal"),
            c("#C9EDEF", "#FED9D7")
          ),
          color = "black"
        )
    }
  })

  process_driver_sample <- function(selected_sample, mutations_data, individual_mutation = NULL) {
    tryCatch(
      {
        if (is.null(selected_sample) || length(selected_sample) == 0 || selected_sample == "") {
          showNotification("Invalid sample name", type = "error")
          return(FALSE)
        }

        if (is.null(mutations_data) || nrow(mutations_data) == 0) {
          showNotification("No mutations provided for analysis", type = "error")
          return(FALSE)
        }

        if (any(selected_sample %in% sample_TCGA$samplename)) {
          prefix <- "TCGA"
        } else if (any(selected_sample %in% sample_PCAWG$samplename)) {
          prefix <- "PCAWG"
        } else {
          showNotification(paste("Sample", selected_sample, "not found in TCGA or PCAWG datasets."), type = "error")
          return(FALSE)
        }

        sampleDir <- file.path(global_TCGA_PCAWG, prefix, selected_sample)
        wwwDir <- file.path(global_www, prefix, selected_sample)
        snvFile <- list.files(wwwDir, pattern = "\\.snv\\.txt$", full.names = TRUE)
        cnaFile <- list.files(wwwDir, pattern = "\\.cna\\.txt$", full.names = TRUE)
        purityFile <- list.files(wwwDir, pattern = "\\.purity\\.txt$", full.names = TRUE)

        if (length(snvFile) == 0 || length(cnaFile) == 0 || length(purityFile) == 0) {
          file_data <- read_from_preprocess(sampleDir)
          if (is.null(file_data)) {
            purity <- NA
            df_snv <- NULL
            df_cna <- NULL
          } else {
            df_snv <- file_data$df_snv
            df_cna <- file_data$df_cna
            purity <- file_data$purity
          }
        } else {
          file_data <- read_and_validate_files(snvFile[1], cnaFile[1], purityFile[1])
          if (is.null(file_data)) {
            showNotification("Error reading sample files.", type = "error")
            return(FALSE)
          }
          df_snv <- file_data$df_snv
          df_cna <- file_data$df_cna
          purity <- file_data$purity
        }

        best_lambda_dir <- file.path(sampleDir, "final_result", "Best_lambda")
        matching_file1 <- list.files(best_lambda_dir, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
        matching_file2 <- list.files(best_lambda_dir, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)

        if (length(matching_file1) == 0 || length(matching_file2) == 0 || is.null(df_snv)) {
          plot_data <- if (!is.null(individual_mutation)) {
            individual_mutation
          } else {
            mutations_data
          }

          output$driverPlot1 <- renderPlotly({
            purity_label <- if (!is.na(purity)) paste0(" - Purity = ", round(purity, 3)) else ""
            gg1 <- ggplot() +
              xlim(0, 1) +
              ggtitle(paste0(selected_sample, purity_label, " | Driver Mutations Only (No Background)")) +
              ggpubr::theme_pubr() +
              xlab("VAF") +
              ylab("Density") +
              theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

            if (nrow(plot_data) > 0) {
              for (i in 1:nrow(plot_data)) {
                mutation <- plot_data[i, ]
                gg1 <- gg1 +
                  geom_vline(
                    xintercept = round(mutation$VAF, 3),
                    linetype = "dashed",
                    color = ifelse(mutation$Subclonality == "Clonal", "#047857", "#FFA07A")
                  )
              }
            }

            p1 <- ggplotly(gg1)

            if (nrow(plot_data) > 0) {
              for (i in 1:nrow(plot_data)) {
                mutation <- plot_data[i, ]
                p1 <- plotly::add_annotations(
                  p1,
                  x = round(mutation$VAF, 3),
                  y = 1,
                  xref = "x",
                  yref = "paper",
                  text = paste0(mutation$gene, " (", mutation$protein, ")"),
                  showarrow = FALSE,
                  xanchor = "left",
                  yanchor = "top",
                  font = list(
                    size = 10,
                    color = ifelse(mutation$Subclonality == "Clonal", "#047857", "#FFA07A")
                  )
                )
              }
            }

            p1
          })

          output$driverPlot2 <- renderPlotly({
            gg2 <- ggplot() +
              xlim(0, 1.5) +
              ggtitle(paste0("CP Distribution - ", selected_sample, " (Driver Mutations Only)")) +
              ggpubr::theme_pubr() +
              xlab("Cellular Prevalence (CP)") +
              ylab("Density") +
              theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

            if (nrow(plot_data) > 0) {
              for (i in 1:nrow(plot_data)) {
                mutation <- plot_data[i, ]
                if (!is.na(mutation$CP_unpenalized) && !is.null(mutation$CP_unpenalized)) {
                  gg2 <- gg2 +
                    geom_vline(
                      xintercept = round(mutation$CP_unpenalized, 3),
                      linetype = "dashed",
                      color = ifelse(mutation$Subclonality == "Clonal", "#047857", "#FFA07A")
                    )
                }
              }
            }

            p2 <- ggplotly(gg2)

            if (nrow(plot_data) > 0) {
              for (i in 1:nrow(plot_data)) {
                mutation <- plot_data[i, ]
                if (!is.na(mutation$CP_unpenalized) && !is.null(mutation$CP_unpenalized)) {
                  p2 <- plotly::add_annotations(
                    p2,
                    x = round(mutation$CP_unpenalized, 3),
                    y = 1,
                    xref = "x",
                    yref = "paper",
                    text = paste0(mutation$gene, " (", mutation$protein, ")"),
                    showarrow = FALSE,
                    xanchor = "left",
                    yanchor = "top",
                    font = list(
                      size = 10,
                      color = ifelse(mutation$Subclonality == "Clonal", "#047857", "#FFA07A")
                    )
                  )
                }
              }
            }

            p2
          })

          output$driverCaption1 <- renderUI({
            HTML("<p>
          - <b>Variant Allele Frequency (VAF)</b> indicates the proportion of sequencing reads that support a variant allele.<br>
          - <b>Colored vertical lines</b> show the VAF of each driver mutation in this sample.<br>
          - Note: Full mutation distribution not available for this sample.
        </p>")
          })

          output$driverCaption2 <- renderUI({
            HTML("<p>
          - <b>Cellular Prevalence (CP)</b> is the fraction of all cells from the sample carrying a particular SNV.<br>
          - <b>Colored vertical lines</b> show the CP of each driver mutation in this sample.<br>
          - Note: Full mutation distribution not available for this sample.
        </p>")
          })

          return(TRUE)
        }

        df_assigned <- read.table(matching_file1[1], header = TRUE, sep = "\t")
        df_subc <- read.table(matching_file2[1], header = TRUE, sep = "\t")

        df_snv$chromosome_index     <- as.character(df_snv$chromosome_index)
        df_snv$position              <- as.integer(df_snv$position)
        df_cna$chromosome_index      <- as.character(df_cna$chromosome_index)
        df_assigned$chromosome_index <- as.character(df_assigned$chromosome_index)
        df_assigned$position         <- as.integer(df_assigned$position)

        df_snvcna <- df_snv %>%
          mutate(total_cn = map2_chr(position, chromosome_index, function(x, y) {
            inds <- x >= df_cna$start_position & x <= df_cna$end_position & y == df_cna$chromosome_index
            if (any(inds)) as.character(df_cna$total_cn[which.max(inds)]) else NA
          }))

        df_snvcna$sum_counts <- df_snvcna$ref_count + df_snvcna$alt_count
        mean_rd <- mean(df_snvcna$sum_counts)
        min_rd <- min(df_snvcna$sum_counts)
        max_rd <- max(df_snvcna$sum_counts)

        merged_df <- merge(df_snvcna, df_assigned, by = c("chromosome_index", "position"))
        merged_df["AF"] <- merged_df["alt_count"] / (merged_df["sum_counts"])

        merged_df$total_cn <- as.numeric(merged_df$total_cn)
        merged_df$b_i_V <- pmax(1, round(merged_df$AF * (1 / purity) * ((purity * merged_df$total_cn) + (2 * (1 - purity))), 0))
        merged_df$CP_unpenalized <- (merged_df$alt_count * ((1 - purity) * 2 + purity * merged_df$total_cn)) /
          (merged_df$b_i_V * (merged_df$alt_count + merged_df$ref_count))
        merged_df$CCF_unpenalized <- merged_df$CP_unpenalized / purity

        merged_df <- merged_df %>%
          mutate(
            Subclonality = ifelse(cluster_index == 0, "Clonal", "Subclonal"),
            ReadDepth = sum_counts
          )

        subclonality_counts <- merged_df %>%
          count(Subclonality) %>%
          rename(n = n)

        total_SNVs <- sum(df_subc$num_SNV)
        recommended_smooth <- recommended_smoothing_factor(total_SNVs)

        output$smoothingExplanation1_driver <- renderUI({
          HTML(paste0("Recommended smoothing factor: ", recommended_smooth))
        })

        output$smoothingExplanation2_driver <- renderUI({
          HTML(paste0("Recommended smoothing factor: ", recommended_smooth))
        })

        highlight_mutations <- if (!is.null(individual_mutation)) {
          individual_mutation
        } else {
          mutations_data
        }

        output$driverPlot1 <- renderPlotly({
          create_vaf_plot(
            merged_df = merged_df,
            sample_name = selected_sample,
            purity = purity,
            read_depth = list(mean = mean_rd, min = min_rd, max = max_rd),
            smoothing_factor = input$smoothingFactor1_driver %||% 0.45,
            color_by = "clonality",
            subclonality_counts = subclonality_counts,
            driver_mutations = highlight_mutations
          )
        })

        output$driverPlot2 <- renderPlotly({
          create_cp_plot(
            merged_df = merged_df,
            sample_name = selected_sample,
            smoothing_factor = input$smoothingFactor2_driver %||% 0.45,
            color_by = "clonality",
            subclonality_counts = subclonality_counts,
            df_subc = df_subc,
            driver_mutations = highlight_mutations
          )
        })

        output$driverCaption1 <- renderUI({ get_vaf_plot_caption() })
        output$driverCaption2 <- renderUI({ get_cp_plot_caption() })

        return(TRUE)
      },
      error = function(e) {
        showNotification(paste("Error processing sample:", e$message), type = "error")

        output$driverPlot1 <- renderPlotly({
          plotly::plot_ly(type = "scatter", mode = "markers") %>%
            plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
            plotly::add_annotations(
              text = paste("Error processing sample:", e$message),
              showarrow = FALSE
            )
        })

        output$driverPlot2 <- renderPlotly({
          plotly::plot_ly(type = "scatter", mode = "markers") %>%
            plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
            plotly::add_annotations(
              text = paste("Error processing sample:", e$message),
              showarrow = FALSE
            )
        })
      }
    )
  }

  selected_driver_row <- reactiveVal(NULL)

  observeEvent(input$driverMutationTable_rows_selected, {
    row_selected <- input$driverMutationTable_rows_selected

    if (is.null(row_selected)) {
      shinyjs::hide("driverPlotSection")
      output$driverPlot1 <- renderPlotly({ NULL })
      output$driverPlot2 <- renderPlotly({ NULL })
      return()
    }

    shinyjs::show("driverPlotSection")
    shinyjs::show("smoothingFactor1_driver")
    shinyjs::show("smoothingFactor2_driver")
    shinyjs::show("smoothingExplanation1_driver")
    shinyjs::show("smoothingExplanation2_driver")

    selected_cancer_type <- input$cancer_type_driver
    if (is.null(selected_cancer_type) || selected_cancer_type == "") {
      showNotification("Please select a cancer type first", type = "warning")
      return()
    }

    view_mode <- ifelse(is.null(input$view_mode), "individual", input$view_mode)

    tryCatch(
      {
        if (view_mode == "consolidated") {
          consolidated_data <- driver_data_consolidated()

          if (is.null(consolidated_data) || nrow(consolidated_data) == 0 || row_selected > nrow(consolidated_data)) {
            showNotification("Invalid row selection", type = "error")
            return()
          }

          selected_sample <- consolidated_data$sample[row_selected]
          selected_driver_row(data.frame(sample = selected_sample, gene = NA_character_))

          all_sample_mutations <- driver_mutation_data[
            driver_mutation_data$cancer == selected_cancer_type &
              driver_mutation_data$sample == selected_sample,
          ]

          if (nrow(all_sample_mutations) == 0) {
            showNotification("No mutations found for the selected sample", type = "warning")
            return()
          }

          process_driver_sample(selected_sample, all_sample_mutations, individual_mutation = NULL)
        } else {
          individual_data <- driver_data()

          if (is.null(individual_data) || nrow(individual_data) == 0 || row_selected > nrow(individual_data)) {
            showNotification("Invalid row selection", type = "error")
            return()
          }

          selected_mutation <- individual_data[row_selected, ]
          selected_sample <- selected_mutation$sample
          selected_driver_row(selected_mutation)

          all_sample_mutations <- driver_mutation_data[
            driver_mutation_data$cancer == selected_cancer_type &
              driver_mutation_data$sample == selected_sample,
          ]

          if (nrow(all_sample_mutations) == 0) {
            showNotification("No mutations found for the selected sample.", type = "warning")
            return()
          }

          process_driver_sample(selected_sample, all_sample_mutations, individual_mutation = selected_mutation)
        }
      },
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  observeEvent(input$cancer_type_driver, {
    shinyjs::hide("smoothingControls")

    dataTableProxy("driverMutationTable") %>% selectRows(NULL)
    selected_driver_row(NULL)

    shinyjs::hide("driverPlotSection")
    output$driverPlot1 <- renderPlotly({ NULL })
    output$driverPlot2 <- renderPlotly({ NULL })
    output$driverCaption1 <- renderUI({ NULL })
    output$driverCaption2 <- renderUI({ NULL })

    selected_cancer_type <- input$cancer_type_driver
    if (is.null(selected_cancer_type) || selected_cancer_type == "") {
      return()
    }

    driver_mutations <- isolate({
      driver_mutation_data %>%
        filter(cancer == selected_cancer_type)
    })

    if (nrow(driver_mutations) == 0) {
      showNotification("No driver mutations found for this cancer type", type = "warning")
      output$driverMutationTable <- DT::renderDataTable({
        DT::datatable(
          data.frame(message = "No data available for the selected cancer type"),
          options = list(dom = "t")
        )
      })
      return()
    }

    view_mode <- ifelse(is.null(input$view_mode), "individual", input$view_mode)

    output$driverMutationTable <- DT::renderDataTable({
      if (view_mode == "consolidated") {
        samples <- unique(driver_mutations$sample)
        consolidated <- data.frame(
          sample = character(),
          genes = character(),
          proteins = character(),
          Subclonality = character(),
          mutation_count = integer(),
          stringsAsFactors = FALSE
        )

        for (s in samples) {
          sample_muts <- driver_mutations[driver_mutations$sample == s, ]
          has_clonal <- any(sample_muts$Subclonality == "Clonal")
          has_subclonal <- any(sample_muts$Subclonality == "Subclonal")

          subclonality <- if (has_clonal && has_subclonal) {
            "Clonal, Subclonal"
          } else if (has_clonal) {
            "Clonal"
          } else {
            "Subclonal"
          }

          consolidated <- rbind(consolidated, data.frame(
            sample = s,
            genes = paste(unique(sample_muts$gene), collapse = ", "),
            proteins = paste(unique(sample_muts$protein), collapse = ", "),
            Subclonality = subclonality,
            mutation_count = nrow(sample_muts),
            stringsAsFactors = FALSE
          ))
        }

        DT::datatable(
          consolidated,
          selection = "single",
          options = list(
            pageLength = 10,
            searchHighlight = TRUE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(
            "Subclonality",
            backgroundColor = DT::styleEqual(
              c("Clonal", "Subclonal", "Clonal, Subclonal"),
              c("#C9EDEF", "#FED9D7", "#E6E6FA")
            ),
            color = "black"
          )
      } else {
        display_columns <- c("gene", "sample", "protein", "consequence", "Subclonality", "VAF", "CP_unpenalized")
        display_data <- driver_mutations[, display_columns]

        DT::datatable(
          display_data,
          selection = "single",
          options = list(
            pageLength = 10,
            searchHighlight = TRUE
          ),
          rownames = FALSE
        ) %>%
          DT::formatRound(c("VAF", "CP_unpenalized"), digits = 3) %>%
          DT::formatStyle(
            "Subclonality",
            backgroundColor = DT::styleEqual(
              c("Clonal", "Subclonal"),
              c("#C9EDEF", "#FED9D7")
            ),
            color = "black"
          )
      }
    })

    output$driverCaption1 <- renderUI({
      HTML("<p>
      - <b>Variant Allele Frequency (VAF)</b> indicates the proportion of sequencing reads that support a variant allele.<br>
      - <b>Mean read depth</b> is the average number of sequencing reads per SNV, influencing data reliability.<br>
      - <b>Density</b> estimates the probability density function of VAFs based on kernel density estimation (KDE).<br>
    </p>")
    })

    output$driverCaption2 <- renderUI({
      HTML("<p>
      - <b>Cellular Prevalence (CP)</b> the fraction of all cells (both tumor and normal) from the sample carrying a particular SNV.<br>
      - <b>Vertical dashed lines</b> indicate subclonal clusters identified in the tumor sample.<br>
      - <b>Points are colored</b> according to whether mutations are clonal (present in all tumor cells) or subclonal (found in a subset).<br>
    </p>")
    })
  })

  observe({
    selected_data <- driver_data()
    if (!is.null(selected_data)) {
      shinyjs::show("downloadDriverMutation")
      shinyjs::show("downloadDriverMutation_gene")
    } else {
      shinyjs::hide("downloadDriverMutation")
      shinyjs::hide("downloadDriverMutation_gene")
    }
  })

  observe({
    row <- selected_driver_row()
    if (is.null(row)) {
      shinyjs::disable("downloadDriverMutation")
      shinyjs::disable("downloadDriverMutation_gene")
    } else {
      shinyjs::enable("downloadDriverMutation")
      shinyjs::enable("downloadDriverMutation_gene")
    }
  })

  observeEvent(input$gene_name_driver, {
    selected_gene_name <- input$gene_name_driver
    sample_names <- unique(driver_mutation_data$sample[driver_mutation_data$gene == selected_gene_name])
    updateSelectInput(session, "sample_name_driver", choices = sample_names)
  })

  observeEvent(input$submitDriver, {
    selected_sample <- input$sample_name_driver
    selected_cancer_type <- input$cancer_type_driver

    if (is.null(selected_sample) || selected_sample == "") {
      showNotification("Please select a valid sample.", type = "error")
      return()
    }

    selected_data <- driver_mutation_data %>% filter(sample == selected_sample)

    if (nrow(selected_data) == 0) {
      showNotification("No data available for the selected sample.", type = "error")
      return()
    }

    selected_data <- selected_data %>%
      mutate(cancer = selected_cancer_type)

    output$driverMutationTable <- DT::renderDataTable({
      DT::datatable(selected_data %>% select(gene, protein, consequence, cancer, Subclonality, VAF, CP_unpenalized),
        options = list(pageLength = 10)
      ) %>%
        DT::formatRound(c("VAF", "CP_unpenalized"), digits = 3) %>%
        DT::formatStyle(
          "Subclonality",
          backgroundColor = DT::styleEqual(c("Clonal", "Subclonal"), c("#C9EDEF", "#FED9D7")),
          color = DT::styleEqual(c("Clonal", "Subclonal"), c("black", "black"))
        )
    })
    shinyjs::show("downloadDriverMutation")
  })

  driver_output_cols <- function(df) {
    keep <- c(
      tcga_id        = "tcga_id",
      gene           = "Hugo_Symbol",
      chromosome     = "chromosome",
      position       = "position",
      cancer_type    = "cancer_type",
      clonality      = "Subclonality",
      VAF            = "VAF",
      CP_unpenalized = "CP_unpenalized",
      consequence    = "Consequence"
    )
    available <- keep[keep %in% names(df)]
    out <- df[, available, drop = FALSE]
    names(out) <- names(available)
    out
  }

  output$downloadDriverMutation <- downloadHandler(
    filename = function() {
      if (!is.null(input$view_mode) && input$view_mode == "consolidated") {
        paste0("Driver_Mutation_Data-", input$cancer_type_driver,
               "-Consolidated-", Sys.Date(), ".txt")
      } else {
        row <- selected_driver_row()
        sample <- if (!is.null(row)) row$sample else "unknown"
        paste0("Driver_Mutation_Data-", sample, "-", Sys.Date(), ".txt")
      }
    },
    content = function(file) {
      view_mode <- ifelse(!is.null(input$view_mode), input$view_mode, "individual")
      if (view_mode == "consolidated") {
        selected_data <- driver_data_consolidated()
      } else {
        row <- selected_driver_row()
        df <- driver_data()
        selected_data <- if (!is.null(row) && !is.null(df))
          df[df$sample == row$sample, ] else NULL
      }
      if (!is.null(selected_data) && nrow(selected_data) > 0) {
        out <- if (view_mode == "consolidated") selected_data
               else driver_output_cols(selected_data)
        write.table(out, file, sep = "\t", row.names = FALSE, quote = FALSE)
      } else {
        writeLines("No data available", file)
      }
    },
    contentType = "text/plain"
  )

  output$downloadDriverMutation_gene <- downloadHandler(
    filename = function() {
      row <- selected_driver_row()
      gene <- if (!is.null(row) && !is.na(row$gene)) row$gene else "unknown"
      paste0("Driver_Mutation_Data-", gene, "-", Sys.Date(), ".txt")
    },
    content = function(file) {
      row <- selected_driver_row()
      req(!is.null(row), !is.na(row$gene))
      df <- driver_data()
      filtered <- df[df$gene == row$gene, ]
      if (!is.null(filtered) && nrow(filtered) > 0) {
        write.table(
          driver_output_cols(filtered),
          file, sep = "\t", row.names = FALSE, quote = FALSE
        )
      } else {
        writeLines("No data available", file)
      }
    },
    contentType = "text/plain"
  )

  observe({
    if (is.null(input$driverMutationTable_rows_selected)) {
      shinyjs::hide("smoothingFactor1_driver")
      shinyjs::hide("smoothingFactor2_driver")
      shinyjs::hide("smoothingExplanation1_driver")
      shinyjs::hide("smoothingExplanation2_driver")
    }
  })

  output$driverMutationPlots <- renderUI({
    if (!is.null(input$cancer_type_driver)) {
      tagList(
        tabsetPanel(
          tabPanel(
            "Plots",
            uiOutput("smoothingExplanation1_driver"),
            plotlyOutput("driverPlot1"),
            uiOutput("driverCaption1"),
            uiOutput("smoothingExplanation2_driver"),
            plotlyOutput("driverPlot2"),
            uiOutput("driverCaption2")
          ),
          tabPanel(
            "Data Table",
            DT::DTOutput("driverMutationTable"),
            shinyjs::hidden(downloadButton("downloadDriverMutation", "Download by Sample")),
            tags$h4(tags$b(tags$span("Reference:"))),
            p("Martínez-Jiménez, Francisco, et al. 'A compendium of mutational cancer driver genes.' Nature Reviews Cancer 20.10 (2020): 555-572.")
          )
        )
      )
    }
  })

  observeEvent(input$setSamples, {
    numSamples <- input$numSamples
    output$dynamicFileInputs <- renderUI({
      lapply(1:numSamples, function(i) {
        tagList(
          h4(paste("Sample", i)),
          textInput(paste0("sampleName", i), "Sample Name:", value = paste0("Sample", i)),
          if (input$uploadMode == "all_at_once") {
            fileInput(paste0("fileInput", i),
              "Upload all three required files (snv.txt, cna.txt, purity.txt):",
              multiple = TRUE,
              accept = c(".txt")
            )
          } else {
            tagList(
              fileInput(paste0("snvFile", i), "SNV file:", multiple = FALSE, accept = c(".txt")),
              fileInput(paste0("cnaFile", i), "CNA file:", multiple = FALSE, accept = c(".txt")),
              fileInput(paste0("purityFile", i), "Purity file:", multiple = FALSE, accept = c(".txt")),
              br()
            )
          }
        )
      })
    })
    shinyjs::show("uploadSamples")
    shinyjs::hide("setSamples")
    shinyjs::disable("numSamples")
  })

  observeEvent(input$uploadSamples, {
    numSamples <- input$numSamples
    samples <- store_uploaded_files(input, numSamples, input$uploadMode)
    uploaded_samples$samples <- samples
    samples_uploaded(TRUE)

    updateSelectInput(session, "selectedSample",
      choices = names(samples)
    )

    showNotification("Samples uploaded successfully.", type = "message")
    shinyjs::hide("uploadSamples")
    shinyjs::show("selectedSample")
    shinyjs::show("submit")
    shinyjs::show("colorBy")
  })

  observeEvent(input$clear, {
    uploaded_samples$samples <- list()
    samples_uploaded(FALSE)
    updateNumericInput(session, "numSamples", value = 1)
    shinyjs::enable("numSamples")

    output$dynamicFileInputs <- renderUI({
      NULL
    })

    elements_to_hide <- c(
      "selectedSample", "submit", "colorBy", "clear",
      "plot1", "plot2", "caption1", "caption2",
      "smoothingFactor1", "smoothingFactor2",
      "smoothingExplanation1", "smoothingExplanation2",
      "uploadSamples"
    )

    for (element in elements_to_hide) {
      shinyjs::hide(element)
    }

    shinyjs::show("setSamples")

    output$plot1 <- renderPlotly({
      NULL
    })
    output$plot2 <- renderPlotly({
      NULL
    })
    output$caption1 <- renderUI({
      NULL
    })
    output$caption2 <- renderUI({
      NULL
    })
    output$plotOutputArea <- renderUI({
      NULL
    })

    plots_ready_clipp(FALSE)
    updateSelectInput(session, "selectedFile", choices = NULL)

    if (!is.null(sample_TCGA) && nrow(sample_TCGA) > 0) {
      default_cancer_type_tcga <- sample_TCGA$cancer_type[1]
      default_samples_tcga <- sample_TCGA$samplename[sample_TCGA$cancer_type == default_cancer_type_tcga]
      updateSelectInput(session, "cancer_type_TCGA", selected = default_cancer_type_tcga)
      updateSelectInput(session, "sample_name_TCGA", choices = default_samples_tcga, selected = default_samples_tcga[1])
    }

    if (!is.null(sample_PCAWG) && nrow(sample_PCAWG) > 0) {
      default_cancer_type_pcawg <- sample_PCAWG$cancer_type[1]
      default_samples_pcawg <- sample_PCAWG$samplename[sample_PCAWG$cancer_type == default_cancer_type_pcawg]
      updateSelectInput(session, "cancer_type_PCAWG", selected = default_cancer_type_pcawg)
      updateSelectInput(session, "sample_name_PCAWG", choices = default_samples_pcawg, selected = default_samples_pcawg[1])
    }
  })

  observeEvent(input$submit, {
    withProgress(message = "Processing data...", value = 0, {
      plots_ready_clipp(FALSE)
      tab_states$clipp_web <- FALSE

      selectedSample <- input$selectedSample
      sampleFiles <- uploaded_samples$samples[[selectedSample]]

      if (is.null(sampleFiles)) {
        showNotification("Please select a sample to analyze.", type = "error")
        return()
      }

      process_result <- process_sample_files(sampleFiles, selectedSample)

      if (!process_result$success) {
        showNotification(process_result$message, type = "error")
        return()
      }

      shinyjs::show("clear")
      shinyjs::show("plotOutputArea")

      current_analysis <- get_current_analysis(sampleFiles)

      if (!is.null(current_analysis)) {
        output$plot1 <- renderPlotly({
          create_vaf_plot(
            merged_df = current_analysis$merged_data,
            sample_name = selectedSample,
            purity = current_analysis$purity,
            read_depth = current_analysis$read_depth,
            smoothing_factor = input$smoothingFactor1 %||% 0.45,
            color_by = input$colorBy %||% "clonality",
            subclonality_counts = current_analysis$subclonality_counts
          )
        })

        output$plot2 <- renderPlotly({
          create_cp_plot(
            merged_df = current_analysis$merged_data,
            sample_name = selectedSample,
            smoothing_factor = input$smoothingFactor2 %||% 0.45,
            color_by = input$colorBy %||% "clonality",
            subclonality_counts = current_analysis$subclonality_counts,
            df_subc = current_analysis$subclonal_structure
          )
        })

        output$caption1 <- renderUI({
          get_vaf_plot_caption()
        })

        output$caption2 <- renderUI({
          get_cp_plot_caption()
        })

        shinyjs::show("plot1")
        shinyjs::show("plot2")
        shinyjs::show("caption1")
        shinyjs::show("caption2")

        output$smoothingExplanation1 <- renderUI({
          recommended <- recommended_smoothing_factor(current_analysis$total_mutations)
          HTML(paste0("Recommended smoothing factor: ", recommended))
        })

        output$smoothingExplanation2 <- renderUI({
          recommended <- recommended_smoothing_factor(current_analysis$total_mutations)
          HTML(paste0("Recommended smoothing factor: ", recommended))
        })

        shinyjs::show("smoothingFactor1")
        shinyjs::show("smoothingFactor2")
        shinyjs::show("smoothingExplanation1")
        shinyjs::show("smoothingExplanation2")
        shinyjs::show(selector = ".controls-area")
      }

      resultPath <- "forCliPP/CliPP/sample_id/final_result/Best_lambda"
      result_files <- list.files(resultPath, pattern = "\\.txt$")
      updateSelectInput(session, "selectedFile", choices = result_files)

      plots_ready_clipp(TRUE)
      tab_states$clipp_web <- TRUE
    })
  })

  output$plot1 <- renderPlotly({
    req(plots_ready_clipp())

    selected_sample <- input$selectedSample
    current_analysis <- get_current_analysis(uploaded_samples$samples[[selected_sample]])

    if (is.null(current_analysis)) {
      return(NULL)
    }

    create_vaf_plot(
      merged_df = current_analysis$merged_data,
      sample_name = selected_sample,
      purity = current_analysis$purity,
      read_depth = current_analysis$read_depth,
      smoothing_factor = input$smoothingFactor1,
      color_by = input$colorBy,
      subclonality_counts = current_analysis$subclonality_counts
    )
  })

  output$plot2 <- renderPlotly({
    req(plots_ready_clipp())

    selected_sample <- input$selectedSample
    current_analysis <- get_current_analysis(uploaded_samples$samples[[selected_sample]])

    if (is.null(current_analysis)) {
      return(NULL)
    }

    create_cp_plot(
      merged_df = current_analysis$merged_data,
      sample_name = selected_sample,
      smoothing_factor = input$smoothingFactor2,
      color_by = input$colorBy,
      subclonality_counts = current_analysis$subclonality_counts,
      df_subc = current_analysis$subclonal_structure
    )
  })

  output$caption1 <- renderUI({
    req(plots_ready_clipp())
    get_vaf_plot_caption()
  })

  output$caption2 <- renderUI({
    req(plots_ready_clipp())
    get_cp_plot_caption()
  })

  output$plotOutputArea <- renderUI({
    req(plots_ready_clipp())

    resultPath <- "forCliPP/CliPP/sample_id/final_result/Best_lambda"
    result_files <- list.files(resultPath, pattern = "\\.txt$")

    fluidPage(
      h4("Output:"),
      textOutput("result"),
      selectInput("selectedFile", "Choose a file to download:", choices = result_files),
      downloadButton("downloadResult", "Download Result")
    )
  })

  output$cancer_type_ui_TCGA <- renderUI({
    available_cancer_types <- sort(unique(sample_TCGA$cancer_type))

    selectInput("cancer_type_TCGA", "Select Cancer Type:",
      choices = available_cancer_types
    )
  })

  output$sample_name_ui_TCGA <- renderUI({
    req(input$cancer_type_TCGA)
    filtered_samples <- sample_TCGA$samplename[sample_TCGA$cancer_type == input$cancer_type_TCGA]
    selectInput("sample_name_TCGA", "Select Sample Name:",
      choices = sort(filtered_samples)
    )
  })

  output$sample_count_TCGA <- renderUI({
    req(input$cancer_type_TCGA)

    selected_cancer_type <- input$cancer_type_TCGA
    count <- sum(sample_TCGA$cancer_type == selected_cancer_type)

    tags$div(
      class = "sample-count-number",
      style = "font-size: 18px; font-weight: 600; color: #155724; text-align: center;",
      paste("n =", count),
      tags$div(
        class = "sample-count-details",
        style = "font-size: 12px; color: #6c757d; margin-top: 5px;",
        paste("samples in", selected_cancer_type)
      )
    )
  })

  output$cancer_type_ui_PCAWG <- renderUI({
    available_cancer_types <- sort(unique(sample_PCAWG$cancer_type))

    selectInput("cancer_type_PCAWG", "Select Cancer Type:",
      choices = available_cancer_types
    )
  })

  output$sample_name_ui_PCAWG <- renderUI({
    req(input$cancer_type_PCAWG)
    filtered_samples <- sample_PCAWG$samplename[sample_PCAWG$cancer_type == input$cancer_type_PCAWG]
    selectInput("sample_name_PCAWG", "Select Sample Name:",
      choices = sort(filtered_samples)
    )
  })

  output$sample_count_PCAWG <- renderUI({
    req(input$cancer_type_PCAWG)

    selected_cancer_type <- input$cancer_type_PCAWG
    count <- sum(sample_PCAWG$cancer_type == selected_cancer_type)

    tags$div(
      class = "sample-count-number",
      style = "font-size: 18px; font-weight: 600; color: #155724; text-align: center;",
      paste("n =", count),
      tags$div(
        class = "sample-count-details",
        style = "font-size: 12px; color: #6c757d; margin-top: 5px;",
        paste("samples in", selected_cancer_type)
      )
    )
  })


  observeEvent(input$submitTCGA, {
    plots_ready_TCGA(FALSE)
    tab_states$tcga <- FALSE

    req(input$cancer_type_TCGA, input$sample_name_TCGA)

    shinyjs::disable("cancer_type_TCGA")
    shinyjs::disable("sample_name_TCGA")
    shinyjs::hide("submitTCGA")
    shinyjs::disable("submitTCGA")

    sample_dir <- file.path(global_TCGA_PCAWG, "TCGA", input$sample_name_TCGA)
    success <- prepareAndRunCliPP(sample_dir, "TCGA", input$sample_name_TCGA)

    if (!success) {
      showNotification("Failed to process sample", type = "error")
      shinyjs::show("submitTCGA")
      return()
    }

    www_dir <- file.path(global_www, "TCGA", input$sample_name_TCGA)
    snv_file <- list.files(www_dir, pattern = "\\.snv\\.txt$", full.names = TRUE)
    
    if (length(snv_file) > 0) {
      tryCatch({
        snv_count <- count_snvs(snv_file[1])
        if (snv_count > 20000) {
          showNotification("Sample has too many SNVs (>20,000) for real-time visualization. Please use the download option or a smaller sample.", type = "error")
          shinyjs::show("submitTCGA")
          shinyjs::enable("cancer_type_TCGA")
          shinyjs::enable("sample_name_TCGA")
          return()
        }
      }, error = function(e) {
      })
    }

    file_data <- read_tcga_files(sample_dir)

    if (is.null(file_data)) {
      showNotification("Error reading sample files", type = "error")
      shinyjs::show("submitTCGA")
      return()
    }

    analysis_result <- process_tcga_data_internal(file_data, input$sample_name_TCGA, global_TCGA_PCAWG)

    if (is.null(analysis_result)) {
      showNotification("Error processing data", type = "error")
      shinyjs::show("submitTCGA")
      return()
    }

    tcga_analysis(analysis_result)

    shinyjs::show("colorByTCGA")
    shinyjs::show("clearTCGA")
    shinyjs::show("plot1TCGA")
    shinyjs::show("plot2TCGA")
    shinyjs::show("caption1TCGA")
    shinyjs::show("caption2TCGA")

    plots_ready_TCGA(TRUE)
    tab_states$tcga <- TRUE

    shinyjs::show("smoothingFactor1_TCGA")
    shinyjs::show("smoothingFactor2_TCGA")
    shinyjs::show("smoothingExplanation1_TCGA")
    shinyjs::show("smoothingExplanation2_TCGA")
    shinyjs::show("downloadPlot1TCGA")
    shinyjs::show("downloadPlot2TCGA")
    shinyjs::show(selector = ".controls-area")

    output$downloadUI_TCGA <- renderUI({
      best_lambda_dir <- file.path(sample_dir, "final_result", "Best_lambda")
      result_files <- list.files(best_lambda_dir, pattern = "\\.txt$")
      if (length(result_files) > 0) {
        tagList(
          selectInput("selectedFileTCGA", "Choose a file to download:", choices = result_files),
          downloadButton("downloadResultTCGA", "Download Result")
        )
      }
    })

    shinyjs::show("downloadUI_TCGA")
  })

  observeEvent(input$submitPCAWG, {
    plots_ready_PCAWG(FALSE)
    tab_states$pcawg <- FALSE

    req(input$cancer_type_PCAWG, input$sample_name_PCAWG)

    shinyjs::disable("cancer_type_PCAWG")
    shinyjs::disable("sample_name_PCAWG")
    shinyjs::hide("submitPCAWG")
    shinyjs::disable("submitPCAWG")

    sample_dir <- file.path(global_TCGA_PCAWG, "PCAWG", input$sample_name_PCAWG)
    www_dir <- file.path(global_www, "PCAWG", input$sample_name_PCAWG)

    snv_file <- list.files(www_dir, pattern = "\\.snv\\.txt$", full.names = TRUE)
    cna_file <- list.files(www_dir, pattern = "\\.cna\\.txt$", full.names = TRUE)
    purity_file <- list.files(www_dir, pattern = "\\.purity\\.txt$", full.names = TRUE)

    if (length(snv_file) > 0) {
      tryCatch(
        {
          snv_count <- count_snvs(snv_file[1])
          if (snv_count > 20000) {
            showNotification(
              type = "error"
            )
            shinyjs::show("submitPCAWG")
            return()
          }
        },
        error = function(e) {
          shinyjs::show("submitPCAWG")
        }
      )
    }

    success <- prepareAndRunCliPP(sample_dir, "PCAWG", input$sample_name_PCAWG)

    if (!success) {
      shinyjs::show("submitPCAWG")
      return()
    }

    file_data <- read_pcawg_files(sample_dir)

    if (is.null(file_data)) {
      shinyjs::show("submitPCAWG")
      return()
    }

    analysis_result <- process_pcawg_data_internal(file_data, input$sample_name_PCAWG, global_TCGA_PCAWG)

    if (is.null(analysis_result)) {
      shinyjs::show("submitPCAWG")
      return()
    }

    pcawg_analysis(analysis_result)

    shinyjs::show("colorByPCAWG")
    shinyjs::show("clearPCAWG")
    shinyjs::show("plot1PCAWG")
    shinyjs::show("plot2PCAWG")
    shinyjs::show("caption1PCAWG")
    shinyjs::show("caption2PCAWG")

    plots_ready_PCAWG(TRUE)
    tab_states$pcawg <- TRUE
    shinyjs::show("smoothingFactor1_PCAWG")
    shinyjs::show("smoothingFactor2_PCAWG")
    shinyjs::show("smoothingExplanation1_PCAWG")
    shinyjs::show("smoothingExplanation2_PCAWG")
    shinyjs::show("downloadPlot1PCAWG")
    shinyjs::show("downloadPlot2PCAWG")
    shinyjs::show(selector = ".controls-area")

    output$downloadUI_PCAWG <- renderUI({
      best_lambda_dir <- file.path(sample_dir, "final_result", "Best_lambda")
      result_files <- list.files(best_lambda_dir, pattern = "\\.txt$")
      if (length(result_files) > 0) {
        tagList(
          selectInput("selectedFilePCAWG", "Choose a file to download:", choices = result_files),
          downloadButton("downloadResultPCAWG", "Download Result")
        )
      }
    })

    shinyjs::show("downloadUI_PCAWG")
  })

  observeEvent(input$clearTCGA, {
    tcga_analysis(NULL)
    plots_ready_TCGA(FALSE)
    tab_states$tcga <- FALSE

    shinyjs::enable("cancer_type_TCGA")
    shinyjs::enable("sample_name_TCGA")

    if (!is.null(sample_TCGA) && nrow(sample_TCGA) > 0) {
      default_cancer_type <- sample_TCGA$cancer_type[1]
      default_samples <- sample_TCGA$samplename[sample_TCGA$cancer_type == default_cancer_type]
      updateSelectInput(session, "cancer_type_TCGA", selected = default_cancer_type)
      updateSelectInput(session, "sample_name_TCGA", choices = default_samples, selected = default_samples[1])
    }

    elements_to_hide <- c(
      "plot1TCGA", "plot2TCGA",
      "caption1TCGA", "caption2TCGA",
      "smoothingFactor1_TCGA", "smoothingFactor2_TCGA",
      "smoothingExplanation1_TCGA", "smoothingExplanation2_TCGA",
      "colorByTCGA", "clearTCGA",
      "downloadUI_TCGA"
    )

    for (element in elements_to_hide) {
      shinyjs::hide(element)
    }

    output$plot1TCGA <- renderPlotly({
      plotly::plot_ly(type = "scatter", mode = "markers") %>%
        plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
        plotly::layout(title = "")
    })
    output$plot2TCGA <- renderPlotly({
      plotly::plot_ly(type = "scatter", mode = "markers") %>%
        plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
        plotly::layout(title = "")
    })

    output$downloadUI_TCGA <- renderUI({
      NULL
    })

    shinyjs::show("submitTCGA")
    shinyjs::enable("submitTCGA")
    updateSliderInput(session, "smoothingFactor1_TCGA", value = 0.45)
    updateSliderInput(session, "smoothingFactor2_TCGA", value = 0.45)

    invalidateLater(1)
  })

  observeEvent(input$clearPCAWG, {
    pcawg_analysis(NULL)
    plots_ready_PCAWG(FALSE)
    tab_states$pcawg <- FALSE

    shinyjs::enable("cancer_type_PCAWG")
    shinyjs::enable("sample_name_PCAWG")

    if (!is.null(sample_PCAWG) && nrow(sample_PCAWG) > 0) {
      default_cancer_type <- sample_PCAWG$cancer_type[1]
      default_samples <- sample_PCAWG$samplename[sample_PCAWG$cancer_type == default_cancer_type]
      updateSelectInput(session, "cancer_type_PCAWG", selected = default_cancer_type)
      updateSelectInput(session, "sample_name_PCAWG", choices = default_samples, selected = default_samples[1])
    }

    elements_to_hide <- c(
      "plot1PCAWG", "plot2PCAWG",
      "caption1PCAWG", "caption2PCAWG",
      "smoothingFactor1_PCAWG", "smoothingFactor2_PCAWG",
      "smoothingExplanation1_PCAWG", "smoothingExplanation2_PCAWG",
      "colorByPCAWG", "clearPCAWG",
      "downloadUI_PCAWG"
    )

    for (element in elements_to_hide) {
      shinyjs::hide(element)
    }

    output$plot1PCAWG <- renderPlotly({
      plotly::plot_ly(type = "scatter", mode = "markers") %>%
        plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
        plotly::layout(title = "")
    })
    output$plot2PCAWG <- renderPlotly({
      plotly::plot_ly(type = "scatter", mode = "markers") %>%
        plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
        plotly::layout(title = "")
    })

    output$plot1PCAWG <- renderPlotly({
      NULL
    })
    output$plot2PCAWG <- renderPlotly({
      NULL
    })

    shinyjs::show("submitPCAWG")
    shinyjs::enable("submitPCAWG")

    invalidateLater(1)
  })

  tcga_analysis <- reactiveVal(NULL)
  pcawg_analysis <- reactiveVal(NULL)

  output$plot1TCGA <- renderPlotly({
    req(plots_ready_TCGA())
    current_analysis <- tcga_analysis()
    req(!is.null(current_analysis))

    tryCatch(
      {
        vaf_plot <- create_vaf_plot(
          merged_df = current_analysis$merged_data,
          sample_name = input$sample_name_TCGA,
          purity = current_analysis$purity,
          read_depth = current_analysis$read_depth,
          smoothing_factor = input$smoothingFactor1_TCGA %||% 0.5,
          color_by = input$colorByTCGA %||% "clonality",
          subclonality_counts = current_analysis$subclonality_counts
        )
        return(vaf_plot)
      },
      error = function(e) {
        return(plotly::plot_ly(type = "scatter", mode = "markers") %>%
          plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
          plotly::add_annotations(
            text = paste("Error:", e$message),
            showarrow = FALSE
          ) %>%
          plotly::layout(
            xaxis = list(title = "VAF"),
            yaxis = list(title = "Density")
          ))
      }
    )
  })

  output$plot2TCGA <- renderPlotly({
    req(plots_ready_TCGA())
    current_analysis <- tcga_analysis()
    req(!is.null(current_analysis))

    tryCatch(
      {
        cp_plot <- create_cp_plot(
          merged_df = current_analysis$merged_data,
          sample_name = input$sample_name_TCGA,
          smoothing_factor = input$smoothingFactor2_TCGA %||% 0.5,
          color_by = input$colorByTCGA %||% "clonality",
          subclonality_counts = current_analysis$subclonality_counts,
          df_subc = current_analysis$subclonal_structure
        )
        return(cp_plot)
      },
      error = function(e) {
        return(plotly::plot_ly(type = "scatter", mode = "markers") %>%
          plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
          plotly::add_annotations(
            text = paste("Error:", e$message),
            showarrow = FALSE
          ) %>%
          plotly::layout(
            xaxis = list(title = "CP"),
            yaxis = list(title = "Density")
          ))
      }
    )
  })

  output$caption1TCGA <- renderUI({
    req(plots_ready_TCGA())
    get_vaf_plot_caption()
  })

  output$caption2TCGA <- renderUI({
    req(plots_ready_TCGA())
    get_cp_plot_caption()
  })

  output$smoothingExplanation1_TCGA <- renderUI({
    req(tcga_analysis())
    recommended <- recommended_smoothing_factor(tcga_analysis()$total_mutations)
    HTML(paste0("Recommended smoothing factor: ", recommended))
  })

  output$smoothingExplanation2_TCGA <- renderUI({
    req(tcga_analysis())
    recommended <- recommended_smoothing_factor(tcga_analysis()$total_mutations)
    HTML(paste0("Recommended smoothing factor: ", recommended))
  })

  output$plot1PCAWG <- renderPlotly({
    req(plots_ready_PCAWG())
    current_analysis <- pcawg_analysis()
    req(!is.null(current_analysis))

    tryCatch(
      {
        if (is.null(current_analysis$merged_data) || nrow(current_analysis$merged_data) == 0) {
          return(
            plotly::plot_ly(type = "scatter", mode = "markers") %>%
              plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
              plotly::add_annotations(
                text = "No valid data found for this sample",
                showarrow = FALSE
              ) %>%
              plotly::layout(
                xaxis = list(title = "VAF"),
                yaxis = list(title = "Density")
              )
          )
        }

        vaf_plot <- create_vaf_plot(
          merged_df = current_analysis$merged_data,
          sample_name = input$sample_name_PCAWG,
          purity = current_analysis$purity,
          read_depth = list(
            mean = current_analysis$read_depth$mean,
            min = ifelse(!is.null(current_analysis$merged_data$sum_counts) && length(current_analysis$merged_data$sum_counts) > 0,
              min(current_analysis$merged_data$sum_counts, na.rm = TRUE), 0
            ),
            max = ifelse(!is.null(current_analysis$merged_data$sum_counts) && length(current_analysis$merged_data$sum_counts) > 0,
              max(current_analysis$merged_data$sum_counts, na.rm = TRUE), 0
            )
          ),
          smoothing_factor = input$smoothingFactor1_PCAWG %||% 0.5,
          color_by = input$colorByPCAWG %||% "clonality",
          subclonality_counts = current_analysis$subclonality_counts
        )
        return(vaf_plot)
      },
      error = function(e) {
        return(
          plotly::plot_ly(type = "scatter", mode = "markers") %>%
            plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
            plotly::add_annotations(
              text = paste("Error:", e$message),
              showarrow = FALSE
            ) %>%
            plotly::layout(
              xaxis = list(title = "VAF"),
              yaxis = list(title = "Density")
            )
        )
      }
    )
  })

  output$plot2PCAWG <- renderPlotly({
    req(plots_ready_PCAWG())
    current_analysis <- pcawg_analysis()
    req(!is.null(current_analysis))

    tryCatch(
      {
        if (is.null(current_analysis$merged_data) || nrow(current_analysis$merged_data) == 0) {
          return(
            plotly::plot_ly(type = "scatter", mode = "markers") %>%
              plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
              plotly::add_annotations(
                text = "No valid data found for this sample",
                showarrow = FALSE
              ) %>%
              plotly::layout(
                xaxis = list(title = "Cellular Prevalence"),
                yaxis = list(title = "Density")
              )
          )
        }

        cp_plot <- create_cp_plot(
          merged_df = current_analysis$merged_data,
          sample_name = input$sample_name_PCAWG,
          smoothing_factor = input$smoothingFactor2_PCAWG %||% 0.5,
          color_by = input$colorByPCAWG %||% "clonality",
          subclonality_counts = current_analysis$subclonality_counts,
          df_subc = current_analysis$subclonal_structure
        )
        return(cp_plot)
      },
      error = function(e) {
        return(
          plotly::plot_ly(type = "scatter", mode = "markers") %>%
            plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
            plotly::add_annotations(
              text = paste("Error:", e$message),
              showarrow = FALSE
            ) %>%
            plotly::layout(
              xaxis = list(title = "Cellular Prevalence"),
              yaxis = list(title = "Density")
            )
        )
      }
    )
  })

  output$caption1PCAWG <- renderUI({
    req(plots_ready_PCAWG())
    get_vaf_plot_caption()
  })

  output$caption2PCAWG <- renderUI({
    req(plots_ready_PCAWG())
    get_cp_plot_caption()
  })

  output$smoothingExplanation1_PCAWG <- renderUI({
    req(pcawg_analysis())
    recommended <- recommended_smoothing_factor(pcawg_analysis()$total_mutations)
    HTML(paste0("Recommended smoothing factor: ", recommended))
  })

  output$smoothingExplanation2_PCAWG <- renderUI({
    req(pcawg_analysis())
    recommended <- recommended_smoothing_factor(pcawg_analysis()$total_mutations)
    HTML(paste0("Recommended smoothing factor: ", recommended))
  })

  output$downloadPlot1TCGA <- downloadHandler(
    filename = function() {
      paste0(input$sample_name_TCGA, "_VAF_", Sys.Date(), ".png")
    },
    content = function(file) {
      current_analysis <- tcga_analysis()
      req(!is.null(current_analysis))
      gg <- create_vaf_plot(
        merged_df           = current_analysis$merged_data,
        sample_name         = input$sample_name_TCGA,
        purity              = current_analysis$purity,
        read_depth          = current_analysis$read_depth,
        smoothing_factor    = input$smoothingFactor1_TCGA %||% 0.5,
        color_by            = input$colorByTCGA %||% "clonality",
        subclonality_counts = current_analysis$subclonality_counts,
        return_gg           = TRUE
      )
      ggplot2::ggsave(file, plot = gg, device = "png", width = 8, height = 5, dpi = 150)
    },
    contentType = "image/png"
  )

  output$downloadPlot2TCGA <- downloadHandler(
    filename = function() {
      paste0(input$sample_name_TCGA, "_CP_", Sys.Date(), ".png")
    },
    content = function(file) {
      current_analysis <- tcga_analysis()
      req(!is.null(current_analysis))
      gg <- create_cp_plot(
        merged_df           = current_analysis$merged_data,
        sample_name         = input$sample_name_TCGA,
        smoothing_factor    = input$smoothingFactor2_TCGA %||% 0.5,
        color_by            = input$colorByTCGA %||% "clonality",
        subclonality_counts = current_analysis$subclonality_counts,
        df_subc             = current_analysis$subclonal_structure,
        return_gg           = TRUE
      )
      ggplot2::ggsave(file, plot = gg, device = "png", width = 8, height = 5, dpi = 150)
    },
    contentType = "image/png"
  )

  output$downloadPlot1PCAWG <- downloadHandler(
    filename = function() {
      paste0(input$sample_name_PCAWG, "_VAF_", Sys.Date(), ".png")
    },
    content = function(file) {
      current_analysis <- pcawg_analysis()
      req(!is.null(current_analysis))
      gg <- create_vaf_plot(
        merged_df           = current_analysis$merged_data,
        sample_name         = input$sample_name_PCAWG,
        purity              = current_analysis$purity,
        read_depth          = current_analysis$read_depth,
        smoothing_factor    = input$smoothingFactor1_PCAWG %||% 0.5,
        color_by            = input$colorByPCAWG %||% "clonality",
        subclonality_counts = current_analysis$subclonality_counts,
        return_gg           = TRUE
      )
      ggplot2::ggsave(file, plot = gg, device = "png", width = 8, height = 5, dpi = 150)
    },
    contentType = "image/png"
  )

  output$downloadPlot2PCAWG <- downloadHandler(
    filename = function() {
      paste0(input$sample_name_PCAWG, "_CP_", Sys.Date(), ".png")
    },
    content = function(file) {
      current_analysis <- pcawg_analysis()
      req(!is.null(current_analysis))
      gg <- create_cp_plot(
        merged_df           = current_analysis$merged_data,
        sample_name         = input$sample_name_PCAWG,
        smoothing_factor    = input$smoothingFactor2_PCAWG %||% 0.5,
        color_by            = input$colorByPCAWG %||% "clonality",
        subclonality_counts = current_analysis$subclonality_counts,
        df_subc             = current_analysis$subclonal_structure,
        return_gg           = TRUE
      )
      ggplot2::ggsave(file, plot = gg, device = "png", width = 8, height = 5, dpi = 150)
    },
    contentType = "image/png"
  )

  output$downloadResultTCGA <- downloadHandler(
    filename = function() {
      selected_file <- input$selectedFileTCGA
      if (grepl(".txt$", selected_file)) {
        selected_file
      } else {
        paste0(selected_file, ".txt")
      }
    },
    content = function(file) {
      req(input$sample_name_TCGA)
      result_path <- file.path(global_TCGA_PCAWG, "TCGA", input$sample_name_TCGA, "final_result", "Best_lambda")
      file_path <- file.path(result_path, input$selectedFileTCGA)

      if (!file.exists(file_path)) {
        stop("File not found")
      }

      file.copy(from = file_path, to = file)
    },
    contentType = "text/plain"
  )

  output$downloadResultPCAWG <- downloadHandler(
    filename = function() {
      selected_file <- input$selectedFilePCAWG
      if (grepl(".txt$", selected_file)) {
        selected_file
      } else {
        paste0(selected_file, ".txt")
      }
    },
    content = function(file) {
      req(input$sample_name_PCAWG)
      result_path <- file.path(global_TCGA_PCAWG, "PCAWG", input$sample_name_PCAWG, "final_result", "Best_lambda")
      file_path <- file.path(result_path, input$selectedFilePCAWG)

      if (!file.exists(file_path)) {
        stop("File not found")
      }

      file.copy(from = file_path, to = file)
    },
    contentType = "text/plain"
  )

  output$downloadResult <- downloadHandler(
    filename = function() {
      selected_file <- input$selectedFile
      if (grepl(".txt$", selected_file)) {
        selected_file
      } else {
        paste0(selected_file, ".txt")
      }
    },
    content = function(file) {
      selected_sample <- input$selectedSample
      result_path <- file.path("forCliPP/CliPP/sample_id/final_result/Best_lambda")
      file_path <- file.path(result_path, input$selectedFile)

      if (!file.exists(file_path)) {
        stop("File not found")
      }

      file.copy(from = file_path, to = file)
    },
    contentType = "text/plain"
  )

  observeEvent(input$`navbar-container`, {
    current_tab <- input$`navbar-container`

    controls <- c(
      "smoothingFactor1", "smoothingFactor2",
      "smoothingFactor1_TCGA", "smoothingFactor2_TCGA",
      "smoothingFactor1_PCAWG", "smoothingFactor2_PCAWG",
      "smoothingFactor1_driver", "smoothingFactor2_driver",
      "smoothingExplanation1", "smoothingExplanation2",
      "smoothingExplanation1_TCGA", "smoothingExplanation2_TCGA",
      "smoothingExplanation1_PCAWG", "smoothingExplanation2_PCAWG",
      "smoothingExplanation1_driver", "smoothingExplanation2_driver"
    )

    for (control in controls) {
      shinyjs::hide(control)
    }

    if (current_tab == "CliPP-on-Web") {
      if (tab_states$clipp_web) {
        shinyjs::show("smoothingFactor1")
        shinyjs::show("smoothingFactor2")
        shinyjs::show("smoothingExplanation1")
        shinyjs::show("smoothingExplanation2")

        shinyjs::show("plot1")
        shinyjs::show("plot2")
        shinyjs::show("caption1")
        shinyjs::show("caption2")
        shinyjs::show("plotOutputArea")
        shinyjs::show("controls-area")
      }
    } else if (current_tab == "CliPP Data Resources") {
      current_subtab <- input$tabset

      if (current_subtab == "TCGA") {
        if (tab_states$tcga) {
          shinyjs::show("smoothingFactor1_TCGA")
          shinyjs::show("smoothingFactor2_TCGA")
          shinyjs::show("smoothingExplanation1_TCGA")
          shinyjs::show("smoothingExplanation2_TCGA")
          shinyjs::show("colorByTCGA")
          shinyjs::show("plot1TCGA")
          shinyjs::show("plot2TCGA")
          shinyjs::show("caption1TCGA")
          shinyjs::show("caption2TCGA")
          shinyjs::show("downloadUI_TCGA")
        }
      } else if (current_subtab == "PCAWG" && tab_states$pcawg) {
        shinyjs::show("smoothingFactor1_PCAWG")
        shinyjs::show("smoothingFactor2_PCAWG")
        shinyjs::show("smoothingExplanation1_PCAWG")
        shinyjs::show("smoothingExplanation2_PCAWG")
        shinyjs::show("colorByPCAWG")
        shinyjs::show("plot1PCAWG")
        shinyjs::show("plot2PCAWG")
        shinyjs::show("caption1PCAWG")
        shinyjs::show("caption2PCAWG")
        shinyjs::show("downloadUI_PCAWG")
      }
    } else if (current_tab == "Driver Mutation" && tab_states$driver) {
      shinyjs::show("smoothingFactor1_driver")
      shinyjs::show("smoothingFactor2_driver")
      shinyjs::show("smoothingExplanation1_driver")
      shinyjs::show("smoothingExplanation2_driver")
      shinyjs::show("driverPlot1")
      shinyjs::show("driverPlot2")
      shinyjs::show("driverCaption1")
      shinyjs::show("driverCaption2")
    }
  })


  observeEvent(input$selectedSample,
    {
      if (!is.null(input$selectedSample) && plots_ready_clipp()) {
        shinyjs::hide("plot1")
        shinyjs::hide("plot2")
        shinyjs::hide("caption1")
        shinyjs::hide("caption2")
        shinyjs::hide("plotOutputArea")
        shinyjs::hide("smoothingFactor1")
        shinyjs::hide("smoothingFactor2")
        shinyjs::hide("smoothingExplanation1")
        shinyjs::hide("smoothingExplanation2")
        shinyjs::hide(selector = ".controls-area")
      }
    },
    ignoreInit = TRUE
  )


  observeEvent(input$view_mode, {
    dataTableProxy("driverMutationTable") %>% selectRows(NULL)

    shinyjs::hide("smoothingFactor1_driver")
    shinyjs::hide("smoothingFactor2_driver")
    shinyjs::hide("smoothingExplanation1_driver")
    shinyjs::hide("smoothingExplanation2_driver")

    selected_cancer_type <- input$cancer_type_driver
    if (is.null(selected_cancer_type) || selected_cancer_type == "") {
      return()
    }

    driver_mutations <- isolate({
      driver_mutation_data %>%
        filter(cancer == selected_cancer_type)
    })

    if (nrow(driver_mutations) == 0) {
      return()
    }

    view_mode <- input$view_mode

    output$driverMutationTable <- DT::renderDataTable({
      if (view_mode == "consolidated") {
        samples <- unique(driver_mutations$sample)
        consolidated <- data.frame(
          sample = character(),
          genes = character(),
          proteins = character(),
          Subclonality = character(),
          mutation_count = integer(),
          stringsAsFactors = FALSE
        )

        for (s in samples) {
          sample_muts <- driver_mutations[driver_mutations$sample == s, ]
          has_clonal <- any(sample_muts$Subclonality == "Clonal")
          has_subclonal <- any(sample_muts$Subclonality == "Subclonal")

          subclonality <- if (has_clonal && has_subclonal) {
            "Clonal, Subclonal"
          } else if (has_clonal) {
            "Clonal"
          } else {
            "Subclonal"
          }

          consolidated <- rbind(consolidated, data.frame(
            sample = s,
            genes = paste(unique(sample_muts$gene), collapse = ", "),
            proteins = paste(unique(sample_muts$protein), collapse = ", "),
            Subclonality = subclonality,
            mutation_count = nrow(sample_muts),
            stringsAsFactors = FALSE
          ))
        }

        DT::datatable(
          consolidated,
          selection = "single",
          options = list(
            pageLength = 10,
            searchHighlight = TRUE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(
            "Subclonality",
            backgroundColor = DT::styleEqual(
              c("Clonal", "Subclonal", "Clonal, Subclonal"),
              c("#C9EDEF", "#FED9D7", "#E6E6FA")
            ),
            color = "black"
          )
      } else {
        display_columns <- c("gene", "sample", "protein", "consequence", "Subclonality", "VAF", "CP_unpenalized")
        display_data <- driver_mutations[, display_columns]

        DT::datatable(
          display_data,
          selection = "single",
          options = list(
            pageLength = 10,
            searchHighlight = TRUE
          ),
          rownames = FALSE
        ) %>%
          DT::formatRound(c("VAF", "CP_unpenalized"), digits = 3) %>%
          DT::formatStyle(
            "Subclonality",
            backgroundColor = DT::styleEqual(
              c("Clonal", "Subclonal"),
              c("#C9EDEF", "#FED9D7")
            ),
            color = "black"
          )
      }
    })
  })

  survival_data <- reactive({
    load_survival_data()
  })

  output$cancerTypeUI_survival <- renderUI({
    data_list <- survival_data()

    if (input$survivalType == "PFI") {
      cancer_types <- get_pfi_cancer_types(data_list)
    } else {
      cancer_types <- get_os_cancer_types(data_list)
    }

    if (length(cancer_types) == 0) {
      return(div(
        tags$p("No data available for the selected survival endpoint.", style = "color: red;")
      ))
    }

    selectInput(
      "cancerType_survival",
      "Select Cancer Type:",
      choices = cancer_types,
      selected = cancer_types[1]
    )
  })

  current_survival_plot <- reactiveVal(NULL)
  current_survival_data <- reactiveVal(NULL)

  observeEvent(input$generateKMPlot, {
    req(input$survivalType, input$cancerType_survival)

    data_list <- survival_data()

    if (input$survivalType == "PFI") {
      df_data <- data_list$PFI_data %>%
        dplyr::filter(can_sub == input$cancerType_survival)
    } else {
      df_data <- data_list$OS_data %>%
        dplyr::filter(can_sub == input$cancerType_survival)
    }

    current_survival_data(df_data)

    gg_plot <- create_km_plot(df_data, input$survivalType, input$cancerType_survival)
    current_survival_plot(gg_plot)
  })
  output$survivalPlotGenerated <- reactive({
    !is.null(current_survival_plot())
  })
  outputOptions(output, "survivalPlotGenerated", suspendWhenHidden = FALSE)

  output$survivalPlot <- renderPlotly({
    plot_obj <- current_survival_plot()

    if (is.null(plot_obj)) {
      return(plot_ly(type = "scatter", mode = "markers") %>%
        add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
        add_annotations(
          text = "Click 'Generate Survival Plot' to view the analysis",
          showarrow = FALSE,
          font = list(size = 16)
        ))
    }

    convert_to_plotly(plot_obj)
  })

  output$survivalPlotInfo <- renderUI({
    req(current_survival_plot())

    tags$div(
      tags$hr(),
      tags$h5(tags$b("Analysis Information:")),
      tags$p(paste(
        "This Kaplan-Meier survival analysis stratifies patients based on their subclonal mutational fraction (sMF).",
        "The optimal cutoff is determined using decision tree analysis to maximize survival differences between groups."
      )),
      tags$p(paste(
        "sMF represents the proportion of mutations that are subclonal (present in only a subset of tumor cells),",
        "providing insight into tumor heterogeneity and evolution."
      ))
    )
  })

  output$survivalSampleInfo <- renderUI({
    df_data <- current_survival_data()

    if (is.null(df_data) || nrow(df_data) == 0) {
      return(NULL)
    }

    n_total <- nrow(df_data)
    endpoint <- if (input$survivalType == "PFI") "progression-free interval" else "overall survival"

    tags$div(
      tags$hr(),
      tags$h5(tags$b("Sample Information:")),
      tags$p(paste("Total samples with", endpoint, "data:", n_total)),
      tags$p(paste("Cancer type/subtype:", input$cancerType_survival))
    )
  })

  output$downloadSurvivalPlot <- downloadHandler(
    filename = function() {
      paste0("survival_plot_", input$cancerType_survival, "_", input$survivalType, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      plot_obj <- current_survival_plot()
      if (!is.null(plot_obj)) {
        ggsave(file, plot_obj, width = 12, height = 8, dpi = 300)
      }
    }
  )


  output$cancer_types_ui_tmb <- renderUI({
    req(input$selected_cohort_tmb)

    if (nrow(tmb_smf_data) == 0) {
      return(tags$p("No TMB/sMF data available"))
    }

    available_types <- tmb_smf_data %>%
      filter(cohort == input$selected_cohort_tmb) %>%
      pull(cancer_type) %>%
      unique() %>%
      sort()

    if (length(available_types) == 0) {
      return(tags$p("No cancer types available for selected cohort"))
    }

    selectInput("selected_cancer_types_tmb",
      "Select Cancer Types:",
      choices = c(setNames(available_types, available_types)),
      selected = NULL,
      multiple = TRUE
    )
  })

  current_tmb_plot <- reactiveVal()
  current_correlation_data <- reactiveVal()

  observeEvent(input$generate_tmb_plot, {
    req(input$selected_cohort_tmb)

    if (nrow(tmb_smf_data) == 0) {
      showNotification("No TMB/sMF data available", type = "error")
      return()
    }

    selected_cancer_types <- NULL
    if (!is.null(input$selected_cancer_types_tmb) && length(input$selected_cancer_types_tmb) > 0) {
      selected_cancer_types <- input$selected_cancer_types_tmb[input$selected_cancer_types_tmb != ""]
      if (length(selected_cancer_types) == 0) {
        selected_cancer_types <- NULL
      }
    }

    plot_obj <- create_tmb_smf_plot(
      data = tmb_smf_data,
      selected_cohorts = c(input$selected_cohort_tmb),
      selected_cancer_types = selected_cancer_types,
      log_scale = input$log_scale_tmb
    )

    current_tmb_plot(plot_obj)

    plot_data <- tmb_smf_data %>%
      filter(cohort == input$selected_cohort_tmb)

    if (!is.null(selected_cancer_types) && length(selected_cancer_types) > 0) {
      plot_data <- plot_data %>%
        filter(cancer_type %in% selected_cancer_types)
    }

    corr_data <- create_correlation_summary(plot_data, c(input$selected_cohort_tmb))
    current_correlation_data(corr_data)
  })

  output$tmb_smf_plot <- renderPlotly({
    plot_obj <- current_tmb_plot()

    if (is.null(plot_obj)) {
      return(plot_ly(type = "scatter", mode = "markers") %>%
        add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
        add_annotations(
          text = "Click 'Generate Survival Plot' to view the analysis",
          showarrow = FALSE,
          font = list(size = 16)
        ))
    }

    tryCatch(
      {
        ggplotly(plot_obj, tooltip = c("x", "y")) %>%
          layout(
            title = list(text = "TMB vs sMF Analysis", font = list(size = 16)),
            showlegend = TRUE
          )
      },
      error = function(e) {
        plot_ly(type = "scatter", mode = "markers") %>%
          add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
          add_annotations(
            text = paste("Error rendering plot:", e$message),
            showarrow = FALSE,
            font = list(size = 14)
          )
      }
    )
  })

  output$correlation_table <- DT::renderDT({
    corr_data <- current_correlation_data()

    if (is.null(corr_data) || nrow(corr_data) == 0) {
      return(data.frame(Message = "Generate plot to see correlation data"))
    }

    display_data <- corr_data %>%
      select(
        "Cancer Type" = cancer_type,
        "Cohorts" = cohorts,
        "Samples" = n_samples,
        "Correlation" = correlation_rounded,
        "P-value" = p_value_formatted,
        "Significance" = significance
      )

    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "ftip"
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Correlation",
        backgroundColor = styleInterval(c(-0.3, 0.3), c("#ffcccc", "white", "#ccffcc"))
      )
  })

  output$tmb_smf_info <- renderUI({
    req(current_tmb_plot())

    plot_data <- tmb_smf_data
    if (!is.null(input$selected_cohort_tmb)) {
      plot_data <- plot_data %>%
        filter(cohort == input$selected_cohort_tmb)
    }

    if (!is.null(input$selected_cancer_types_tmb) && length(input$selected_cancer_types_tmb) > 0) {
      selected_cancer_types <- input$selected_cancer_types_tmb[input$selected_cancer_types_tmb != ""]
      if (length(selected_cancer_types) > 0) {
        plot_data <- plot_data %>%
          filter(cancer_type %in% selected_cancer_types)
      }
    }

    n_total <- nrow(plot_data)
    n_cohorts <- length(unique(plot_data$cohort))
    n_cancer_types <- length(unique(plot_data$cancer_type))

    tags$div(
      tags$hr(),
      tags$h5(tags$b("Analysis Summary:")),
      tags$p(paste("Total samples analyzed:", n_total)),
      tags$p(paste("Number of cohorts:", n_cohorts)),
      tags$p(paste("Number of cancer types:", n_cancer_types)),
      if (n_total > 0) {
        tags$div(
          tags$p(paste("TMB range:", sprintf("%.2f - %.2f", min(plot_data$TMB), max(plot_data$TMB)))),
          tags$p(paste("sMF range:", sprintf("%.3f - %.3f", min(plot_data$sMF), max(plot_data$sMF))))
        )
      }
    )
  })

  output$download_tmb_plot <- downloadHandler(
    filename = function() {
      cohorts_str <- input$selected_cohort_tmb
      paste0("tmb_smf_plot_", cohorts_str, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      plot_obj <- current_tmb_plot()
      if (!is.null(plot_obj)) {
        ggsave(file, plot_obj, width = 16, height = 12, dpi = 300)
      }
    }
  )

  output$download_correlation_table <- downloadHandler(
    filename = function() {
      cohorts_str <- input$selected_cohort_tmb
      paste0("tmb_smf_correlations_", cohorts_str, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      corr_data <- current_correlation_data()
      if (!is.null(corr_data) && nrow(corr_data) > 0) {
        write.csv(corr_data, file, row.names = FALSE)
      }
    }
  )

  output$cohort_ui_heatmap <- renderUI({
    available_cohorts <- get_available_cohorts_heatmap(heatmap_data)

    tags$div(
      class = "cohort-display",
      style = "padding: 15px; background: #f8f9fa; border-radius: 5px; margin-bottom: 15px;",
      tags$h6("Cohorts Included:", style = "margin-bottom: 10px; font-weight: bold;"),
      tags$div(
        style = "display: flex; flex-direction: column; gap: 5px;",
        tags$div(
          style = "display: flex; align-items: center; padding: 3px 0;",
          tags$span("✓", style = "color: #28a745; margin-right: 8px; font-weight: bold;"),
          tags$span("TCGA", style = "color: #495057;")
        ),
        tags$div(
          style = "display: flex; align-items: center; padding: 3px 0;",
          tags$span("✓", style = "color: #28a745; margin-right: 8px; font-weight: bold;"),
          tags$span("OCCAMS", style = "color: #495057;")
        ),
        tags$div(
          style = "display: flex; align-items: center; padding: 3px 0;",
          tags$span("✓", style = "color: #28a745; margin-right: 8px; font-weight: bold;"),
          tags$span("mCRPC", style = "color: #495057;")
        )
      ),
      shinyjs::hidden(
        selectInput("selected_cohorts_heatmap",
          "Hidden cohorts:",
          choices = available_cohorts,
          selected = available_cohorts,
          multiple = TRUE
        )
      )
    )
  })

  output$cancer_types_ui_heatmap <- renderUI({
    req(input$selected_cohorts_heatmap)

    available_types <- get_available_cancer_types_heatmap(heatmap_data)
    if (length(available_types) == 0) {
      return(tags$p("No cancer types available for heatmap analysis"))
    }

    tags$div(
      tags$label("Filter TCGA Cancer Types:",
        style = "font-weight: bold; margin-bottom: 5px; display: block;"
      ),
      selectInput("selected_cancer_types_heatmap",
        NULL, # No label since we have custom label above
        choices = c("All TCGA cancer types" = "", setNames(available_types, available_types)),
        selected = "",
        multiple = TRUE
      )
    )
  })

  current_heatmap_plot <- reactiveVal()
  current_heatmap_summary <- reactiveVal()

  observeEvent(input$generate_heatmap, {
    req(input$selected_cohorts_heatmap)

    if (nrow(heatmap_data) == 0) {
      showNotification("Heatmap data not yet available.", type = "warning")
      return()
    }

    selected_cancer_types <- NULL
    if (!is.null(input$selected_cancer_types_heatmap) && length(input$selected_cancer_types_heatmap) > 0) {
      selected_cancer_types <- input$selected_cancer_types_heatmap[input$selected_cancer_types_heatmap != ""]
      if (length(selected_cancer_types) == 0) {
        selected_cancer_types <- NULL
      }
    }

    tryCatch(
      {
        plot_obj <- create_heatmap_plot(
          data = heatmap_data,
          selected_cohorts = c(input$selected_cohorts_heatmap),
          selected_cancer_types = selected_cancer_types,
          show_numbers = input$show_numbers_heatmap %||% TRUE
        )
        current_heatmap_plot(plot_obj)

        summary_data <- create_heatmap_summary(heatmap_data, c(input$selected_cohorts_heatmap))
        current_heatmap_summary(summary_data)
      },
      error = function(e) {
        warning(paste("Error creating heatmap:", e$message))
      }
    )
  })

  output$heatmapGenerated <- reactive({
    !is.null(current_heatmap_plot())
  })
  outputOptions(output, "heatmapGenerated", suspendWhenHidden = FALSE)

  output$heatmap_plot <- renderPlot({
    plot_obj <- current_heatmap_plot()
    if (is.null(plot_obj)) {
      return(NULL)
    }
    draw(plot_obj)
  })

  output$heatmap_summary_table <- renderDT({
    summary_data <- current_heatmap_summary()
    if (is.null(summary_data)) {
      return(NULL)
    }

    datatable(summary_data,
      options = list(dom = "t", pageLength = -1, ordering = FALSE, searching = FALSE),
      rownames = FALSE
    )
  })

  output$heatmap_info <- renderUI({
    req(current_heatmap_plot())

    tags$div(
      tags$h5(tags$b("Analysis Information:")),
      tags$p("This heatmap displays Pearson correlations between subclonal mutational fraction (sMF) and:"),
      tags$ul(
        tags$li(tags$b("TMB:"), " Tumor mutational burden"),
        tags$li(tags$b("NRPCC:"), " Non-reference point clonal cluster"),
        tags$li(tags$b("Purity:"), " Tumor purity estimates")
      ),
      tags$p("The correlation values range from -1 (perfect negative correlation) to +1 (perfect positive correlation)."),
      tags$p("Colors: Blue = negative correlation, White = no correlation, Red = positive correlation.")
    )
  })

  output$download_heatmap <- downloadHandler(
    filename = function() {
      cohorts_str <- paste(input$selected_cohorts_heatmap, collapse = "_")
      paste0("sMF_correlation_heatmap_", cohorts_str, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      plot_obj <- current_heatmap_plot()
      if (!is.null(plot_obj)) {
        png(file, width = 12, height = 8, units = "in", res = 300)
        draw(plot_obj)
        dev.off()
      }
    }
  )

  output$download_heatmap_data <- downloadHandler(
    filename = function() {
      cohorts_str <- paste(input$selected_cohorts_heatmap, collapse = "_")
      cancer_types <- if (!is.null(input$selected_cancer_types_heatmap) && length(input$selected_cancer_types_heatmap) > 0) {
        paste0("_", paste(input$selected_cancer_types_heatmap, collapse = "_"))
      } else {
        ""
      }
      paste0("sMF_correlation_data_", cohorts_str, cancer_types, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$selected_cohorts_heatmap)

      selected_cancer_types <- input$selected_cancer_types_heatmap
      if (is.null(selected_cancer_types) || length(selected_cancer_types) == 0 || "" %in% selected_cancer_types) {
        selected_cancer_types <- NULL
      }

      heatmap_data <- load_heatmap_data()

      plot_data <- heatmap_data

      if (!is.null(input$selected_cohorts_heatmap) && length(input$selected_cohorts_heatmap) > 0) {
        plot_data <- plot_data %>%
          filter(cohort %in% input$selected_cohorts_heatmap)
      }

      if (!is.null(selected_cancer_types) && length(selected_cancer_types) > 0) {
        plot_data <- plot_data %>%
          filter(cancer_type %in% selected_cancer_types)
      }

      if (nrow(plot_data) > 0) {
        write.csv(plot_data, file, row.names = FALSE)
      }
    }
  )

  output$downloadSim4k <- downloadHandler(
    filename = function() paste0("CliPPSim4k_", format(Sys.Date(), "%Y%m%d"), ".txt"),
    content  = function(file) {
      src <- file.path(global_TCGA_PCAWG, "sample.snv.txt")
      if (file.exists(src)) file.copy(src, file)
      else writeLines("Simulation data not available.", file)
    },
    contentType = "text/plain"
  )

  output$browseSim4k <- downloadHandler(
    filename = function() paste0("CliPPSim4k_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    content  = function(file) {
      src <- file.path(global_TCGA_PCAWG, "CliPP_sim4k_output_MCP_debug.zip")
      if (file.exists(src)) file.copy(src, file)
      else writeLines("Simulation data not available.", file)
    },
    contentType = "application/zip"
  )

  output$browseTCGA <- downloadHandler(
    filename = function() paste0("TCGA_CliPP_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    content  = function(file) {
      src <- file.path(global_TCGA_PCAWG, "TCGA.zip")
      if (file.exists(src)) file.copy(src, file)
      else writeLines("TCGA zip not available.", file)
    },
    contentType = "application/zip"
  )

  output$browsePCAWG <- downloadHandler(
    filename = function() paste0("PCAWG_CliPP_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    content  = function(file) {
      src <- file.path(global_TCGA_PCAWG, "PCAWG.zip")
      if (file.exists(src)) file.copy(src, file)
      else writeLines("PCAWG zip not available.", file)
    },
    contentType = "application/zip"
  )

  output$downloadTCGA_cancer <- downloadHandler(
    filename = function() {
      paste0("TCGA_", input$tcga_cancer_type_dl,
             "_CliPP_raw_", format(Sys.Date(), "%Y%m%d"), ".tsv")
    },
    content = function(file) {
      cohort_cancer_download(
        "TCGA", input$tcga_cancer_type_dl, sample_TCGA, file
      )
    },
    contentType = "text/tab-separated-values"
  )

  output$downloadPCAWG_cancer <- downloadHandler(
    filename = function() {
      paste0("PCAWG_", input$pcawg_cancer_type_dl,
             "_CliPP_raw_", format(Sys.Date(), "%Y%m%d"), ".tsv")
    },
    content = function(file) {
      cohort_cancer_download(
        "PCAWG", input$pcawg_cancer_type_dl, sample_PCAWG, file
      )
    },
    contentType = "text/tab-separated-values"
  )

  output$downloadDriverData <- downloadHandler(
    filename = function() {
      paste0("Driver_Mutation_Data_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      tryCatch({
        driver_file <- file.path(global_TCGA_PCAWG, "Driver_info.csv")
        if (!file.exists(driver_file)) {
          writeLines("Driver data file not found.", file)
          return()
        }
        raw <- read.csv(driver_file, stringsAsFactors = FALSE)
        write.csv(driver_output_cols(raw), file, row.names = FALSE)
      }, error = function(e) {
        writeLines(paste("Error serving file:", e$message), file)
      })
    },
    contentType = "text/csv"
  )

  output$downloadDriverData_cancer <- downloadHandler(
    filename = function() {
      paste0("Driver_Mutation_Data_", input$driver_cancer_type_dl,
             "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      tryCatch({
        cancer <- input$driver_cancer_type_dl
        df <- driver_mutation_data[driver_mutation_data$cancer_type == cancer, ]
        if (nrow(df) == 0) {
          writeLines("No data available for the selected cancer type.", file)
          return()
        }
        write.csv(driver_output_cols(df), file, row.names = FALSE)
      }, error = function(e) {
        writeLines(paste("Error generating file:", e$message), file)
      })
    },
    contentType = "text/csv"
  )

  # ============================================================
  # PHYLOGENETIC TREE TAB
  # ============================================================

  phylo_tree_data <- reactiveVal(NULL)

  output$phyloTreeGenerated <- reactive({ !is.null(phylo_tree_data()) })
  outputOptions(output, "phyloTreeGenerated", suspendWhenHidden = FALSE)

  observeEvent(input$generatePhyloTree, {
    source_choice <- input$phylo_source %||% "tcga"

    result <- tryCatch({
      if (source_choice == "tcga") {
        selected_sample <- input$phylo_sample_tcga
        if (is.null(selected_sample) || selected_sample == "") {
          showNotification("Please select a TCGA sample.", type = "warning")
          return(NULL)
        }
        
        if (!is.null(tcga_analysis()) && (input$sample_name_TCGA %||% "") == selected_sample) {
          analysis <- tcga_analysis()
        } else {
          sample_dir <- file.path(global_TCGA_PCAWG, "TCGA", selected_sample)
          file_data <- read_tcga_files(sample_dir)
          if (is.null(file_data)) {
            showNotification("Error reading TCGA sample files.", type = "error")
            return(NULL)
          }
          analysis <- process_tcga_data_internal(file_data, selected_sample, global_TCGA_PCAWG)
        }
        
        if (is.null(analysis)) {
          showNotification("Could not load TCGA analysis data.", type = "error")
          return(NULL)
        }
        
        list(
          merged_data = analysis$merged_data,
          df_subc     = analysis$subclonal_structure,
          sample_name = selected_sample
        )

      } else if (source_choice == "pcawg") {
        selected_sample <- input$phylo_sample_pcawg
        if (is.null(selected_sample) || selected_sample == "") {
          showNotification("Please select a PCAWG sample.", type = "warning")
          return(NULL)
        }
        
        if (!is.null(pcawg_analysis()) && (input$sample_name_PCAWG %||% "") == selected_sample) {
          analysis <- pcawg_analysis()
        } else {
          sample_dir <- file.path(global_TCGA_PCAWG, "PCAWG", selected_sample)
          file_data <- read_pcawg_files(sample_dir)
          if (is.null(file_data)) {
            showNotification("Error reading PCAWG sample files.", type = "error")
            return(NULL)
          }
          analysis <- process_pcawg_data_internal(file_data, selected_sample, global_TCGA_PCAWG)
        }
        
        if (is.null(analysis)) {
          showNotification("Could not load PCAWG analysis data.", type = "error")
          return(NULL)
        }
        
        list(
          merged_data = analysis$merged_data,
          df_subc     = analysis$subclonal_structure,
          sample_name = selected_sample
        )

      } else {
        selected <- input$phylo_sample_clipp
        if (is.null(selected) || selected == "") {
          showNotification("No uploaded sample selected.", type = "warning")
          return(NULL)
        }
        
        sampleFiles <- uploaded_samples$samples[[selected]]
        analysis <- get_current_analysis(sampleFiles)
        if (is.null(analysis)) {
          showNotification("Analysis not ready. Please wait or ensure the sample was submitted in CliPP-on-Web.", type = "warning")
          return(NULL)
        }
        list(
          merged_data = analysis$merged_data,
          df_subc     = analysis$subclonal_structure,
          sample_name = selected
        )
      }
    }, error = function(e) {
      showNotification(paste("Error retrieving analysis data:", e$message), type = "error")
      NULL
    })

    if (is.null(result)) return()

    cluster_df <- summarise_clusters(result$merged_data, result$df_subc)
    if (is.null(cluster_df) || nrow(cluster_df) == 0) {
      showNotification("Could not extract cluster data from the selected sample.", type = "error")
      return()
    }

    edge_df <- build_phylo_edges(cluster_df)
    phylo_tree_data(list(
      edge_df     = edge_df,
      cluster_df  = cluster_df,
      sample_name = result$sample_name
    ))
  })

  output$phyloTreePlot <- renderPlot({
    td <- phylo_tree_data()
    if (is.null(td)) {
      plot.new()
      text(0.5, 0.5, "Select a data source and click\n\"Generate Tree\" to begin.",
           cex = 1.3, col = "grey60")
      return()
    }
    plot_phylo_tree(td$edge_df, td$cluster_df, td$sample_name)
  }, height = function() {
    td <- phylo_tree_data()
    if (is.null(td)) return(600)
    n_clusters <- nrow(td$cluster_df)
    return(max(800, 300 + n_clusters * 120))
  })

  output$phyloTreeInfo <- renderUI({
    td <- phylo_tree_data()
    req(!is.null(td))
    cluster_df <- td$cluster_df
    n_clusters <- nrow(cluster_df)
    n_muts     <- sum(cluster_df$n_muts, na.rm = TRUE)
    clonal_ccf <- max(cluster_df$ccf, na.rm = TRUE)
    HTML(paste0(
      "<b>Sample:</b> ", td$sample_name, "<br>",
      "<b>Clusters:</b> ", n_clusters, " &nbsp;|&nbsp; ",
      "<b>Total mutations:</b> ", n_muts, "<br>",
      "<b>Clonal cluster CCF:</b> ", round(clonal_ccf, 3), "<br>",
      "<small style='color:#888'>Tree topology: CONIPHER-inspired CCF nesting. ",
      "Branch lengths = CCF difference between parent and child cluster.</small>"
    ))
  })

  output$downloadPhyloTree <- downloadHandler(
    filename = function() {
      td <- phylo_tree_data()
      sn <- if (!is.null(td)) gsub("[^A-Za-z0-9_-]", "_", td$sample_name) else "phylo"
      paste0(sn, "_phylogenetic_tree.pdf")
    },
    content = function(file) {
      td <- phylo_tree_data()
      req(!is.null(td))
      pdf(file, width = 10, height = 8)
      plot_phylo_tree(td$edge_df, td$cluster_df, td$sample_name)
      dev.off()
    }
  )
}
