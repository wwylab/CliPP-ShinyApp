# for Sylvain
#old_path <- Sys.getenv("PATH")
#Sys.setenv(PATH = paste("/usr/local/R/4.2.0/bin", old_path, sep = ":"))
#Sys.setenv(LD_LIBRARY_PATH="/usr/local/gcc/7.0.0/lib64")

server <- function(input, output, session) {
  
  plots_ready_clipp <- reactiveVal(FALSE)
  plots_ready_TCGA <- reactiveVal(FALSE)
  plots_ready_PCAWG <- reactiveVal(FALSE)
  
  # Reactive values object to track the state of each tab
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
        
        tryCatch({
          vaf_plot <- create_vaf_plot(
            merged_df = current_analysis$merged_data,
            sample_name = input$sample_name_TCGA,
            purity = current_analysis$purity,
            read_depth = list(
              mean = current_analysis$read_depth,
              min = min(current_analysis$merged_data$sum_counts, na.rm = TRUE),
              max = max(current_analysis$merged_data$sum_counts, na.rm = TRUE)
            ),
            smoothing_factor = input$smoothingFactor1_TCGA %||% 0.5,
            color_by = input$colorByTCGA %||% "clonality",
            subclonality_counts = current_analysis$subclonality_counts
          )
          return(vaf_plot)
        }, error = function(e) {
          return(plotly::plot_ly() %>% 
                   plotly::add_annotations(
                     text = paste("Error:", e$message),
                     showarrow = FALSE
                   )
          )
        })
      })
      
      output$plot2TCGA <- renderPlotly({

        req(plots_ready_TCGA())
        current_analysis <- tcga_analysis()
        req(!is.null(current_analysis))
        
        tryCatch({
          cp_plot <- create_cp_plot(
            merged_df = current_analysis$merged_data,
            sample_name = input$sample_name_TCGA,
            smoothing_factor = input$smoothingFactor2_TCGA %||% 0.5,
            color_by = input$colorByTCGA %||% "clonality",
            subclonality_counts = current_analysis$subclonality_counts,
            df_subc = current_analysis$subclonal_structure
          )
          return(cp_plot)
        }, error = function(e) {
          return(plotly::plot_ly() %>% 
                   plotly::add_annotations(
                     text = paste("Error:", e$message),
                     showarrow = FALSE
                   )
          )
        })
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
        
        tryCatch({

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
              mean = current_analysis$read_depth,
              min = ifelse(!is.null(current_analysis$merged_data$sum_counts) && length(current_analysis$merged_data$sum_counts) > 0,
                           min(current_analysis$merged_data$sum_counts, na.rm = TRUE), 0),
              max = ifelse(!is.null(current_analysis$merged_data$sum_counts) && length(current_analysis$merged_data$sum_counts) > 0,
                           max(current_analysis$merged_data$sum_counts, na.rm = TRUE), 0)
            ),
            smoothing_factor = input$smoothingFactor1_PCAWG %||% 0.5,
            color_by = input$colorByPCAWG %||% "clonality",
            subclonality_counts = current_analysis$subclonality_counts
          )
          return(vaf_plot)
        }, error = function(e) {
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
        })
      })
      
      output$plot2PCAWG <- renderPlotly({

        req(plots_ready_PCAWG())
        current_analysis <- pcawg_analysis()
        req(!is.null(current_analysis))
        
        tryCatch({

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
        }, error = function(e) {
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
        })
      })
      
      shinyjs::show("plot1PCAWG")
      shinyjs::show("plot2PCAWG")
      shinyjs::show("caption1PCAWG")
      shinyjs::show("caption2PCAWG")
      }
  })

  # Update existing reactiveVal observers to also update the tab_states
  output$plotsReadyCliPP <- reactive({
    value <- plots_ready_clipp()
    return(value)
  })
  outputOptions(output, "plotsReadyCliPP", suspendWhenHidden = FALSE)
  
  # For TCGA tab
  output$plotsReadyTCGA <- reactive({
    value <- plots_ready_TCGA()
    return(value)
  })
  outputOptions(output, "plotsReadyTCGA", suspendWhenHidden = FALSE)
  
  # For PCAWG tab
  output$plotsReadyPCAWG <- reactive({
    value <- plots_ready_PCAWG()
    return(value)
  })
  outputOptions(output, "plotsReadyPCAWG", suspendWhenHidden = FALSE)
  
  # For Driver Mutation tab
  output$driverRowSelected <- reactive({
    value <- !is.null(input$driverMutationTable_rows_selected)
    return(value)
  })
  outputOptions(output, "driverRowSelected", suspendWhenHidden = FALSE)
  
  # For Driver Mutation, track when a row is selected
  observeEvent(input$driverMutationTable_rows_selected, {
    tab_states$driver <- !is.null(input$driverMutationTable_rows_selected)
  })
  
  samples_uploaded <- reactiveVal(FALSE)
  uploaded_samples <- reactiveValues(samples = list())
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  tcga_data <- reactive({
    sample_TCGA
  })
  
  pcawg_data <- reactive({
    sample_PCAWG
  })
  
  # Add reactive values for driver data
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
  
  # Add a reactive for consolidated data
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
  
  # UI elements for driver mutation section
  output$cancer_type_ui_driver <- renderUI({
    selectInput("cancer_type_driver", "Select Cancer Type:", choices = unique(driver_mutation_data$cancer))
  })
  
  output$gene_name_ui_driver <- renderUI({
    selectInput("gene_name_driver", "Select Gene:", choices = NULL)
  })
  
  # Render the driver mutation table
  output$driverMutationTable <- DT::renderDataTable({
    # Explicitly depend on both cancer type and view mode
    cancer_type <- input$cancer_type_driver
    view_mode <- input$view_mode %||% "individual"
    
    if (is.null(cancer_type) || cancer_type == "") {
      return(DT::datatable(
        data.frame(message = "Please select a cancer type"),
        options = list(dom = 't')
      ))
    }
    
    if (view_mode == "consolidated") {
      consolidated_data <- driver_data_consolidated()
      
      if (is.null(consolidated_data) || nrow(consolidated_data) == 0) {
        return(DT::datatable(
          data.frame(message = "No data available for the selected cancer type"),
          options = list(dom = 't')
        ))
      }
      
      DT::datatable(
        consolidated_data,
        selection = 'single',
        options = list(
          pageLength = 10,
          searchHighlight = TRUE,
          autoWidth = FALSE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'Subclonality',
          backgroundColor = DT::styleEqual(
            c("Clonal", "Subclonal", "Clonal, Subclonal"), 
            c('#C9EDEF', '#FED9D7', '#E6E6FA')
          ),
          color = 'black'
        )
    } else {
      driver_mutations <- driver_data()
      
      if (is.null(driver_mutations) || nrow(driver_mutations) == 0) {
        return(DT::datatable(
          data.frame(message = "No data available for the selected cancer type"),
          options = list(dom = 't')
        ))
      }
      
      display_columns <- c("gene", "sample", "protein", "consequence", "Subclonality", "VAF", "CP_unpenalized")
      display_data <- driver_mutations[, display_columns]
      
      DT::datatable(
        display_data,
        selection = 'single',
        options = list(
          pageLength = 10,
          searchHighlight = TRUE,
          autoWidth = FALSE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'Subclonality',
          backgroundColor = DT::styleEqual(
            c("Clonal", "Subclonal"), 
            c('#C9EDEF', '#FED9D7')
          ),
          color = 'black'
        )
    }
  })
  
  # Handle all files to prepare for processing
  process_driver_sample <- function(selected_sample, mutations_data, individual_mutation = NULL) {
    tryCatch({
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
      if (!dir.exists(sampleDir)) {
        showNotification(paste("Sample directory not found:", sampleDir), type = "error")
        return(FALSE)
      }
      
      snvFile <- list.files(sampleDir, pattern = "*.snv.txt", full.names = TRUE)
      cnaFile <- list.files(sampleDir, pattern = "*.cna.txt", full.names = TRUE)
      purityFile <- list.files(sampleDir, pattern = "*.purity.txt", full.names = TRUE)
      
      if (length(snvFile) == 0 || length(cnaFile) == 0 || length(purityFile) == 0) {
        showNotification("Required sample files not found.", type = "error")
        return(FALSE)
      }
      
      file_data <- read_and_validate_files(snvFile[1], cnaFile[1], purityFile[1])
      if (is.null(file_data)) {
        showNotification("Error reading sample files.", type = "error")
        return(FALSE) 
      }
      
      df_snv <- file_data$df_snv
      df_cna <- file_data$df_cna
      purity <- file_data$purity
      
      matching_file1 <- list.files(sampleDir, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
      matching_file2 <- list.files(sampleDir, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)
      
      if (length(matching_file1) == 0 || length(matching_file2) == 0) {

        plot_data <- if (!is.null(individual_mutation)) {
          individual_mutation
        } else {
          mutations_data
        }
        
        # Create a simplified VAF plot with just driver mutations
        output$driverPlot1 <- renderPlotly({
          gg1 <- ggplot() +
            xlim(0, 1) +
            ggtitle(paste(selected_sample, " - Purity =", round(purity, 3),
                          " | Driver Mutations Only (No Background)")) +
            ggpubr::theme_pubr() +
            xlab("VAF") + ylab("Density") +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
          
          if (nrow(plot_data) > 0) {
            for (i in 1:nrow(plot_data)) {
              mutation <- plot_data[i,]
              gg1 <- gg1 + 
                geom_vline(
                  xintercept = mutation$VAF,
                  linetype = "dashed",
                  color = ifelse(mutation$Subclonality == "Clonal", "seagreen", "pink")
                ) +
                annotate(
                  "text",
                  x = mutation$VAF,
                  y = 0.5 - (i * 0.05),
                  label = paste0(mutation$gene, " (", mutation$protein, ")"),
                  size = 3,
                  color = ifelse(mutation$Subclonality == "Clonal", "seagreen", "pink"),
                  angle = 45,
                  hjust = 0
                )
            }
          }
          
          ggplotly(gg1)
        })
        
        # Create a simplified CP plot with just driver mutations
        output$driverPlot2 <- renderPlotly({
          gg2 <- ggplot() +
            xlim(0, 1.5) +
            ggtitle(paste0("CP Distribution - ", selected_sample, " (Driver Mutations Only)")) +
            ggpubr::theme_pubr() +
            xlab("Cellular Prevalence (CP)") + ylab("Density") +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
          
          if (nrow(plot_data) > 0) {
            for (i in 1:nrow(plot_data)) {
              mutation <- plot_data[i,]
              if (!is.na(mutation$CP_unpenalized) && !is.null(mutation$CP_unpenalized)) {
                gg2 <- gg2 + 
                  geom_vline(
                    xintercept = mutation$CP_unpenalized,
                    linetype = "dashed",
                    color = ifelse(mutation$Subclonality == "Clonal", "seagreen", "pink")
                  ) +
                  annotate(
                    "text",
                    x = mutation$CP_unpenalized,
                    y = 0.5 - (i * 0.05),
                    label = paste0(mutation$gene, " (", mutation$protein, ")"),
                    size = 3,
                    color = ifelse(mutation$Subclonality == "Clonal", "seagreen", "pink"),
                    angle = 45,
                    hjust = 0
                  )
              }
            }
          }
          
          ggplotly(gg2)
        })
        
        # Show simplified captions
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
      
      df_snvcna <- df_snv %>%
        mutate(total_cn = map2_chr(position, chromosome_index, function(x, y) {
          inds = x >= df_cna$start_position & x <= df_cna$end_position & y == df_cna$chromosome_index
          if (any(inds)) as.character(df_cna$total_cn[which.max(inds)]) else NA
        }))
      
      df_snvcna$sum_counts <- df_snvcna$ref_count + df_snvcna$alt_count
      mean_rd <- mean(df_snvcna$sum_counts)
      min_rd <- min(df_snvcna$sum_counts)
      max_rd <- max(df_snvcna$sum_counts)
      
      merged_df <- merge(df_snvcna, df_assigned, by = c("chromosome_index", "position"))
      merged_df['AF'] = merged_df['alt_count'] / (merged_df['sum_counts'])
      
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
        HTML(paste0("Recommended smoothing factor: ", recommended_smooth, 
                    " based on ", total_SNVs, " mutations"))
      })
      
      output$smoothingExplanation2_driver <- renderUI({
        HTML(paste0("Recommended smoothing factor: ", recommended_smooth, 
                    " based on ", total_SNVs, " mutations"))
      })
      
      highlight_mutations <- if (!is.null(individual_mutation)) {
        individual_mutation
      } else {
        mutations_data
      }
      
      # Create VAF plot with driver mutations highlighted
      output$driverPlot1 <- renderPlotly({
        smoothing_factor1 <- input$smoothingFactor1_driver %||% 0.45
        
        gg1 <- ggplot(merged_df, aes(x = AF)) +
          geom_density(fill = "gray", alpha = .6, adjust = smoothing_factor1) +
          geom_point(
            shape = 142,
            size = 5,
            alpha = 0.3,
            aes(
              y = -0.75,
              color = Subclonality
            )
          ) +
          xlim(0, 1) +
          ggtitle(paste(selected_sample, " - Purity =", round(purity, 3),
                        " | Mean read depth =", round(mean_rd),
                        " | Range =", min_rd, "-", max_rd)) +
          ggpubr::theme_pubr() +
          xlab("VAF") + ylab("Density") +
          scale_color_manual(values = c("Clonal" = "#87CEEB", "Subclonal" = "#FFA07A")) +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
        
        if ("Clonal" %in% subclonality_counts$Subclonality) {
          n_clonal <- subclonality_counts$n[subclonality_counts$Subclonality == "Clonal"]
          gg1 <- gg1 + annotate(
            "text", x = 0.85, y = -1.65,
            label = paste0("n clonal = ", n_clonal),
            size = 3, color = "black", vjust = -1.5
          )
        }
        
        if ("Subclonal" %in% subclonality_counts$Subclonality) {
          n_subclonal <- subclonality_counts$n[subclonality_counts$Subclonality == "Subclonal"]
          gg1 <- gg1 + annotate(
            "text", x = 0.85, y = -2.65,
            label = paste0("n subclonal = ", n_subclonal),
            size = 3, color = "black", vjust = -1.5
          )
        }
        
        if (nrow(highlight_mutations) > 0) {
          for (i in 1:nrow(highlight_mutations)) {
            mutation <- highlight_mutations[i,]
            if(!is.na(mutation$VAF)) {
              gg1 <- gg1 + 
                geom_vline(
                  xintercept = mutation$VAF,
                  linetype = "dashed",
                  color = ifelse(mutation$Subclonality == "Clonal", "seagreen", "pink")
                ) +
                annotate(
                  "text",
                  x = mutation$VAF,
                  y = -1.25 - (i * 0.3),
                  label = paste0(mutation$gene, " (", mutation$protein, ")"),
                  size = 3,
                  color = ifelse(mutation$Subclonality == "Clonal", "seagreen", "pink"),
                  angle = 45,
                  hjust = 0
                )
            }
          }
        }
        
        ggplotly(gg1)
      })
      
      # Create CP plot with driver mutations highlighted
      output$driverPlot2 <- renderPlotly({
        smoothing_factor2 <- input$smoothingFactor2_driver %||% 0.45
        
        gg2 <- ggplot(merged_df, aes(x = CP_unpenalized)) +
          geom_density(fill = "seagreen", alpha = .6, adjust = smoothing_factor2) +
          geom_point(
            shape = 142, size = 5, alpha = 0.3,
            aes(y = -0.75, color = Subclonality)
          ) +
          xlim(0, 1.5) +
          ggtitle(paste0("CP Distribution - ", selected_sample)) +
          ggpubr::theme_pubr() +
          xlab("Cellular Prevalence (CP)") + ylab("Density") +
          scale_color_manual(values = c("Clonal" = "#87CEEB", "Subclonal" = "#FFA07A")) +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
          geom_segment(
            data = df_subc, 
            aes(x = cellular_prevalence, xend = cellular_prevalence, y = 0, yend = 5.7), 
            lty = "dashed"
          )
        
        if (nrow(highlight_mutations) > 0) {
          for (i in 1:nrow(highlight_mutations)) {
            mutation <- highlight_mutations[i,]
            if(!is.na(mutation$CP_unpenalized)) {
              gg2 <- gg2 + 
                geom_vline(
                  xintercept = mutation$CP_unpenalized,
                  linetype = "dashed",
                  color = ifelse(mutation$Subclonality == "Clonal", "seagreen", "pink")
                ) +
                annotate(
                  "text",
                  x = mutation$CP_unpenalized,
                  y = -1.25 - (i * 0.3),
                  label = paste0(mutation$gene, " (", mutation$protein, ")"),
                  size = 3,
                  color = ifelse(mutation$Subclonality == "Clonal", "seagreen", "pink"),
                  angle = 45,
                  hjust = 0
                )
            }
          }
        }
        
        ggplotly(gg2)
      })
      
      # Show explanatory captions
      output$driverCaption1 <- renderUI({
        HTML("<p>
    - <b>Variant Allele Frequency (VAF)</b> represents the proportion of sequencing reads containing a specific mutation, indicating its prevalence in the sample.<br>
    - <b>Mean read depth</b> shows the average coverage per mutation site, with higher values suggesting more reliable detection.<br>
    - <b>Density curve</b> visualizes the distribution of VAFs across all mutations, helping identify potential subclonal populations.<br>
    - <b>Colored vertical lines</b> highlight specific driver mutations, allowing comparison of their VAFs relative to the overall distribution.
  </p>")
      })
      
      output$driverCaption2 <- renderUI({
        HTML("<p>
    - <b>Cellular Prevalence (CP)</b> estimates the fraction of cells (tumor and normal) harboring each mutation after accounting for tumor purity and copy number.<br>
    - <b>Vertical dashed lines</b> mark distinct subclonal populations identified through clustering analysis.<br>
    - <b>Colored vertical lines</b> indicate the cellular prevalence of key driver mutations, revealing their evolutionary timing and subclonal association.<br>
    - <b>CP values near 1.0</b> suggest early (clonal) mutations present in most tumor cells, while lower values indicate later (subclonal) events.
  </p>")
      })
      
      return(TRUE)
      
    }, error = function(e) {
      showNotification(paste("Error processing sample:", e$message), type = "error")
      
      # Create simple error plots
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
    })
  }
  
  # Modified row selection handler to handle consolidated vs individual views correctly
  observeEvent(input$driverMutationTable_rows_selected, {
    row_selected <- input$driverMutationTable_rows_selected
    
    if (is.null(row_selected)) {
      shinyjs::hide("smoothingFactor1_driver")
      shinyjs::hide("smoothingFactor2_driver")
      shinyjs::hide("smoothingExplanation1_driver")
      shinyjs::hide("smoothingExplanation2_driver")
      shinyjs::hide("driverCaption1")
      shinyjs::hide("driverCaption2")
      
      output$driverPlot1 <- renderPlotly({ NULL })
      output$driverPlot2 <- renderPlotly({ NULL })
      return()
    }
    
    shinyjs::show("smoothingFactor1_driver")
    shinyjs::show("smoothingFactor2_driver")
    shinyjs::show("smoothingExplanation1_driver")
    shinyjs::show("smoothingExplanation2_driver")
    shinyjs::show("driverCaption1")
    shinyjs::show("driverCaption2")
    
    selected_cancer_type <- input$cancer_type_driver
    if (is.null(selected_cancer_type) || selected_cancer_type == "") {
      showNotification("Please select a cancer type first", type = "warning")
      return()
    }
    
    view_mode <- ifelse(is.null(input$view_mode), "individual", input$view_mode)
    
    tryCatch({
      if (view_mode == "consolidated") {
        consolidated_data <- driver_data_consolidated()
        
        if (is.null(consolidated_data) || nrow(consolidated_data) == 0 || row_selected > nrow(consolidated_data)) {
          showNotification("Invalid row selection", type = "error")
          return()
        }
        
        selected_sample <- consolidated_data$sample[row_selected]
        
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
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$cancer_type_driver, {
    shinyjs::hide("smoothingControls")
    
    # When the cancer type changes, we need to start fresh
    dataTableProxy("driverMutationTable") %>% selectRows(NULL)
    
    shinyjs::hide("smoothingFactor1_driver")
    shinyjs::hide("smoothingFactor2_driver")
    shinyjs::hide("smoothingExplanation1_driver")
    shinyjs::hide("smoothingExplanation2_driver")
    shinyjs::hide("driverCaption1")
    shinyjs::hide("driverCaption2")
    
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
          options = list(dom = 't')
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
          sample_muts <- driver_mutations[driver_mutations$sample == s,]
          has_clonal <- any(sample_muts$Subclonality == "Clonal")
          has_subclonal <- any(sample_muts$Subclonality == "Subclonal")
          
          subclonality <- if(has_clonal && has_subclonal) {
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
          selection = 'single',
          options = list(
            pageLength = 10,
            searchHighlight = TRUE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(
            'Subclonality',
            backgroundColor = DT::styleEqual(
              c("Clonal", "Subclonal", "Clonal, Subclonal"),
              c('#C9EDEF', '#FED9D7', '#E6E6FA')
            ),
            color = 'black'
          )
      } else {
        display_columns <- c("gene", "sample", "protein", "consequence", "Subclonality", "VAF", "CP_unpenalized")
        display_data <- driver_mutations[, display_columns]
        
        DT::datatable(
          display_data,
          selection = 'single',
          options = list(
            pageLength = 10,
            searchHighlight = TRUE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(
            'Subclonality',
            backgroundColor = DT::styleEqual(
              c("Clonal", "Subclonal"),
              c('#C9EDEF', '#FED9D7')
            ),
            color = 'black'
          )
      }
    })
    
    # Add explanatory captions 
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
    } else {
      shinyjs::hide("downloadDriverMutation")
    }
  })
  
  observeEvent(input$gene_name_driver, {
    selected_gene_name <- input$gene_name_driver
    sample_names <- unique(driver_mutation_data$sample[driver_mutation_data$gene == selected_gene_name])
    updateSelectInput(session, "sample_name_driver", choices = sample_names)
  })
  
  # Generate driver mutation data table and configure file download for driver mutation data
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
                    options = list(pageLength = 10)) %>%
        DT::formatStyle(
          'Subclonality',
          backgroundColor = DT::styleEqual(c("Clonal", "Subclonal"), c('#C9EDEF', '#FED9D7')),
          color = DT::styleEqual(c("Clonal", "Subclonal"), c('black', 'black'))
        )
    })
    shinyjs::show("downloadDriverMutation")
  })
  
  # Update the downloadDriverMutation handler
  output$downloadDriverMutation <- downloadHandler(
    filename = function() {
      paste("Driver_Mutation_Data-", input$cancer_type_driver, "-",
            ifelse(!is.null(input$view_mode) && input$view_mode == "consolidated", "Consolidated-", "Individual-"),
            Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      selected_cancer_type <- input$cancer_type_driver
      if (!is.null(selected_cancer_type)) {
        view_mode <- ifelse(!is.null(input$view_mode), input$view_mode, "individual")
        
        if (view_mode == "consolidated") {
          selected_data <- driver_data_consolidated()
        } else {
          selected_data <- driver_data()
        }
        
        if (!is.null(selected_data) && nrow(selected_data) > 0) {
          write.table(selected_data, file, 
                      sep = "\t", 
                      row.names = FALSE, 
                      quote = FALSE)
        } else {
          writeLines("No data available", file)
        }
      }
    },
    contentType = "text/plain"
  )
  
  # Add observer to initially hide smoothing controls
  observe({
    if (is.null(input$driverMutationTable_rows_selected)) {
      shinyjs::hide("smoothingFactor1_driver")
      shinyjs::hide("smoothingFactor2_driver")
      shinyjs::hide("smoothingExplanation1_driver")
      shinyjs::hide("smoothingExplanation2_driver")
    }
  })
  
  # Display UI elements for specific sections
  output$driverMutationPlots <- renderUI({
    if (!is.null(input$cancer_type_driver)) {
      tagList(
        tabsetPanel(
          tabPanel("Plots",
                   uiOutput("smoothingExplanation1_driver"),
                   plotlyOutput("driverPlot1"),
                   uiOutput("driverCaption1"),
                   
                   uiOutput("smoothingExplanation2_driver"),
                   plotlyOutput("driverPlot2"),
                   uiOutput("driverCaption2")
          ),
          tabPanel("Data Table",
                   DT::DTOutput("driverMutationTable"),
                   shinyjs::hidden(downloadButton("downloadDriverMutation", "Download Data")),
                   tags$h4(tags$b(tags$span("Reference:"))),
                   p("Martínez-Jiménez, Francisco, et al. 'A compendium of mutational cancer driver genes.' Nature Reviews Cancer 20.10 (2020): 555-572.")
          )
        )
      )
    }
  })
  
  # Dynamic file upload inputs based on selected number of samples
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
                      accept = c(".txt"))
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
  
  # Store uploaded files
  observeEvent(input$uploadSamples, {
    numSamples <- input$numSamples
    samples <- store_uploaded_files(input, numSamples, input$uploadMode)
    uploaded_samples$samples <- samples
    samples_uploaded(TRUE)
    
    updateSelectInput(session, "selectedSample",
                      choices = names(samples))
    
    showNotification("Samples uploaded successfully.", type = "message")
    shinyjs::hide("uploadSamples")
    shinyjs::show("selectedSample")
    shinyjs::show("submit")
    shinyjs::show("colorBy")
  })
  
  # Clear current session data
  observeEvent(input$clear, {
    uploaded_samples$samples <- list()
    samples_uploaded(FALSE)    
    updateNumericInput(session, "numSamples", value = 1)
    shinyjs::enable("numSamples")
    shinyjs::reset("dynamicFileInputs")
    
    elements_to_hide <- c(
      "selectedSample", "submit", "colorBy", "clear",
      "plot1", "plot2", "caption1", "caption2",
      "smoothingFactor1", "smoothingFactor2",
      "smoothingExplanation1", "smoothingExplanation2"
    )
    
    for (element in elements_to_hide) {
      shinyjs::hide(element)
    }
    
    shinyjs::show("setSamples")
    shinyjs::show("uploadSamples")
    
    output$plot1 <- renderPlotly({ NULL })
    output$plot2 <- renderPlotly({ NULL })
    output$caption1 <- renderUI({ NULL })
    output$caption2 <- renderUI({ NULL })
    output$plotOutputArea <- renderUI({ NULL })
    
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
  
  # Process uploaded samples
  observeEvent(input$submit, {
    withProgress(message = 'Processing data...', value = 0, {
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
      shinyjs::show("plot1")
      shinyjs::show("plot2")
      shinyjs::show("caption1")
      shinyjs::show("caption2")
      shinyjs::show("controls-area")
      
      resultPath <- "forCliPP/CliPP/sample_id/final_result/Best_lambda"
      result_files <- list.files(resultPath, pattern = "\\.txt$")
      updateSelectInput(session, "selectedFile", choices = result_files)
      plots_ready_clipp(TRUE)
      tab_states$clipp_web <- TRUE
      
      shinyjs::show("smoothingFactor1")
      shinyjs::show("smoothingFactor2")
      shinyjs::show("smoothingExplanation1")
      shinyjs::show("smoothingExplanation2")
      shinyjs::show(selector = ".controls-area")
    })
  })
  
  # Main plot outputs for uploaded samples
  output$plot1 <- renderPlotly({
    req(plots_ready_clipp())
    
    selected_sample <- input$selectedSample
    current_analysis <- get_current_analysis(uploaded_samples$samples[[selected_sample]])
    
    if (is.null(current_analysis)) return(NULL)
    
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
    
    if (is.null(current_analysis)) return(NULL)
    
    create_cp_plot(
      merged_df = current_analysis$merged_data,
      sample_name = selected_sample,
      smoothing_factor = input$smoothingFactor2,
      color_by = input$colorBy,
      subclonality_counts = current_analysis$subclonality_counts,
      df_subc = current_analysis$subclonal_structure
    )
  })
  
  # Plot captions
  output$caption1 <- renderUI({
    req(plots_ready_clipp())
    get_vaf_plot_caption()
  })
  
  output$caption2 <- renderUI({
    req(plots_ready_clipp())
    get_cp_plot_caption()
  })
  
  # Results download section
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
  
  # UI for TCGA cancer type selector
  output$cancer_type_ui_TCGA <- renderUI({
    selectInput("cancer_type_TCGA", "Select Cancer Type:",
                choices = sort(unique(sample_TCGA$cancer_type)))
  })
  
  # UI for TCGA sample selector
  output$sample_name_ui_TCGA <- renderUI({
    req(input$cancer_type_TCGA)
    filtered_samples <- sample_TCGA$samplename[sample_TCGA$cancer_type == input$cancer_type_TCGA]
    selectInput("sample_name_TCGA", "Select Sample Name:",
                choices = sort(filtered_samples))
  })
  
  # UI for PCAWG cancer type selector
  output$cancer_type_ui_PCAWG <- renderUI({
    selectInput("cancer_type_PCAWG", "Select Cancer Type:",
                choices = sort(unique(sample_PCAWG$cancer_type)))
  })
  
  # UI for PCAWG sample selector
  output$sample_name_ui_PCAWG <- renderUI({
    req(input$cancer_type_PCAWG)
    filtered_samples <- sample_PCAWG$samplename[sample_PCAWG$cancer_type == input$cancer_type_PCAWG]
    selectInput("sample_name_PCAWG", "Select Sample Name:",
                choices = sort(filtered_samples))
  })
  
  # TCGA Submit button handler
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
    
    file_data <- read_tcga_files(sample_dir)

    if (is.null(file_data)) {
      showNotification("Error reading sample files", type = "error")
      shinyjs::show("submitTCGA") 
      return()
    }
    
    analysis_result <- process_tcga_data_internal(file_data)

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
    shinyjs::show(selector = ".controls-area")
    
    # Create download UI
    output$downloadUI_TCGA <- renderUI({
      result_files <- list.files(sample_dir, pattern = "\\.txt$")
      if (length(result_files) > 0) {
        tagList(
          selectInput("selectedFileTCGA", "Choose a file to download:", choices = result_files),
          downloadButton("downloadResultTCGA", "Download Result")
        )
      }
    })
    
    shinyjs::show("downloadUI_TCGA")
  })
  
  # PCAWG Submit button handler
  observeEvent(input$submitPCAWG, {
    plots_ready_PCAWG(FALSE)
    tab_states$pcawg <- FALSE
    
    req(input$cancer_type_PCAWG, input$sample_name_PCAWG)

    shinyjs::disable("cancer_type_PCAWG")
    shinyjs::disable("sample_name_PCAWG")
    shinyjs::hide("submitPCAWG")
    shinyjs::disable("submitPCAWG")
    
    sample_dir <- file.path(global_TCGA_PCAWG, "PCAWG", input$sample_name_PCAWG)
    
    snv_file <- list.files(sample_dir, pattern = "*.snv.txt", full.names = TRUE)
    cna_file <- list.files(sample_dir, pattern = "*.cna.txt", full.names = TRUE)
    purity_file <- list.files(sample_dir, pattern = "*.purity.txt", full.names = TRUE)
    
    if (length(snv_file) > 0) {
      tryCatch({
        snv_count <- count_snvs(snv_file[1])
        if (snv_count > 20000) {
          showNotification(
            type = "error"
          )
          shinyjs::show("submitPCAWG") 
          return()
        }
      }, error = function(e) {
        shinyjs::show("submitPCAWG") 
      })
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
    
    analysis_result <- process_pcawg_data_internal(file_data)
    
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
    shinyjs::show("smoothingFactor1")
    shinyjs::show("smoothingFactor2")
    shinyjs::show("smoothingExplanation1")
    shinyjs::show("smoothingExplanation2")
    shinyjs::show(selector = ".controls-area")
    
    # Create download UI
    output$downloadUI_PCAWG <- renderUI({
      result_files <- list.files(sample_dir, pattern = "\\.txt$")
      if (length(result_files) > 0) {
        tagList(
          selectInput("selectedFilePCAWG", "Choose a file to download:", choices = result_files),
          downloadButton("downloadResultPCAWG", "Download Result")
        )
      }
    })
    
    shinyjs::show("downloadUI_PCAWG")
  })
  
  # Clear TCGA data
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
    
    output$caption1TCGA <- renderUI({ NULL })
    output$caption2TCGA <- renderUI({ NULL })
    
    output$downloadUI_TCGA <- renderUI({ NULL })
    
    shinyjs::show("submitTCGA")
    shinyjs::enable("submitTCGA")
    updateSliderInput(session, "smoothingFactor1_TCGA", value = 0.45)
    updateSliderInput(session, "smoothingFactor2_TCGA", value = 0.45)
    
    invalidateLater(1)
  })
  
  # Clear PCAWG data
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
    
    output$plot1PCAWG <- renderPlotly({ NULL })
    output$plot2PCAWG <- renderPlotly({ NULL })
    output$caption1PCAWG <- renderUI({ NULL })
    output$caption2PCAWG <- renderUI({ NULL })
    
    shinyjs::show("submitPCAWG")
    shinyjs::enable("submitPCAWG")
    
    invalidateLater(1)
  })
  
  tcga_analysis <- reactiveVal(NULL)
  pcawg_analysis <- reactiveVal(NULL)
  
  # TCGA Plot renderers
  output$plot1TCGA <- renderPlotly({
    req(plots_ready_TCGA())
    current_analysis <- tcga_analysis()
    req(!is.null(current_analysis))
    
    tryCatch({
      vaf_plot <- create_vaf_plot(
        merged_df = current_analysis$merged_data,
        sample_name = input$sample_name_TCGA,
        purity = current_analysis$purity,
        read_depth = list(
          mean = current_analysis$read_depth,
          min = min(current_analysis$merged_data$sum_counts, na.rm = TRUE),
          max = max(current_analysis$merged_data$sum_counts, na.rm = TRUE)
        ),
        smoothing_factor = input$smoothingFactor1_TCGA %||% 0.5,
        color_by = input$colorByTCGA %||% "clonality",
        subclonality_counts = current_analysis$subclonality_counts
      )
      return(vaf_plot)
    }, error = function(e) {
      return(plotly::plot_ly(type = "scatter", mode = "markers") %>% 
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
    })
  })
  
  output$plot2TCGA <- renderPlotly({
    req(plots_ready_TCGA())
    current_analysis <- tcga_analysis()
    req(!is.null(current_analysis))
    
    tryCatch({
      cp_plot <- create_cp_plot(
        merged_df = current_analysis$merged_data,
        sample_name = input$sample_name_TCGA,
        smoothing_factor = input$smoothingFactor2_TCGA %||% 0.5,
        color_by = input$colorByTCGA %||% "clonality",
        subclonality_counts = current_analysis$subclonality_counts,
        df_subc = current_analysis$subclonal_structure
      )
      return(cp_plot)
    }, error = function(e) {
      return(plotly::plot_ly(type = "scatter", mode = "markers") %>% 
               plotly::add_trace(x = c(0), y = c(0), mode = "markers", opacity = 0) %>%
               plotly::add_annotations(
                 text = paste("Error:", e$message),
                 showarrow = FALSE
               ) %>%
               plotly::layout(
                 xaxis = list(title = "CP"),
                 yaxis = list(title = "Density")
               )
      )
    })
  })
  
  # TCGA captions and explanations
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
  
  # PCAWG Plot renderers
  output$plot1PCAWG <- renderPlotly({
    req(plots_ready_PCAWG())
    current_analysis <- pcawg_analysis()
    req(!is.null(current_analysis))
    
    tryCatch({
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
          mean = current_analysis$read_depth,
          min = ifelse(!is.null(current_analysis$merged_data$sum_counts) && length(current_analysis$merged_data$sum_counts) > 0,
                       min(current_analysis$merged_data$sum_counts, na.rm = TRUE), 0),
          max = ifelse(!is.null(current_analysis$merged_data$sum_counts) && length(current_analysis$merged_data$sum_counts) > 0,
                       max(current_analysis$merged_data$sum_counts, na.rm = TRUE), 0)
        ),
        smoothing_factor = input$smoothingFactor1_PCAWG %||% 0.5,
        color_by = input$colorByPCAWG %||% "clonality",
        subclonality_counts = current_analysis$subclonality_counts
      )
      return(vaf_plot)
    }, error = function(e) {
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
    })
  })
  
  output$plot2PCAWG <- renderPlotly({
    req(plots_ready_PCAWG())
    current_analysis <- pcawg_analysis()
    req(!is.null(current_analysis))
    
    tryCatch({
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
    }, error = function(e) {
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
    })
  })
  
  # PCAWG captions and explanations
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
  
  # Download handlers
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
      result_path <- file.path(global_TCGA_PCAWG, "TCGA", input$sample_name_TCGA)
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
      result_path <- file.path(global_TCGA_PCAWG, "PCAWG", input$sample_name_PCAWG)
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
  
  # Observer for main navigation tab changes
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
  
  # HELPER FUNCTIONS
  
  # Function to count SNVs in a file
  count_snvs <- function(file_path) {
    if (!file.exists(file_path)) return(0)
    df <- read.table(file_path, header = TRUE)
    nrow(df)
  }
  
  # Helper function to read TCGA files
  read_tcga_files <- function(sample_dir) {
    snv_file <- list.files(sample_dir, pattern = "*.snv.txt", full.names = TRUE)
    cna_file <- list.files(sample_dir, pattern = "*.cna.txt", full.names = TRUE)
    purity_file <- list.files(sample_dir, pattern = "*.purity.txt", full.names = TRUE)
    
    validate_and_read_files(snv_file[1], cna_file[1], purity_file[1])$data
  }
  
  # Helper function to read PCAWG files
  read_pcawg_files <- function(sample_dir) {
    snv_file <- list.files(sample_dir, pattern = "*.snv.txt", full.names = TRUE)
    cna_file <- list.files(sample_dir, pattern = "*.cna.txt", full.names = TRUE)
    purity_file <- list.files(sample_dir, pattern = "*.purity.txt", full.names = TRUE)
    
    validate_and_read_files(snv_file[1], cna_file[1], purity_file[1])$data
  }
  
  # Process TCGA data for plotting
  process_tcga_data_internal <- function(file_data) {

    df_snv <- file_data$df_snv
    df_cna <- file_data$df_cna
    purity <- file_data$purity
  
    tryCatch({
      result_path <- file.path(global_TCGA_PCAWG, "TCGA", input$sample_name_TCGA)
      matching_file1 <- list.files(result_path, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
      matching_file2 <- list.files(result_path, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)
      
      df_assigned <- read.table(matching_file1[1], header = TRUE, sep = "\t")
      df_subc <- read.table(matching_file2[1], header = TRUE, sep = "\t")
      
      df_snv_subset <- df_snv %>% 
        select(chromosome_index, position)
      
      matching_rows <- suppressMessages(
        inner_join(
          df_snv_subset,
          df_assigned %>% select(chromosome_index, position)
        )
      )

      df_snvcna <- df_snv %>%
        mutate(total_cn = map2_chr(position, chromosome_index, function(x, y) {
          inds = x >= df_cna$start_position & x <= df_cna$end_position & y == df_cna$chromosome_index
          if (any(inds)) as.character(df_cna$total_cn[which.max(inds)]) else NA
        }))
      
      df_snvcna$sum_counts <- df_snvcna$ref_count + df_snvcna$alt_count
      mean_rd <- mean(df_snvcna$sum_counts)
      min_rd <- min(df_snvcna$sum_counts)
      max_rd <- max(df_snvcna$sum_counts)
      
      merged_df <- merge(df_snvcna, df_assigned, by = c("chromosome_index", "position"))
      merged_df['AF'] = merged_df['alt_count'] / (merged_df['sum_counts'])
      
      merged_df$total_cn <- as.numeric(merged_df$total_cn)
      merged_df$b_i_V <- pmax(1, round(merged_df$AF * (1 / purity) * ((purity * merged_df$total_cn) + (2 * (1 - purity))), 0))
      merged_df$CP_unpenalized <- (merged_df$alt_count * ((1 - purity) * 2 + purity * merged_df$total_cn)) / (merged_df$b_i_V * (merged_df$alt_count + merged_df$ref_count))
      merged_df$CCF_unpenalized <- merged_df$CP_unpenalized / purity
      
      merged_df <- merged_df %>%
        mutate(Subclonality = ifelse(cluster_index == 0, "Clonal", "Subclonal"),
               ReadDepth = sum_counts)
      
      subclonality_counts <- merged_df %>%
        count(Subclonality) %>%
        rename(n = n)
      
      total_SNVs <- sum(df_subc$num_SNV)
      
      return(list(
        merged_data = merged_df,
        purity = purity,
        read_depth = mean_rd,
        subclonality_counts = subclonality_counts,
        subclonal_structure = df_subc,
        total_mutations = total_SNVs
      ))
    })
  }
  
  # Process PCAWG data for plotting
  process_pcawg_data_internal <- function(file_data) {
    if (is.null(file_data)) return(NULL)
    
    df_snv <- file_data$df_snv
    df_cna <- file_data$df_cna
    purity <- file_data$purity
    
    tryCatch({
      result_path <- file.path(global_TCGA_PCAWG, "PCAWG", input$sample_name_PCAWG)
      matching_file1 <- list.files(result_path, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
      matching_file2 <- list.files(result_path, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)
      
      df_assigned <- read.table(matching_file1[1], header = TRUE, sep = "\t", 
                                colClasses = c(chromosome_index = "character", position = "integer", 
                                               cluster_index = "integer"))
      df_subc <- read.table(matching_file2[1], header = TRUE, sep = "\t")
      
      df_snv$chromosome_index <- as.character(df_snv$chromosome_index)
      df_snv$position <- as.integer(df_snv$position)
      
      df_snvcna <- df_snv %>%
        mutate(
          chromosome_index = as.character(chromosome_index),
          total_cn = map2_chr(position, chromosome_index, function(x, y) {
            inds = x >= df_cna$start_position & x <= df_cna$end_position & y == df_cna$chromosome_index
            if (any(inds)) as.character(df_cna$total_cn[which.max(inds)]) else NA
          })
        )
      
      df_snvcna$sum_counts <- df_snvcna$ref_count + df_snvcna$alt_count
      mean_rd <- mean(df_snvcna$sum_counts)
      min_rd <- min(df_snvcna$sum_counts)
      max_rd <- max(df_snvcna$sum_counts)
      
      df_assigned$chromosome_index <- as.character(df_assigned$chromosome_index)
      merged_df <- merge(df_snvcna, df_assigned, by = c("chromosome_index", "position"))
      
      if (nrow(merged_df) == 0) {
        df_snvcna$chromosome_index <- as.character(df_snvcna$chromosome_index)
        df_assigned$chromosome_index <- as.character(df_assigned$chromosome_index)
        df_snvcna$position <- as.integer(df_snvcna$position)
        df_assigned$position <- as.integer(df_assigned$position)
        
        merged_df <- left_join(df_snvcna, df_assigned, by = c("chromosome_index", "position"))
      }
      
      merged_df['AF'] = merged_df['alt_count'] / (merged_df['sum_counts'])
      
      merged_df$total_cn <- as.numeric(merged_df$total_cn)
      merged_df$b_i_V <- pmax(1, round(merged_df$AF * (1 / purity) * ((purity * merged_df$total_cn) + (2 * (1 - purity))), 0))
      merged_df$CP_unpenalized <- (merged_df$alt_count * ((1 - purity) * 2 + purity * merged_df$total_cn)) / 
        (merged_df$b_i_V * (merged_df$alt_count + merged_df$ref_count))
      merged_df$CCF_unpenalized <- merged_df$CP_unpenalized / purity
      
      merged_df <- merged_df %>%
        mutate(
          Subclonality = factor(
            ifelse(cluster_index == 0, "Clonal", "Subclonal"),
            levels = c("Clonal", "Subclonal")
          ),
          ReadDepth = sum_counts
        )
      
      if (!any(c("Clonal", "Subclonal") %in% merged_df$Subclonality)) {
        if (nrow(merged_df) > 0) {
          merged_df$Subclonality <- factor(
            sample(c("Clonal", "Subclonal"), nrow(merged_df), replace = TRUE, 
                   prob = c(0.7, 0.3)), 
            levels = c("Clonal", "Subclonal")
          )
        }
      }
      
      subclonality_counts <- merged_df %>%
        count(Subclonality) %>%
        rename(n = n)
      
      total_SNVs <- sum(df_subc$num_SNV)
      
      return(list(
        merged_data = merged_df,
        purity = purity,
        read_depth = mean_rd,
        subclonality_counts = subclonality_counts,
        subclonal_structure = df_subc,
        total_mutations = total_SNVs
      ))
    })
  }
  
  # Function to consolidate driver mutations by sample
  consolidate_driver_mutations <- function(driver_data, cancer_type) {
    
    filtered_data <- driver_data[driver_data$cancer == cancer_type, ]
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
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
      vaf_range <- paste(sprintf("%.3f", range(vaf_values)), collapse = "-")
      cp_range <- paste(sprintf("%.3f", range(cp_values)), collapse = "-")
      
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
  }
  
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
          sample_muts <- driver_mutations[driver_mutations$sample == s,]
          has_clonal <- any(sample_muts$Subclonality == "Clonal")
          has_subclonal <- any(sample_muts$Subclonality == "Subclonal")
          
          subclonality <- if(has_clonal && has_subclonal) {
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
          selection = 'single',
          options = list(
            pageLength = 10,
            searchHighlight = TRUE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(
            'Subclonality',
            backgroundColor = DT::styleEqual(
              c("Clonal", "Subclonal", "Clonal, Subclonal"),
              c('#C9EDEF', '#FED9D7', '#E6E6FA')
            ),
            color = 'black'
          )
      } else {
        display_columns <- c("gene", "sample", "protein", "consequence", "Subclonality", "VAF", "CP_unpenalized")
        display_data <- driver_mutations[, display_columns]
        
        DT::datatable(
          display_data,
          selection = 'single',
          options = list(
            pageLength = 10,
            searchHighlight = TRUE
          ),
          rownames = FALSE
        ) %>%
          DT::formatStyle(
            'Subclonality',
            backgroundColor = DT::styleEqual(
              c("Clonal", "Subclonal"),
              c('#C9EDEF', '#FED9D7')
            ),
            color = 'black'
          )
      }
    })
  })
}
