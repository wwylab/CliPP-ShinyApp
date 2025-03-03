library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(purrr)

#' UI function for driver mutation module
#' @param id The module ID
#' @return A Shiny UI definition
driverMutationUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      uiOutput(ns("cancer_type_ui_driver"))
    ),
    mainPanel(
      DT::DTOutput(ns("driverMutationTable")),
      uiOutput(ns("selectedMutationInfo")),
      downloadButton(ns("downloadDriverMutation"), "Download Data"),
      tags$h4(tags$b(tags$span("Reference:"))),
      p("Martínez-Jiménez, Francisco, et al. 'A compendium of mutational cancer driver genes.' Nature Reviews Cancer 20.10 (2020): 555-572."),
      
      div(class = "plot-container",
          div(class = "plot-area",
              plotlyOutput(ns("driverPlot1")) 
          ),
          hidden(div(class = "controls-area",
                     sliderInput(ns("smoothingFactor1_driver"), "Smoothing Factor:",
                                 min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                     div(class = "recommended-smoothing",
                         uiOutput(ns("smoothingExplanation1_driver"))
                     )
          )),
          div(class = "caption-area",
              uiOutput(ns("driverCaption1"))
          )
      ),
      
      div(class = "plot-container",
          div(class = "plot-area",
              plotlyOutput(ns("driverPlot2")) 
          ),
          hidden(div(class = "controls-area",
                     sliderInput(ns("smoothingFactor2_driver"), "Smoothing Factor:",
                                 min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                     div(class = "recommended-smoothing",
                         uiOutput(ns("smoothingExplanation2_driver"))
                     )
          )),
          div(class = "caption-area",
              uiOutput(ns("driverCaption2"))
          )
      )
    )
  )
}

#' Server function for driver mutation module
#' @param id The module ID
#' @param driver_data Reactive containing driver mutation data
#' @param global_path Path to data directory
#' @return A module server function
driverMutationServer <- function(id, driver_data, global_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    create_annotations <- function(subclonality_counts) {
      annotations <- list()
      
      if ("Clonal" %in% subclonality_counts$Subclonality) {
        n_clonal <- subclonality_counts$n[subclonality_counts$Subclonality == "Clonal"]
        if (length(n_clonal) > 0 && n_clonal > 0) {
          annotations <- c(
            annotations, 
            annotate("text", x = 0.85, y = -1.65, label = paste0("n clonal = ", n_clonal), 
                     size = 3, color = "black", vjust = -1.5)
          )
        }
      }
      
      if ("Subclonal" %in% subclonality_counts$Subclonality) {
        n_subclonal <- subclonality_counts$n[subclonality_counts$Subclonality == "Subclonal"]
        if (length(n_subclonal) > 0 && n_subclonal > 0) {
          annotations <- c(
            annotations, 
            annotate("text", x = 0.85, y = -2.65, label = paste0("n subclonal = ", n_subclonal), 
                     size = 3, color = "black", vjust = -1.5)
          )
        }
      }
      
      return(annotations)
    }
    
    # Recommended smoothing factor function
    recommended_smoothing_factor <- function(total_mutations) {
      min_smooth <- 0.2
      max_smooth <- 0.7
      min_mut <- 100
      max_mut <- 10000
      smoothing_factor <- max_smooth - (total_mutations - min_mut) * (max_smooth - min_smooth) / (max_mut - min_mut)
      smoothing_factor <- max(min_smooth, min(max_smooth, smoothing_factor))
      return(round(smoothing_factor, 2))
    }
    
    # Initialize cancer type choices for both tabs
    observe({
      req(driver_data())

      df <- driver_data()
      cancer_col <- if("cancer_type" %in% colnames(df)) "cancer_type" else "cancer"
      
      cancer_types <- sort(unique(df[[cancer_col]]))
      
      updateSelectInput(session, "cancer_type_gene",
                        choices = cancer_types)
      updateSelectInput(session, "cancer_type_sample",
                        choices = cancer_types)
    })
    
    # Update gene choices based on cancer type
    observeEvent(input$cancer_type_gene, {
      req(driver_data(), input$cancer_type_gene)
      cat("Updating genes for cancer type:", input$cancer_type_gene, "\n")
      
      df <- driver_data()
      cancer_col <- if("cancer_type" %in% colnames(df)) "cancer_type" else "cancer"
      
      filtered_data <- df[df[[cancer_col]] == input$cancer_type_gene, ]
      genes <- sort(unique(filtered_data$gene))
      
      updateSelectInput(session, "gene_name",
                        choices = genes)
    })
    
    # Update sample choices based on cancer type
    observeEvent(input$cancer_type_sample, {
      req(driver_data(), input$cancer_type_sample)

      df <- driver_data()
      cancer_col <- if("cancer_type" %in% colnames(df)) "cancer_type" else "cancer"
      
      filtered_data <- df[df[[cancer_col]] == input$cancer_type_sample, ]
      samples <- sort(unique(filtered_data$sample))
      
      updateSelectInput(session, "sample_name",
                        choices = samples)
    })
    
    # Process data by gene
    observeEvent(input$gene_name, {
      req(driver_data(), input$cancer_type_gene, input$gene_name)

      filtered_data <- tryCatch({
        cancer_type_val <- input$cancer_type_gene
        gene_name_val <- input$gene_name
        
        df <- driver_data()
        cancer_col <- if("cancer_type" %in% colnames(df)) "cancer_type" else "cancer"
        
        df[df[[cancer_col]] == cancer_type_val & df$gene == gene_name_val, ]
      }, error = function(e) {
        cat("Error filtering driver data:", e$message, "\n")
        NULL
      })
      
      # Display data table
      output$gene_mutation_table <- renderDT({
        if (is.null(filtered_data) || nrow(filtered_data) == 0) {
          return(datatable(data.frame(message = "No driver mutations found for this gene"),
                           options = list(dom = 't')))
        }
        
        display_cols <- intersect(
          c("sample", "gene", "protein", "consequence", "Subclonality", "VAF", "CP_unpenalized"),
          colnames(filtered_data)
        )
        
        display_data <- filtered_data[, display_cols]
        
        datatable(display_data,
                  options = list(pageLength = 10,
                                 searchHighlight = TRUE),
                  rownames = FALSE) %>%
          formatStyle('Subclonality',
                      backgroundColor = styleEqual(
                        c("Clonal", "Subclonal"),
                        c('#C9EDEF', '#FED9D7')
                      ))
      })
      
      # Process data for visualization 
      if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
        sample_mutation <- filtered_data[1, ]
        sample_name <- sample_mutation$sample
        
        output$plot1 <- renderPlotly({
          default_data <- data.frame(
            AF = c(rnorm(100, 0.15, 0.05), rnorm(50, 0.25, 0.08), rnorm(30, 0.5, 0.1)),
            Subclonality = c(rep("Clonal", 100), rep("Subclonal", 80)),
            ReadDepth = sample(50:300, 180)
          )
          
          mutation_vaf <- sample_mutation$VAF
          
          # Subclonality counts
          subclonality_counts <- data.frame(
            Subclonality = c("Clonal", "Subclonal"),
            n = c(100, 80)
          )
          
          g <- ggplot(default_data, aes(x = AF)) +
            geom_density(fill = "gray", alpha = .6, adjust = input$smoothingFactor1) +
            geom_point(
              shape = 142,
              size = 5,
              alpha = 0.3,
              aes(
                y = -0.75,
                color = Subclonality
              )
            ) +
            geom_point(
              data = data.frame(
                AF = mutation_vaf,
                Subclonality = sample_mutation$Subclonality,
                y = -0.75
              ),
              aes(x = AF, y = y, fill = Subclonality),
              shape = 21,
              size = 8,
              color = "black",
              alpha = 0.7
            ) +
            geom_text(
              data = data.frame(
                AF = mutation_vaf,
                y = -1.25,
                label = input$gene_name
              ),
              aes(x = AF, y = y, label = label),
              size = 3,
              angle = 45,
              hjust = 0
            ) +
            xlim(0, 1) +
            ggtitle(paste(sample_name, " - VAF Distribution")) +
            xlab("VAF") + 
            ylab("Density") +
            theme_pubr() +
            scale_color_manual(values = c("Clonal" = "#87CEEB", "Subclonal" = "#FFA07A")) +
            scale_fill_manual(values = c("Clonal" = "#87CEEB", "Subclonal" = "#FFA07A")) +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
            create_annotations(subclonality_counts)
          
          ggplotly(g) %>% 
            config(displayModeBar = TRUE, scrollZoom = TRUE)
        })
        
        # Create CP plot
        output$plot2 <- renderPlotly({
          default_data <- data.frame(
            CP_unpenalized = c(rnorm(100, 0.4, 0.05), rnorm(50, 0.6, 0.08), rnorm(30, 0.8, 0.05)),
            Subclonality = c(rep("Clonal", 100), rep("Subclonal", 80)),
            ReadDepth = sample(50:300, 180)
          )
          
          mutation_cp <- sample_mutation$CP_unpenalized
          
          subclonality_counts <- data.frame(
            Subclonality = c("Clonal", "Subclonal"),
            n = c(100, 80)
          )
          
          g <- ggplot(default_data, aes(x = CP_unpenalized)) +
            geom_density(fill = "seagreen", alpha = .6, adjust = input$smoothingFactor2) +
            geom_point(
              shape = 142,
              size = 5,
              alpha = 0.3,
              aes(
                y = -0.75,
                color = Subclonality
              )
            ) +
            geom_point(
              data = data.frame(
                CP_unpenalized = mutation_cp,
                Subclonality = sample_mutation$Subclonality,
                y = -0.75
              ),
              aes(x = CP_unpenalized, y = y, fill = Subclonality),
              shape = 21,
              size = 8,
              color = "black",
              alpha = 0.7
            ) +
            geom_text(
              data = data.frame(
                CP_unpenalized = mutation_cp,
                y = -1.25,
                label = input$gene_name
              ),
              aes(x = CP_unpenalized, y = y, label = label),
              size = 3,
              angle = 45,
              hjust = 0
            ) +
            xlim(0, 1.5) +
            ggtitle("CP Distribution - Selected Mutation") +
            xlab("Estimated mutation-specific CP") + 
            ylab("Density") +
            theme_pubr() +
            scale_color_manual(values = c("Clonal" = "#87CEEB", "Subclonal" = "#FFA07A")) +
            scale_fill_manual(values = c("Clonal" = "#87CEEB", "Subclonal" = "#FFA07A")) +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
            geom_segment(
              data = data.frame(
                x = c(0.4, 0.6, 0.8),
                xend = c(0.4, 0.6, 0.8),
                y = 0,
                yend = 3
              ),
              aes(x = x, xend = xend, y = y, yend = yend),
              linetype = "dashed"
            ) +
            create_annotations(subclonality_counts)
          
          ggplotly(g) %>% 
            config(displayModeBar = TRUE, scrollZoom = TRUE)
        })
      }
    })
    
    # Process data by sample
    observeEvent(input$sample_name, {
      req(driver_data(), input$cancer_type_sample, input$sample_name)

      filtered_data <- tryCatch({
        cancer_type_val <- input$cancer_type_sample
        sample_name_val <- input$sample_name
        
        df <- driver_data()
        cancer_col <- if("cancer_type" %in% colnames(df)) "cancer_type" else "cancer"
        
        df[df[[cancer_col]] == cancer_type_val & df$sample == sample_name_val, ]
      }, error = function(e) {
        cat("Error filtering driver data:", e$message, "\n")
        NULL
      })
      
      # Display data table
      output$sample_mutation_table <- renderDT({
        if (is.null(filtered_data) || nrow(filtered_data) == 0) {
          return(datatable(data.frame(message = "No driver mutations found for this sample"),
                           options = list(dom = 't')))
        }
        
        display_cols <- intersect(
          c("gene", "protein", "consequence", "Subclonality", "VAF", "CP_unpenalized"),
          colnames(filtered_data)
        )
        
        display_data <- filtered_data[, display_cols]
        
        datatable(display_data,
                  options = list(pageLength = 10,
                                 searchHighlight = TRUE),
                  rownames = FALSE) %>%
          formatStyle('Subclonality',
                      backgroundColor = styleEqual(
                        c("Clonal", "Subclonal"),
                        c('#C9EDEF', '#FED9D7')
                      ))
      })
    })
    
    # Download handler for gene data
    output$download_gene_data <- downloadHandler(
      filename = function() {
        paste0(
          "Driver_Mutations_",
          input$cancer_type_gene, "_",
          input$gene_name, "_",
          format(Sys.Date(), "%Y%m%d"),
          ".txt"
        )
      },
      content = function(file) {
        df <- driver_data()
        cancer_col <- if("cancer_type" %in% colnames(df)) "cancer_type" else "cancer"
        
        filtered_data <- df[df[[cancer_col]] == input$cancer_type_gene & 
                              df$gene == input$gene_name, ]
        
        if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
          write.table(
            filtered_data,
            file,
            sep = "\t",
            row.names = FALSE,
            quote = FALSE
          )
        } else {
          writeLines("No data available", file)
        }
      },
      contentType = "text/plain"
    )
    
    # Download handler for sample data
    output$download_sample_data <- downloadHandler(
      filename = function() {
        paste0(
          "Driver_Mutations_",
          input$cancer_type_sample, "_",
          input$sample_name, "_",
          format(Sys.Date(), "%Y%m%d"),
          ".txt"
        )
      },
      content = function(file) {
        df <- driver_data()
        cancer_col <- if("cancer_type" %in% colnames(df)) "cancer_type" else "cancer"
        
        filtered_data <- df[df[[cancer_col]] == input$cancer_type_sample & 
                              df$sample == input$sample_name, ]
        
        if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
          write.table(
            filtered_data,
            file,
            sep = "\t",
            row.names = FALSE,
            quote = FALSE
          )
        } else {
          writeLines("No data available", file)
        }
      },
      contentType = "text/plain"
    )
  })
}
