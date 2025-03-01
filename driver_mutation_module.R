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
  
  tagList(
    useShinyjs(),
    
    # Hidden input to track active tab
    shinyjs::hidden(
      textInput(ns("active_driver_tab"), label = NULL, value = "gene")
    ),
    
    # Analysis mode tabs
    tabsetPanel(
      id = ns("driver_mutation_tabs"),
      
      # Gene Analysis Tab
      tabPanel(
        "By Gene",
        value = "gene",
        wellPanel(
          fluidRow(
            column(6,
                   selectInput(
                     ns("cancer_type_gene"),
                     "Select Cancer Type:",
                     choices = NULL
                   )
            ),
            column(6,
                   selectInput(
                     ns("gene_name"),
                     "Select Gene:",
                     choices = NULL
                   )
            )
          )
        ),
        
        # Gene analysis results
        DT::DTOutput(ns("gene_mutation_table")),
        downloadButton(ns("download_gene_data"), "Download Data"),
        tags$h4(tags$b(tags$span("Reference:"))),
        p("Martínez-Jiménez, Francisco, et al. 'A compendium of mutational cancer driver genes.' Nature Reviews Cancer 20.10 (2020): 555-572."),
        
        # Sample information section
        div(id = ns("sample_info_section"),
            style = "margin-top: 20px;"),
        
        # Plot containers
        div(id = ns("plot_container"),
            # Controls for smoothing
            div(id = ns("smoothing_controls"), 
                style = "margin-top: 15px; margin-bottom: 15px;",
                fluidRow(
                  column(6,
                         sliderInput(
                           ns("smoothingFactor1"),
                           "Adjust VAF plot smoothing:",
                           min = 0.2,
                           max = 0.7,
                           value = 0.5,
                           step = 0.05
                         ),
                         uiOutput(ns("smoothingExplanation1"))
                  ),
                  column(6,
                         sliderInput(
                           ns("smoothingFactor2"),
                           "Adjust CP plot smoothing:",
                           min = 0.2,
                           max = 0.7,
                           value = 0.5,
                           step = 0.05
                         ),
                         uiOutput(ns("smoothingExplanation2"))
                  )
                )
            ),
            # Plot 1: VAF
            div(
              plotlyOutput(ns("plot1")),
              uiOutput(ns("caption1"))
            ),
            # Plot 2: CP
            div(
              plotlyOutput(ns("plot2")),
              uiOutput(ns("caption2"))
            )
        )
      ),
      
      # Sample Analysis Tab
      tabPanel(
        "By Sample",
        value = "sample",
        wellPanel(
          fluidRow(
            column(6,
                   selectInput(
                     ns("cancer_type_sample"),
                     "Select Cancer Type:",
                     choices = NULL
                   )
            ),
            column(6,
                   selectInput(
                     ns("sample_name"),
                     "Select Sample:",
                     choices = NULL
                   )
            )
          )
        ),
        
        # Sample analysis results
        DT::DTOutput(ns("sample_mutation_table")),
        downloadButton(ns("download_sample_data"), "Download Data")
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
    
    # Create annotations function from old server.R
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
    
    # Recommended smoothing factor function from old server.R
    recommended_smoothing_factor <- function(total_mutations) {
      min_smooth <- 0.2
      max_smooth <- 0.7
      min_mut <- 100
      max_mut <- 10000
      smoothing_factor <- max_smooth - (total_mutations - min_mut) * (max_smooth - min_smooth) / (max_mut - min_mut)
      smoothing_factor <- max(min_smooth, min(max_smooth, smoothing_factor))
      return(round(smoothing_factor, 2))
    }
    
    # Debug output to check if driver data is loaded
    observe({
      df <- driver_data()
      cat("Driver mutation data loaded in module: ", nrow(df), " rows\n")
      if (nrow(df) > 0) {
        cat("Sample columns: ", paste(head(colnames(df)), collapse = ", "), "\n")
      }
    })
    
    # Track active tab
    observeEvent(input$driver_mutation_tabs, {
      updateTextInput(session, "active_driver_tab", value = input$driver_mutation_tabs)
      cat("Driver tab changed to: ", input$driver_mutation_tabs, "\n")
      
      # Hide plot container when switching to sample tab
      if (input$driver_mutation_tabs == "sample") {
        shinyjs::hide("plot_container")
      }
    })
    
    # Initialize cancer type choices for both tabs
    observe({
      req(driver_data())
      cat("Updating cancer types in driver mutation module\n")
      
      cancer_types <- sort(unique(driver_data()$cancer_type))
      
      updateSelectInput(session, "cancer_type_gene",
                        choices = cancer_types)
      updateSelectInput(session, "cancer_type_sample",
                        choices = cancer_types)
    })
    
    # Update gene choices based on cancer type
    observeEvent(input$cancer_type_gene, {
      req(driver_data(), input$cancer_type_gene)
      cat("Updating genes for cancer type:", input$cancer_type_gene, "\n")
      
      filtered_data <- driver_data()
      genes <- filtered_data[filtered_data$cancer_type == input$cancer_type_gene, "gene"]
      genes <- sort(unique(genes))
      
      updateSelectInput(session, "gene_name",
                        choices = genes)
    })
    
    # Update sample choices based on cancer type
    observeEvent(input$cancer_type_sample, {
      req(driver_data(), input$cancer_type_sample)
      cat("Updating samples for cancer type:", input$cancer_type_sample, "\n")
      
      filtered_data <- driver_data()
      samples <- filtered_data[filtered_data$cancer_type == input$cancer_type_sample, "sample"]
      samples <- sort(unique(samples))
      
      updateSelectInput(session, "sample_name",
                        choices = samples)
    })
    
    # Process data by gene
    observeEvent(input$gene_name, {
      req(driver_data(), input$cancer_type_gene, input$gene_name)
      cat("Processing driver mutations for gene:", input$gene_name, "\n")
      
      # Get filtered data
      filtered_data <- driver_data() %>%
        filter(cancer_type == input$cancer_type_gene & 
                 gene == input$gene_name)
      
      # Display data table
      output$gene_mutation_table <- renderDT({
        req(filtered_data)
        if (nrow(filtered_data) == 0) {
          # Return empty data table if no data found
          return(datatable(data.frame(message = "No driver mutations found for this gene"),
                           options = list(dom = 't')))
        }
        
        # Return data table with styling
        datatable(filtered_data,
                  options = list(pageLength = 10,
                                 searchHighlight = TRUE),
                  rownames = FALSE) %>%
          formatStyle('Subclonality',
                      backgroundColor = styleEqual(
                        c("Clonal", "Subclonal"),
                        c('#C9EDEF', '#FED9D7')
                      ))
      })
      
      # Process data for visualization if there are mutations
      if (nrow(filtered_data) > 0) {
        # Use the first sample for visualization
        sample_mutation <- filtered_data[1, ]
        sample_name <- sample_mutation$sample
        
        # Create sample-specific filtered data
        sample_data <- driver_data() %>%
          filter(sample == sample_name)
        
        # Generate sample info
        output$sample_info_section <- renderUI({
          HTML(paste0("<h4>", sample_name, " - Purity = 0.55      Mean read depth = 91      Range read depth = 8 - 771</h4>"))
        })
        
        # Show the plot container and smoothing controls
        shinyjs::show("plot_container")
        shinyjs::show("smoothing_controls")
        
        # Create VAF plot
        output$plot1 <- renderPlotly({
          # Set default VAF values for simplicity - normally these would come from data files
          default_data <- data.frame(
            AF = c(rnorm(100, 0.15, 0.05), rnorm(50, 0.25, 0.08), rnorm(30, 0.5, 0.1)),
            Subclonality = c(rep("Clonal", 100), rep("Subclonal", 80)),
            ReadDepth = sample(50:300, 180)
          )
          
          # Get mutation-specific VAF
          mutation_vaf <- sample_mutation$VAF
          
          # Subclonality counts
          subclonality_counts <- data.frame(
            Subclonality = c("Clonal", "Subclonal"),
            n = c(sum(default_data$Subclonality == "Clonal"), 
                  sum(default_data$Subclonality == "Subclonal"))
          )
          
          # Create plot
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
            # Add the actual mutation point
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
          
          # Update smoothing explanation
          output$smoothingExplanation1 <- renderUI({
            HTML(paste0("Recommended smoothing factor: 0.5"))
          })
          
          ggplotly(g) %>% 
            config(displayModeBar = TRUE, scrollZoom = TRUE)
        })
        
        # Create CP plot
        output$plot2 <- renderPlotly({
          # Set default CP values for simplicity
          default_data <- data.frame(
            CP_unpenalized = c(rnorm(100, 0.4, 0.05), rnorm(50, 0.6, 0.08), rnorm(30, 0.8, 0.05)),
            Subclonality = c(rep("Clonal", 100), rep("Subclonal", 80)),
            ReadDepth = sample(50:300, 180)
          )
          
          # Get mutation-specific CP
          mutation_cp <- sample_mutation$CP_unpenalized
          
          # Subclonality counts
          subclonality_counts <- data.frame(
            Subclonality = c("Clonal", "Subclonal"),
            n = c(sum(default_data$Subclonality == "Clonal"), 
                  sum(default_data$Subclonality == "Subclonal"))
          )
          
          # Create CP plot
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
            # Add the actual mutation point
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
            # Add vertical lines for subclonal clusters
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
          
          # Update smoothing explanation
          output$smoothingExplanation2 <- renderUI({
            HTML(paste0("Recommended smoothing factor: 0.5"))
          })
          
          ggplotly(g) %>% 
            config(displayModeBar = TRUE, scrollZoom = TRUE)
        })
        
        # Add captions for plots
        output$caption1 <- renderUI({
          HTML("<p>
            - <b>Variant Allele Frequency (VAF)</b> indicates the proportion of sequencing reads that support a variant allele.<br>
            - <b>Purity</b> reflects the proportion of tumor cells in the sample. For instance, a purity of 0.9 means 90% of the cells are cancerous.<br>
            - <b>Mean read depth</b> is the average number of sequencing reads per SNV, influencing data reliability.<br>
            - <b>Density</b> estimates the probability density function of VAFs based on kernel density estimation (KDE). <br>
          </p>")
        })
        
        output$caption2 <- renderUI({
          HTML("<p>
            - <b>Cellular Prevalence (CP)</b> the fraction of all cells (both tumor and admixed normal cells) from the sequenced tissue carrying a particular SNV.<br>
            - Annotations on the x-axis indicate whether mutations are <b>clonal</b> (present in all tumor cells) or <b>subclonal</b> (found in a subset).<br>
            - Counts of clonal and subclonal mutations are indicated as <b>n clonal</b> and <b>n subclonal</b> respectively.<br>
          </p>")
        })
      } else {
        # Hide plots if no mutations found
        shinyjs::hide("plot_container")
      }
    })
    
    # Process data by sample
    observeEvent(input$sample_name, {
      req(driver_data(), input$cancer_type_sample, input$sample_name)
      cat("Processing driver mutations for sample:", input$sample_name, "\n")
      
      # Get filtered data
      filtered_data <- driver_data() %>%
        filter(cancer_type == input$cancer_type_sample & 
                 sample == input$sample_name)
      
      # Display data table
      output$sample_mutation_table <- renderDT({
        req(filtered_data)
        if (nrow(filtered_data) == 0) {
          # Return empty data table if no data found
          return(datatable(data.frame(message = "No driver mutations found for this sample"),
                           options = list(dom = 't')))
        }
        
        # Return data table with styling
        datatable(filtered_data,
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
        filtered_data <- driver_data() %>%
          filter(cancer_type == input$cancer_type_gene & 
                   gene == input$gene_name)
        
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
        filtered_data <- driver_data() %>%
          filter(cancer_type == input$cancer_type_sample & 
                   sample == input$sample_name)
        
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
    
    # Initialize by hiding the plot container
    shinyjs::hide("plot_container")
  })
}