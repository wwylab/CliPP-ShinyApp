library(reticulate)
library(ggplot2)
library(dplyr)
library(purrr)
library(plotly)
library(readr)
library(shinyjs)
library(DT)

global_TCGA_PCAWG <- "./dataSource"

# Load the sample data
sample_PCAWG <- read.csv(file.path(global_TCGA_PCAWG, "CliPP_PCAWG.tsv"), sep='\t')
sample_TCGA <- read.csv(file.path(global_TCGA_PCAWG, "CliPP_TCGA.tsv"), sep='\t')

# Add dataset column to distinguish between PCAWG and TCGA
sample_PCAWG$dataset <- "PCAWG"
sample_TCGA$dataset <- "TCGA"

sample_data <- rbind(sample_PCAWG, sample_TCGA)

# Load the driver mutation data
driver_mutation_data <- read.delim("./driver_mut_data.tsv")

server <- function(input, output, session) {
  plotsReadyCliPP <- reactiveVal(FALSE)
  plotsReadyTCGA <- reactiveVal(FALSE)
  plotsReadyPCAWG <- reactiveVal(FALSE)
  
  samplesUploaded <- reactiveVal(FALSE)
  # Define reactive values for the uploaded sample files
  uploadedSamples <- reactiveValues(samples = list())
  
  if (Sys.info()[['user']] == 'shiny') {
    # Sys.setenv(RETICULATE_PYTHON = "./python3")
    Sys.setenv(RETICULATE_PYTHON = "/usr/local/bin/python3")
  }
  
  # Define the output directory globally
  outputDir <- getwd()
  resultPath <- file.path(outputDir, "forCliPP/CliPP/sample_id/final_result/Best_lambda")
  
  # Centralized function to read and validate files
  read_and_validate_files <- function(snvFile, cnaFile, purityFile) {
    if (is.null(snvFile) || is.null(cnaFile) || is.null(purityFile)) {
      showNotification("One or more required files are missing.", type = "error")
      return(NULL)
    }
    
    # Reading data
    df_snv <- read.table(snvFile, header = TRUE)
    df_cna <- read.table(cnaFile, header = TRUE)
    purity <- as.numeric(readLines(purityFile, warn = FALSE))
    
    # Ensure the required columns are present
    required_cols <- c("chromosome_index", "position", "ref_count", "alt_count")
    if (!all(required_cols %in% colnames(df_snv))) {
      missing_cols <- required_cols[!required_cols %in% colnames(df_snv)]
      showNotification(paste("The SNV file does not contain the required columns:", paste(missing_cols, collapse = ", ")), type = "error")
      return(NULL)
    }
    
    list(df_snv = df_snv, df_cna = df_cna, purity = purity)
  }
  
  # Generate input samples dynamically based on input amount
  observeEvent(input$setSamples, {
    numSamples <- input$numSamples
    output$dynamicFileInputs <- renderUI({
      lapply(1:numSamples, function(i) {
        tagList(
          h4(paste("Sample", i)),
          textInput(paste0("sampleName", i), "Sample Name:", value = paste0("Sample", i)),
          # All_at_once path
          if (input$uploadMode == "all_at_once") {
            fileInput(paste0("fileInput", i), "Upload all three required files (snv.txt, cna.txt, purity.txt):",
                      multiple = TRUE, accept = c(".txt"))
            # Single upload (gives users less experienced with technology to upload)
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
  
  # Hide plots when selection process is changed
  observeEvent(input$selectedSample, {
    shinyjs::hide("plot1")
    shinyjs::hide("plot2")
    shinyjs::hide("caption1")
    shinyjs::hide("caption2")
    shinyjs::hide("plotOutputArea")
    #print("Sample selection change")
  })
  
  # Hide submit buttons after they are clicked
  observeEvent(input$submitTCGA, {
    shinyjs::hide("submitTCGA")
    shinyjs::show("colorByTCGA")
    shinyjs::show("smoothingFactor1_TCGA")
    shinyjs::show("smoothingFactor2_TCGA")
    #print("Hide TCGA")
  })
  
  observeEvent(input$submitPCAWG, {
    shinyjs::hide("submitPCAWG")
    shinyjs::show("colorByPCAWG")
    shinyjs::show("smoothingFactor1_PCAWG")
    shinyjs::show("smoothingFactor2_PCAWG")
    #print("Hide PCAWG")
  })
  
  # Helper function to store uploaded files
  storeUploadedFiles <- function(input, numSamples) {
    samples <- list()
    for (i in 1:numSamples) {
      sampleName <- input[[paste0("sampleName", i)]]
      if (input$uploadMode == "all_at_once") {
        files <- input[[paste0("fileInput", i)]]
        if (!is.null(files)) {
          uploadedFiles <- files$datapath
          uploadedNames <- files$name
          
          samples[[sampleName]] <- list(
            snvFile = uploadedFiles[grepl("snv.txt", uploadedNames)],
            cnaFile = uploadedFiles[grepl("cna.txt", uploadedNames)],
            purityFile = uploadedFiles[grepl("purity.txt", uploadedNames)]
          )
        }
      } else {
        snvFile <- input[[paste0("snvFile", i)]]
        cnaFile <- input[[paste0("cnaFile", i)]]
        purityFile <- input[[paste0("purityFile", i)]]
        
        # Stores the samples so that they can be uploaded at a later time
        if (!is.null(snvFile) && !is.null(cnaFile) && !is.null(purityFile)) {
          samples[[sampleName]] <- list(
            snvFile = snvFile$datapath,
            cnaFile = cnaFile$datapath,
            purityFile = purityFile$datapath
          )
        }
      }
    }
    return(samples)
  }
  
  # Store uploaded samples
  observeEvent(input$uploadSamples, {
    numSamples <- input$numSamples
    samples <- storeUploadedFiles(input, numSamples)
    uploadedSamples$samples <- samples
    samplesUploaded(TRUE)
    updateSelectInput(session, "selectedSample", choices = names(samples))
    showNotification("Samples uploaded successfully.", type = "message")
    shinyjs::hide("uploadSamples")
    shinyjs::show("selectedSample")
    shinyjs::show("submit")
    shinyjs::show("colorBy")
    shinyjs::show("smoothingFactor1")
    shinyjs::show("smoothingFactor2")
    
    #print("Uploaded samples")
  })
  
  # Event to clear the current batch and restart the upload process
  observeEvent(input$clear, {
    uploadedSamples$samples <- list()
    samplesUploaded(FALSE)
    updateNumericInput(session, "numSamples", value = 1)
    shinyjs::reset("dynamicFileInputs")
    shinyjs::hide("selectedSample")
    shinyjs::hide("submit")
    shinyjs::hide("colorBy")
    shinyjs::hide("clear")
    shinyjs::hide("plot1")
    shinyjs::hide("plot2")
    shinyjs::hide("caption1")
    shinyjs::hide("caption2")
    shinyjs::hide("plotOutputArea")
    shinyjs::hide("smoothingFactor1")
    shinyjs::hide("smoothingFactor2")
    shinyjs::show("setSamples")
    shinyjs::enable("numSamples")
    plotsReadyCliPP(FALSE)
    shinyjs::show("uploadSamples")
    shinyjs::reset("numSamples")
    
    # Ensure all plot related reactive values are reset
    output$plot1 <- renderPlotly({ NULL })
    output$plot2 <- renderPlotly({ NULL })
    output$caption1 <- renderUI({ NULL })
    output$caption2 <- renderUI({ NULL })
    output$plotOutputArea <- renderUI({ NULL })
    plotsReadyCliPP(FALSE)
    
    #print("Cleared all values")
  })
  
  # This event allows for the plots and output to only be displayed after the entire code has been run
  observeEvent(input$submit, {
    plotsReadyCliPP(FALSE)
    
    selectedSample <- input$selectedSample
    sampleFiles <- uploadedSamples$samples[[selectedSample]]
    
    if (is.null(sampleFiles)) {
      showNotification("Please select a sample to analyze.", type = "error")
      return()
    }
    
    snvFile <- sampleFiles$snvFile
    cnaFile <- sampleFiles$cnaFile
    purityFile <- sampleFiles$purityFile
    
    if (is.null(snvFile) || is.null(cnaFile) || is.null(purityFile)) {
      showNotification("One or more required files are missing.", type = "error")
      return()
    }
    
    file_data <- read_and_validate_files(snvFile, cnaFile, purityFile)
    if (is.null(file_data)) return()
    
    df_snv <- file_data$df_snv
    df_cna <- file_data$df_cna
    purity <- file_data$purity
    
    # Define the directory path
    directoryPath <- "forCliPP/CliPP/sample_id/"
    
    # Check if the directory exists, if not, create it
    if (!dir.exists(directoryPath)) {
      dir.create(directoryPath, recursive = TRUE, showWarnings = TRUE)
    } else {
      # Clear previous files in the directory
      system(paste("rm -rf", shQuote(directoryPath)), intern = TRUE)
    }
    
    # Construct the command
    command <- sprintf("/usr/local/bin/python3 ./forCliPP/CliPP/run_clipp_main.py %s %s %s",
                       shQuote(snvFile), shQuote(cnaFile), shQuote(purityFile))
    
    # Execute the command
    system(command, intern = TRUE)
    
    # Update the file selection dropdown
    updateSelectInput(session, "selectedFile", choices = list.files(resultPath, pattern = "\\.txt$"))
    
    # Output message
    output$result <- renderText({
      "Processing complete. Select a file to download."
    })
    plotsReadyCliPP(TRUE)
    # Show the clear button only after submission
    shinyjs::show("clear")
    
    # Ensure that plot and output UI elements are shown
    shinyjs::show("plot1")
    shinyjs::show("plot2")
    shinyjs::show("caption1")
    shinyjs::show("caption2")
    shinyjs::show("plotOutputArea")
    
    #print("Processed data")
  })
  
  # Download handler
  output$downloadResult <- downloadHandler(
    filename = function() {
      file_path <- file.path(resultPath, input$selectedFile)
      validate(need(file.exists(file_path), "File not found."))
      basename(file_path)
    },
    content = function(file) {
      req(input$tabs == "CliPP-on-Web")
      file.copy(from = file.path(resultPath, input$selectedFile), to = file)
    }
  )
  
  output$warning <- renderText({
    if (is.null(input$submit)) return()
  })
  
  output$plotReadyCliPP <- reactive({ plotsReadyCliPP() })
  outputOptions(output, "plotReadyCliPP", suspendWhenHidden = FALSE)
  
  # Dummy text output to trigger the conditional panel
  output$plotReadyTextCliPP <- renderUI({
    if (plotsReadyCliPP()) {
      div(id = "plotReadyTextCliPP", "true")
    } else {
      div(id = "plotReadyTextCliPP", "false")
    }
  })
  
  output$samplesUploaded <- reactive({ samplesUploaded() })
  outputOptions(output, "samplesUploaded", suspendWhenHidden = FALSE)
  
  # Observe plot readiness and render plots
  observe({
    req(plotsReadyCliPP())
    
    #print("Plot 1 rendered")
    
    # Plot 1
    output$plot1 <- renderPlotly({
      # Ensure files are uploaded
      if (input$submit == 0 || !plotsReadyCliPP()) return()
      
      selectedSample <- input$selectedSample
      sampleFiles <- uploadedSamples$samples[[selectedSample]]
      
      if (is.null(sampleFiles)) return()
      
      snvFile <- sampleFiles$snvFile
      cnaFile <- sampleFiles$cnaFile
      purityFile <- sampleFiles$purityFile
      
      if (is.null(snvFile) || is.null(cnaFile) || is.null(purityFile)) return()
      
      file_data <- read_and_validate_files(snvFile, cnaFile, purityFile)
      if (is.null(file_data)) return()
      
      df_snv <- file_data$df_snv
      df_cna <- file_data$df_cna
      purity <- file_data$purity
      
      # Read mutation and subclonal structure assignments
      matching_file1 <- list.files(resultPath, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
      matching_file2 <- list.files(resultPath, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)
      
      if (length(matching_file1) > 0 && length(matching_file2) > 0) {
        df_assigned <- read.table(matching_file1[1], header = TRUE, sep = "\t")
        df_subc <- read.table(matching_file2[1], header = TRUE, sep = "\t")
        
        df_snvcna <- df_snv %>%
          mutate(total_cn = map2_chr(position, chromosome_index, function(x, y) {
            inds = x >= df_cna$start_position & x <= df_cna$end_position & y == df_cna$chromosome_index
            if (any(inds)) as.character(df_cna$total_cn[which.max(inds)]) else NA
          }))
        
        # Calculate the sum of ref_count and alt_count
        df_snvcna$sum_counts <- df_snvcna$ref_count + df_snvcna$alt_count
        
        # Compute the mean of the sum_counts column
        mean_rd <- mean(df_snvcna$sum_counts)
        min_rd <- min(df_snvcna$sum_counts)
        max_rd <- max(df_snvcna$sum_counts)
        
        merged_df <- merge(df_snvcna, df_assigned, by = c("chromosome_index", "position"))
        merged_df['AF'] = merged_df['alt_count'] / (merged_df['sum_counts'])
        
        merged_df$total_cn <- as.numeric(merged_df$total_cn)
        merged_df$b_i_V <- pmax(1, round(merged_df$AF * (1 / purity) * ((purity * merged_df$total_cn) + (2 * (1 - purity))), 0))
        merged_df$CP_unpenalized <- (merged_df$alt_count * ((1 - purity) * 2 + purity * merged_df$total_cn)) / (merged_df$b_i_V * (merged_df$alt_count + merged_df$ref_count))
        merged_df$CCF_unpenalized <- merged_df$CP_unpenalized / purity
        
        total_SNVs <- sum(df_subc$num_SNV)
        # Smoothing is adjustable with slider
        smoothing_factor1 <- input$smoothingFactor1
        
        merged_df <- merged_df %>%
          mutate(Subclonality = ifelse(cluster_index == 0, "Clonal", "Subclonal"),
                 ReadDepth = sum_counts)
        
        # print(head(merged_df))
        
        subclonality_counts <- merged_df %>%
          count(Subclonality)
        
        # Rename the count column to 'n'
        subclonality_counts <- subclonality_counts %>%
          rename(n = n)
        
        # Determine the color aesthetic based on the user input
        color_aesthetic <- if (input$colorBy == "clonality") "Subclonality" else "ReadDepth"
        
        # Create plot
        gg1 <- ggplot(merged_df, aes(x = AF)) +
          geom_density(fill = "gray", alpha = .6, adjust = smoothing_factor1) +
          suppressWarnings(geom_point(
            # Draw horizontal lines instead of points
            shape = 142,
            # Adjust size based on the figure's dimensions
            size = 5,
            # Transparency
            alpha = 0.3,
            aes(
              # Fixed y-position
              y = -0.75,
              # Dynamic color
              color = color_aesthetic
            )
          )) +
          xlim(0, 1) +
          ggtitle(paste(selectedSample, " - Purity =", round(purity, 3), "      ", "Mean read depth =", round(mean_rd,), "      ", "Range read depth =", min_rd, "-", max_rd)) +
          geom_text(data = data.frame(), aes(label = paste0("n clonal = ", subclonality_counts %>% filter(Subclonality == "Clonal") %>% select(n)), x = .85, y = -1.65)) +
          geom_text(data = data.frame(), aes(label = paste0("n subclonal = ", subclonality_counts %>% filter(Subclonality == "Subclonal") %>% select(n)), x = .85, y = -2.65)) +
          ggpubr::theme_pubr() + xlab("VAF") + ylab("Density") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
        
        # , tooltip = c("x", "color", "text"))
        ggplotly(gg1)
      }
    })
    
    # Here is where the captions for graph 1 are stored
    output$caption1 <- renderUI({
      if (plotsReadyCliPP()) {
        HTML("<p> - <b>Variant Allele Frequency (VAF)</b> indicates the proportion of sequencing reads that support a variant allele.<br>
         - <b>Purity</b> reflects the proportion of tumor cells in the sample. For instance, a purity of 0.9 means 90% of the cells are cancerous.<br>
         - <b>Mean read depth</b> is the average number of sequencing reads per SNV, influencing data reliability.<br>
         - <b>Density</b> estimates the probability density function of VAFs based on kernel density estimation (KDE). <br>
    </p>")
      }
    })
    
    #print("Plot 2 rendered")
    
    # Plot 2
    output$plot2 <- renderPlotly({
      if (input$submit == 0 || !plotsReadyCliPP()) return()
      
      selectedSample <- input$selectedSample
      sampleFiles <- uploadedSamples$samples[[selectedSample]]
      
      if (is.null(sampleFiles)) return()
      
      snvFile <- sampleFiles$snvFile
      cnaFile <- sampleFiles$cnaFile
      purityFile <- sampleFiles$purityFile
      
      if (is.null(snvFile) || is.null(cnaFile) || is.null(purityFile)) return()
      
      file_data <- read_and_validate_files(snvFile, cnaFile, purityFile)
      if (is.null(file_data)) return()
      
      df_snv <- file_data$df_snv
      df_cna <- file_data$df_cna
      purity <- file_data$purity
      
      # Read mutation and subclonal structure assignments
      matching_file1 <- list.files(resultPath, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
      matching_file2 <- list.files(resultPath, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)
      
      if (length(matching_file1) > 0 && length(matching_file2) > 0) {
        df_assigned <- read.table(matching_file1[1], header = TRUE, sep = "\t")
        df_subc <- read.table(matching_file2[1], header = TRUE, sep = "\t")
        
        df_snvcna <- df_snv %>%
          mutate(total_cn = map2_chr(position, chromosome_index, function(x, y) {
            inds = x >= df_cna$start_position & x <= df_cna$end_position & y == df_cna$chromosome_index
            if (any(inds)) as.character(df_cna$total_cn[which.max(inds)]) else NA
          }))
        
        # Calculate the sum of ref_count and alt_count
        df_snvcna$sum_counts <- df_snvcna$ref_count + df_snvcna$alt_count
        
        # Compute the mean of the sum_counts column
        mean_rd <- mean(df_snvcna$sum_counts)
        min_rd <- min(df_snvcna$sum_counts)
        max_rd <- max(df_snvcna$sum_counts)
        
        merged_df <- merge(df_snvcna, df_assigned, by = c("chromosome_index", "position"))
        merged_df['AF'] = merged_df['alt_count'] / (merged_df['sum_counts'])
        
        merged_df$total_cn <- as.numeric(merged_df$total_cn)
        merged_df$b_i_V <- pmax(1, round(merged_df$AF * (1 / purity) * ((purity * merged_df$total_cn) + (2 * (1 - purity))), 0))
        merged_df$CP_unpenalized <- (merged_df$alt_count * ((1 - purity) * 2 + purity * merged_df$total_cn)) / (merged_df$b_i_V * (merged_df$alt_count + merged_df$ref_count))
        merged_df$CCF_unpenalized <- merged_df$CP_unpenalized / purity
        
        total_SNVs <- sum(df_subc$num_SNV)
        # Smoothing is adjustable with slider
        smoothing_factor2 <- input$smoothingFactor2
        
        merged_df <- merged_df %>%
          mutate(Subclonality = ifelse(cluster_index == 0, "Clonal", "Subclonal"),
                 ReadDepth = sum_counts)
        
        # print(head(merged_df))
        
        subclonality_counts <- merged_df %>%
          count(Subclonality)
        
        # Rename the count column to 'n'
        subclonality_counts <- subclonality_counts %>%
          rename(n = n)
        
        # Determine the color aesthetic based on the user input
        color_aesthetic <- if (input$colorBy == "clonality") "Subclonality" else "ReadDepth"
        
        # Create plot
        gg2 <- ggplot(merged_df, aes(x = CP_unpenalized)) +
          geom_density(fill = "seagreen", alpha = .6, adjust = smoothing_factor2) +
          suppressWarnings(geom_point(
            # Draw horizontal lines instead of points
            shape = 142,
            # Adjust size based on the figure's dimensions
            size = 5,
            # Transparency
            alpha = 0.3,
            aes(
              # Fixed y-position
              y = -0.75,
              # Dynamic color
              color = color_aesthetic
            )
          )) +
          xlim(0, 1.5) +
          ggtitle(paste(selectedSample, " - Estimated mutation-specific CP")) +
          geom_text(data = data.frame(), aes(label = paste0("n clonal = ", subclonality_counts %>% filter(Subclonality == "Clonal") %>% select(n)), x = 1.3, y = -1.65)) +
          geom_text(data = data.frame(), aes(label = paste0("n subclonal = ", subclonality_counts %>% filter(Subclonality == "Subclonal") %>% select(n)), x = 1.3, y = -2.65)) +
          ggpubr::theme_pubr() + xlab("Estimated mutation-specific CP") + ylab("Density") +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
          geom_segment(data = df_subc, aes(x = cellular_prevalence, xend = cellular_prevalence, y = 0, yend = 5.7), lty = "dashed")
        
        # , tooltip = c("x", "color", "text"))
        ggplotly(gg2)
      }
    })
    
    # Here is where the captions for graph 2 are stored
    output$caption2 <- renderUI({
      if (plotsReadyCliPP()) {
        HTML("<p> - <b>Cellular Prevalence (CP)</b> the fraction of all cells (both tumor and admixed normal cells) from the sequenced tissue carrying a particular SNV.<br>
         - Annotations on the x-axis indicate whether mutations are <b>clonal</b> (present in all tumor cells) or <b>subclonal</b> (found in a subset).<br>
         - Counts of clonal and subclonal mutations are indicated as <b>n clonal</b> and <b>n subclonal</b> respectively.<br>
    </p>")
      }
    })
    
    # Makes it so that the output file does not appear until after the graphs are generated
    output$plotOutputArea <- renderUI({
      if (plotsReadyCliPP()) {
        fluidPage(
          h4("Output:"),
          textOutput("result"),
          selectInput("selectedFile", "Choose a file to download:", choices = c()),
          downloadButton("downloadResult", "Download Result")
        )
      }
    })
  })
  
  # Handlers for TCGA and PCAWG sections
  createSectionHandlers <- function(prefix) {
    # Define a separate reactive value for each prefix
    plotsReadyPrefix <- switch(prefix,
                               "TCGA" = plotsReadyTCGA,
                               "PCAWG" = plotsReadyPCAWG
    )
    
    observeEvent(input[[paste0("submit", prefix)]], {
      plotsReadyPrefix(FALSE)
      
      #selectedSample <- input[[paste0("selectedSample", prefix)]]
      selectedSample <- input[[paste0("sample_name_", prefix)]]
      sampleDir <- file.path(global_TCGA_PCAWG, prefix, selectedSample)
      
      if (is.null(selectedSample) || selectedSample == "") {
        showNotification("Please select a sample to analyze.", type = "error")
        return()
      }
      
      snvFile <- list.files(sampleDir, pattern = "*.snv.txt", full.names = TRUE)
      cnaFile <- list.files(sampleDir, pattern = "*.cna.txt", full.names = TRUE)
      purityFile <- list.files(sampleDir, pattern = "*.purity.txt", full.names = TRUE)
      
      
      if (length(snvFile) == 0 || length(cnaFile) == 0 || length(purityFile) == 0) {
        showNotification("One or more required files are missing.", type = "error")
        return()
      }
      
      file_data <- read_and_validate_files(snvFile[1], cnaFile[1], purityFile[1])
      
      if (is.null(file_data)) return()
      
      df_snv <- file_data$df_snv
      df_cna <- file_data$df_cna
      purity <- file_data$purity
      
      # Update the file selection dropdown
      updateSelectInput(session, paste0("selectedFile", prefix), choices = list.files(resultPath, pattern = "\\.txt$"))
      
      # Output message
      output[[paste0("result", prefix)]] <- renderText({
        "Processing complete. Select a file to download."
      })
      plotsReadyPrefix(TRUE)
      # Show the clear button only after submission
      shinyjs::show(paste0("clear", prefix))
    })
    
    observe({
      req(plotsReadyPrefix())
      
      # Plot 1
      output[[paste0("plot1", prefix)]] <- renderPlotly({
        # Ensure files are uploaded
        if (input[[paste0("submit", prefix)]] == 0 || !plotsReadyPrefix()) return()
        
        #selectedSample <- input[[paste0("selectedSample", prefix)]]
        selectedSample <- input[[paste0("sample_name_", prefix)]]
        sampleDir <- file.path(global_TCGA_PCAWG, prefix, selectedSample)
        
        if (is.null(selectedSample) || selectedSample == "") return()
        
        snvFile <- list.files(sampleDir, pattern = "*.snv.txt", full.names = TRUE)
        cnaFile <- list.files(sampleDir, pattern = "*.cna.txt", full.names = TRUE)
        purityFile <- list.files(sampleDir, pattern = "*.purity.txt", full.names = TRUE)
        
        if (length(snvFile) == 0 || length(cnaFile) == 0 || length(purityFile) == 0) return()
        
        file_data <- read_and_validate_files(snvFile[1], cnaFile[1], purityFile[1])
        if (is.null(file_data)) return()
        
        df_snv <- file_data$df_snv
        df_cna <- file_data$df_cna
        purity <- file_data$purity
        
        resultPath <- sampleDir
        
        # Read mutation and subclonal structure assignments
        matching_file1 <- list.files(resultPath, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
        matching_file2 <- list.files(resultPath, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)
        
        if (length(matching_file1) > 0 && length(matching_file2) > 0) {
          df_assigned <- read.table(matching_file1[1], header = TRUE, sep = "\t")
          df_subc <- read.table(matching_file2[1], header = TRUE, sep = "\t")
          
          df_snvcna <- df_snv %>%
            mutate(total_cn = map2_chr(position, chromosome_index, function(x, y) {
              inds = x >= df_cna$start_position & x <= df_cna$end_position & y == df_cna$chromosome_index
              if (any(inds)) as.character(df_cna$total_cn[which.max(inds)]) else NA
            }))
          
          # Calculate the sum of ref_count and alt_count
          df_snvcna$sum_counts <- df_snvcna$ref_count + df_snvcna$alt_count
          
          # Compute the mean of the sum_counts column
          mean_rd <- mean(df_snvcna$sum_counts)
          min_rd <- min(df_snvcna$sum_counts)
          max_rd <- max(df_snvcna$sum_counts)
          
          merged_df <- merge(df_snvcna, df_assigned, by = c("chromosome_index", "position"))
          merged_df['AF'] = merged_df['alt_count'] / (merged_df['sum_counts'])
          
          merged_df$total_cn <- as.numeric(merged_df$total_cn)
          merged_df$b_i_V <- pmax(1, round(merged_df$AF * (1 / purity) * ((purity * merged_df$total_cn) + (2 * (1 - purity))), 0))
          merged_df$CP_unpenalized <- (merged_df$alt_count * ((1 - purity) * 2 + purity * merged_df$total_cn)) / (merged_df$b_i_V * (merged_df$alt_count + merged_df$ref_count))
          merged_df$CCF_unpenalized <- merged_df$CP_unpenalized / purity
          
          total_SNVs <- sum(df_subc$num_SNV)
          # Smoothing is adjustable with slider
          smoothing_factor1 <- input[[paste0("smoothingFactor1_", prefix)]]
          
          merged_df <- merged_df %>%
            mutate(Subclonality = ifelse(cluster_index == 0, "Clonal", "Subclonal"),
                   ReadDepth = sum_counts)
          
          #print(head(merged_df))
          
          # Rename the count column to 'n'
          subclonality_counts <- merged_df %>%
            count(Subclonality) %>%
            rename(n = n)
          
          # Determine the color aesthetic based on the user input
          color_aesthetic <- if (input[[paste0("colorBy", prefix)]] == "clonality") "Subclonality" else "ReadDepth"
          
          # Create plot
          gg1 <- ggplot(merged_df, aes(x = AF)) +
            geom_density(fill = "gray", alpha = .6, adjust = smoothing_factor1) +
            suppressWarnings(geom_point(
              # Draw horizontal lines instead of points
              shape = 142,
              # Adjust size based on the figure's dimensions
              size = 5,
              # Transparency
              alpha = 0.3,
              # Fixed y-position and dynamic color
              aes(y = -0.75, color = color_aesthetic)
            )) +
            xlim(0, 1) +
            #ggtitle(paste(selectedSample, " - Purity =", round(purity, 3), "      ", "Mean read depth =", round(mean_rd,), "      ", "Range read depth =", min_rd, "-", max_rd)) +
            ggtitle(paste("Purity =", round(purity, 3), "      ", "Mean read depth =", round(mean_rd,), "      ", "Range read depth =", min_rd, "-", max_rd)) +
            geom_text(data = data.frame(), aes(label = paste0("n clonal = ", subclonality_counts %>% filter(Subclonality == "Clonal") %>% select(n)), x = .85, y = -1.65)) +
            geom_text(data = data.frame(), aes(label = paste0("n subclonal = ", subclonality_counts %>% filter(Subclonality == "Subclonal") %>% select(n)), x = .85, y = -2.65)) +
            ggpubr::theme_pubr() + xlab("VAF") + ylab("Density") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
          
          # , tooltip = c("x", "color", "text"))
          ggplotly(gg1)
        }
      })
      
      # Here is where the captions for graph 1 are stored
      output[[paste0("caption1", prefix)]] <- renderUI({
        if (plotsReadyPrefix()) {
          HTML("<p> - <b>Variant Allele Frequency (VAF)</b> indicates the proportion of sequencing reads that support a variant allele.<br>
           - <b>Purity</b> reflects the proportion of tumor cells in the sample. For instance, a purity of 0.9 means 90% of the cells are cancerous.<br>
           - <b>Mean read depth</b> is the average number of sequencing reads per SNV, influencing data reliability.<br>
           - <b>Density</b> estimates the probability density function of VAFs based on kernel density estimation (KDE). <br>
      </p>")
        }
      })
      
      # Plot 2
      output[[paste0("plot2", prefix)]] <- renderPlotly({
        if (input[[paste0("submit", prefix)]] == 0 || !plotsReadyPrefix()) return()
        
        #selectedSample <- input[[paste0("selectedSample", prefix)]]
        selectedSample <- input[[paste0("sample_name_", prefix)]]
        sampleDir <- file.path(global_TCGA_PCAWG, prefix, selectedSample)
        
        if (is.null(selectedSample) || selectedSample == "") return()
        
        snvFile <- list.files(sampleDir, pattern = "*.snv.txt", full.names = TRUE)
        cnaFile <- list.files(sampleDir, pattern = "*.cna.txt", full.names = TRUE)
        purityFile <- list.files(sampleDir, pattern = "*.purity.txt", full.names = TRUE)
        
        if (length(snvFile) == 0 || length(cnaFile) == 0 || length(purityFile) == 0) return()
        
        file_data <- read_and_validate_files(snvFile[1], cnaFile[1], purityFile[1])
        if (is.null(file_data)) return()
        
        df_snv <- file_data$df_snv
        df_cna <- file_data$df_cna
        purity <- file_data$purity
        
        resultPath <- sampleDir
        
        # Read mutation and subclonal structure assignments
        matching_file1 <- list.files(resultPath, pattern = "mutation_assignments_lam.*\\.txt", full.names = TRUE)
        matching_file2 <- list.files(resultPath, pattern = "subclonal_structure_lam.*\\.txt", full.names = TRUE)
        
        if (length(matching_file1) > 0 && length(matching_file2) > 0) {
          df_assigned <- read.table(matching_file1[1], header = TRUE, sep = "\t")
          df_subc <- read.table(matching_file2[1], header = TRUE, sep = "\t")
          
          df_snvcna <- df_snv %>%
            mutate(total_cn = map2_chr(position, chromosome_index, function(x, y) {
              inds = x >= df_cna$start_position & x <= df_cna$end_position & y == df_cna$chromosome_index
              if (any(inds)) as.character(df_cna$total_cn[which.max(inds)]) else NA
            }))
          
          # Calculate the sum of ref_count and alt_count
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
          
          total_SNVs <- sum(df_subc$num_SNV)
          # Smoothing is adjustable with slider
          smoothing_factor2 <- input[[paste0("smoothingFactor2_", prefix)]]
          
          merged_df <- merged_df %>%
            mutate(Subclonality = ifelse(cluster_index == 0, "Clonal", "Subclonal"),
                   ReadDepth = sum_counts)
          
          subclonality_counts <- merged_df %>%
            count(Subclonality) %>%
            rename(n = n)
          
          color_aesthetic <- if (input[[paste0("colorBy", prefix)]] == "clonality") "Subclonality" else "ReadDepth"
          
          #print(input[[paste0("colorBy", prefix)]])
          #print(summary(merged_df$CP_unpenalized))
          
          # Create plot
          gg2 <- ggplot(merged_df, aes(x = CP_unpenalized)) +
            geom_density(fill = "seagreen", alpha = .6, adjust = smoothing_factor2) +
            suppressWarnings(geom_point(
              shape = 142,
              size = 5,
              alpha = 0.3,
              aes(y = -0.75, color = color_aesthetic)
            )) +
            xlim(0, 1.5) +
            #ggtitle(paste(selectedSample, " - Estimated mutation-specific CP")) +
            ggtitle("Estimated mutation-specific CP") +
            geom_text(data = data.frame(), aes(label = paste0("n clonal = ", subclonality_counts %>% filter(Subclonality == "Clonal") %>% select(n)), x = 1.3, y = -1.65)) +
            geom_text(data = data.frame(), aes(label = paste0("n subclonal = ", subclonality_counts %>% filter(Subclonality == "Subclonal") %>% select(n)), x = 1.3, y = -2.65)) +
            ggpubr::theme_pubr() + xlab("Estimated mutation-specific CP") + ylab("Density") +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
            geom_segment(data = df_subc, aes(x = cellular_prevalence, xend = cellular_prevalence, y = 0, yend = 5.7), lty = "dashed")
          
          # , tooltip = c("x", "color", "text"))
          ggplotly(gg2)
        }
      })
      
      # Here is where the captions for graph 2 are stored
      output[[paste0("caption2", prefix)]] <- renderUI({
        if (plotsReadyPrefix()) {
          HTML("<p> - <b>Cellular Prevalence (CP)</b> the fraction of all cells (both tumor and admixed normal cells) from the sequenced tissue carrying a particular SNV.<br>
           - Annotations on the x-axis indicate whether mutations are <b>clonal</b> (present in all tumor cells) or <b>subclonal</b> (found in a subset).<br>
           - Counts of clonal and subclonal mutations are indicated as <b>n clonal</b> and <b>n subclonal</b> respectively.<br>
      </p>")
        }
      })
      
      # Makes it so that the output file does not appear until after the graphs are generated
      output[[paste0("plotOutputArea", prefix)]] <- renderUI({
        if (plotsReadyPrefix()) {
          fluidPage(
            h4("Output:"),
            textOutput(paste0("result", prefix)),
            selectInput(paste0("selectedFile", prefix), "Choose a file to download:", choices = c()),
            downloadButton(paste0("downloadResult", prefix), "Download Result")
          )
        } else {
          shinyjs::hide(paste0("plotOutputArea", prefix))
        }
      })
    })
    
    output[[paste0("plotReady", prefix)]] <- reactive({ plotsReadyPrefix() })
    outputOptions(output, paste0("plotReady", prefix), suspendWhenHidden = FALSE)
    
    output[[paste0("plotReadyText", prefix)]] <- renderUI({
      if (plotsReadyPrefix()) {
        div(id = paste0("plotReadyText", prefix), "true")
      } else {
        div(id = paste0("plotReadyText", prefix), "false")
      }
    })
  }
  
  createSectionHandlers("TCGA")
  createSectionHandlers("PCAWG")
  
  # List files in their respective folders and subsequently update the dropdown
  list_files <- function(path) {
    list.files(path = path, full.names = TRUE)
  }
  
  observe({
    files <- list.files(file.path(global_TCGA_PCAWG, "TCGA"), full.names = FALSE, recursive = FALSE)
    updateSelectizeInput(session, "selectedSampleTCGA", choices = files, server = TRUE)
  })
  
  observe({
    files <- list.files(file.path(global_TCGA_PCAWG, "PCAWG"), full.names = FALSE, recursive = FALSE)
    updateSelectizeInput(session, "selectedSamplePCAWG", choices = files, server = TRUE)
  })
  
  # Generate cancer type based on selected cancer type (for TCGA portion)
  output$cancer_type_ui_TCGA <- renderUI({
    selectInput("cancer_type_TCGA", "Select Cancer Type:", choices = unique(sample_TCGA$cancer_type))
  })
  
  # Generate sample name based on selected cancer type (for TCGA portion)
  output$sample_name_ui_TCGA <- renderUI({
    selectInput("sample_name_TCGA", "Select Sample Name:", choices = NULL)
  })
  
  # Update sample name based on selected cancer type (for TCGA portion)
  observeEvent(input$cancer_type_TCGA, {
    selected_cancer_type <- input$cancer_type_TCGA
    sample_names <- sample_TCGA$samplename[sample_TCGA$cancer_type == selected_cancer_type]
    updateSelectInput(session, "sample_name_TCGA", choices = sample_names)
  })
  
  # Generate cancer type based on selected dataset (for PCAWG portion)
  output$cancer_type_ui_PCAWG <- renderUI({
    selectInput("cancer_type_PCAWG", "Select Cancer Type:", choices = unique(sample_PCAWG$cancer_type))
  })
  
  # Generate sample name based on selected dataset (for PCAWG portion)
  output$sample_name_ui_PCAWG <- renderUI({
    selectInput("sample_name_PCAWG", "Select Sample Name:", choices = NULL)
  })
  
  # Update sample name based on selected cancer type (for PCAWG portion)
  observeEvent(input$cancer_type_PCAWG, {
    selected_cancer_type <- input$cancer_type_PCAWG
    sample_names <- sample_PCAWG$samplename[sample_PCAWG$cancer_type == selected_cancer_type]
    updateSelectInput(session, "sample_name_PCAWG", choices = sample_names)
  })
  
  # Generate cancer type based on selected dataset (for driver mutation portion)
  output$cancer_type_ui_driver <- renderUI({
    selectInput("cancer_type_driver", "Select Cancer Type:", choices = unique(driver_mutation_data$cancer))
  })
  
  # Generate sample name based on selected dataset (for driver mutation portion)
  output$sample_name_ui_driver <- renderUI({
    selectInput("sample_name_driver", "Select Sample Name:", choices = NULL)
  })
  
  # Update sample name based on selected cancer type (for driver mutation portion)
  observeEvent(input$cancer_type_driver, {
    selected_cancer_type <- input$cancer_type_driver
    sample_names <- driver_mutation_data$sample[driver_mutation_data$cancer == selected_cancer_type]
    updateSelectInput(session, "sample_name_driver", choices = sample_names)
  })
  
  # Make clonal red and subclonal blue
  observeEvent(input$submitDriver, {
    selected_sample <- input$sample_name_driver
    selected_data <- driver_mutation_data %>% filter(sample == selected_sample)
    output$driverMutationTable <- renderTable({
      selected_data %>% select(gene, protein, consequence, cancer, Subclonality, VAF, CP_unpenalized)
    }, rownames = TRUE, options = list(
      rowCallback = JS(
        "function(row, data, index) {",
        "  if (data[4] === 'Clonal') {",
        "    $('td', row).css('background-color', '#FFCCCB');",
        "  } else if (data[4] === 'Subclonal') {",
        "    $('td', row).css('background-color', '#ADD8E6');",
        "  }",
        "}"
      )
    ))
  })
}
