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

driver_output_cols <- function(df) {
  keep <- c(
    tcga_id        = "tcga_id",
    gene           = "Hugo_Symbol",
    chromosome     = "chromosome",
    position       = "position",
    cancer_type    = "cancer_type",
    protein_change = "HGVSp_Short",
    clonality      = "Subclonality",
    cluster_index  = "cluster_index",
    VAF            = "VAF",
    CP_unpenalized = "CP_unpenalized",
    consequence    = "Consequence",
    t_depth        = "t_depth",
    purity         = "purity"
  )
  available <- keep[keep %in% names(df)]
  out <- df[, available, drop = FALSE]
  names(out) <- names(available)
  out
}

clipp_result_files <- function(run_dir = "forCliPP/CliPP/sample_id") {
  files <- list.files(file.path(run_dir, "final_result", "Best_lambda"), pattern = "\\.txt$")
  excluded <- file.path(run_dir, "preprocess_result", "excluded_SNVs.txt")
  if (file.exists(excluded)) files <- c(files, "excluded_SNVs.txt")
  files
}

#' Path of a downloadable result file; excluded_SNVs.txt lives with the
#' preprocessing output, everything else in Best_lambda.
clipp_result_path <- function(file_name, run_dir = "forCliPP/CliPP/sample_id") {
  if (identical(file_name, "excluded_SNVs.txt"))
    file.path(run_dir, "preprocess_result", "excluded_SNVs.txt")
  else
    file.path(run_dir, "final_result", "Best_lambda", file_name)
}

server <- function(input, output, session) {

  observeEvent(input$main_nav_target, {
    updateTabsetPanel(session, "main_nav", selected = input$main_nav_target)
  })

  plots_ready_clipp <- reactiveVal(FALSE)

  # One entry per uploaded sample: status, its CliPP output directory, the
  # silhouette result and the merged analysis the plots draw from.
  runs          <- reactiveVal(list())
  active_sample <- reactiveVal(NULL)

  active_run <- reactive({
    nm <- active_sample()
    if (is.null(nm)) return(NULL)
    runs()[[nm]]
  })

  tab_states <- reactiveValues(
    clipp_web = FALSE
  )

  output$plotsReadyCliPP <- reactive({
    value <- plots_ready_clipp()
    return(value)
  })
  outputOptions(output, "plotsReadyCliPP", suspendWhenHidden = FALSE)

  output$samplesUploaded <- reactive({
    samples_uploaded()
  })
  outputOptions(output, "samplesUploaded", suspendWhenHidden = FALSE)

  samples_uploaded <- reactiveVal(FALSE)
  uploaded_samples <- reactiveValues(samples = list())

  observe({
    shinyjs::toggleState("detailedView", condition = (input$colorBy %||% "clonality") == "clonality")
  })

  # Bulk mode: one picker for every file. The samples, their names and how many
  # there are all come from the filenames -- nothing is declared up front.
  bulk_grouping <- reactiveVal(NULL)

  observeEvent(input$uploadMode, {
    if (identical(input$uploadMode, "all_at_once")) {
      shinyjs::show("bulkUploadBlock")
      shinyjs::hide("individualUploadBlock")
      shinyjs::toggle("uploadSamples",
                      condition = length((bulk_grouping() %||% list())$samples) > 0)
    } else {
      shinyjs::hide("bulkUploadBlock")
      shinyjs::show("individualUploadBlock")
      shinyjs::hide("uploadSamples")
    }
  }, ignoreInit = FALSE)

  observeEvent(input$bulkFiles, {
    files <- input$bulkFiles
    grouping <- group_uploaded_files(files$name, files$datapath)
    bulk_grouping(grouping)

    output$bulkSummary <- renderUI({
      n <- length(grouping$samples)
      div(class = if (n > 0) "bulk-summary bulk-summary-ok" else "bulk-summary bulk-summary-warn",
          bulk_upload_summary(grouping))
    })

    if (length(grouping$samples) > 0) {
      shinyjs::show("uploadSamples")
    } else {
      shinyjs::hide("uploadSamples")
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
    samples <- if (identical(input$uploadMode, "all_at_once")) {
      (bulk_grouping() %||% list(samples = list()))$samples
    } else {
      store_uploaded_files(input, input$numSamples, input$uploadMode)
    }

    if (length(samples) == 0) {
      showNotification("No complete samples to upload yet.", type = "error")
      return()
    }

    uploaded_samples$samples <- samples
    samples_uploaded(TRUE)

    showNotification(sprintf("%d sample%s ready: %s", length(samples),
                             if (length(samples) == 1) "" else "s",
                             paste(names(samples), collapse = ", ")),
                     type = "message")
    shinyjs::hide("uploadSamples")
    shinyjs::hide("bulkUploadBlock")
    shinyjs::hide("individualUploadBlock")
    shinyjs::show("runCard")
    shinyjs::show("submit")
    shinyjs::show("colorBy")
    shinyjs::show("detailedView")
    shinyjs::show("cpScale")
  })

  observeEvent(input$clear, {
    uploaded_samples$samples <- list()
    samples_uploaded(FALSE)
    runs(list())
    active_sample(NULL)
    bulk_grouping(NULL)
    shinyjs::reset("bulkFiles")
    output$bulkSummary <- renderUI({ NULL })
    if (identical(input$uploadMode, "all_at_once")) {
      shinyjs::show("bulkUploadBlock")
    } else {
      shinyjs::show("individualUploadBlock")
    }
    updateNumericInput(session, "numSamples", value = 1)
    shinyjs::enable("numSamples")

    output$dynamicFileInputs <- renderUI({
      NULL
    })

    elements_to_hide <- c(
      "submit", "colorBy", "detailedView", "cpScale", "clear",
      "silhouetteScore", "runCard", "displayCard",
      "plot1", "plot2", "caption1", "caption2",
      "smoothingFactor1", "smoothingFactor2",
      "smoothingExplanation1", "smoothingExplanation2",
      "uploadSamples"
    )

    for (element in elements_to_hide) {
      shinyjs::hide(element)
    }

    shinyjs::show("setSamples")
    shinyjs::show("resultsEmpty")

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
  })

  # ------------------------------------------------------------------------
  # Run every uploaded sample, then present them as tabs.
  #
  # Each sample gets its own CliPP output directory (forCliPP/CliPP/<slug>/),
  # so a batch no longer overwrites itself, and its analysis is computed once
  # here rather than recomputed on every tab switch.
  # ------------------------------------------------------------------------
  observeEvent(input$activeSample, {
    if (!is.null(input$activeSample) && input$activeSample %in% names(runs())) {
      active_sample(input$activeSample)
    }
  })

  output$runSummary <- renderUI({
    n <- length(uploaded_samples$samples)
    if (n == 0) return(NULL)
    p(class = "run-summary",
      sprintf("%d sample%s ready: %s", n, if (n == 1) "" else "s",
              paste(names(uploaded_samples$samples), collapse = ", ")))
  })

  observeEvent(input$submit, {
    samples <- uploaded_samples$samples
    if (length(samples) == 0) {
      showNotification("Upload at least one sample first.", type = "error")
      return()
    }

    plots_ready_clipp(FALSE)
    tab_states$clipp_web <- FALSE
    results <- list()

    withProgress(message = "Running CliPP", value = 0, {
      for (i in seq_along(samples)) {
        nm <- names(samples)[i]
        setProgress(value = (i - 1) / length(samples),
                    detail = sprintf("%s (%d of %d)", nm, i, length(samples)))

        pr <- process_sample_files(samples[[nm]], nm)

        results[[nm]] <- if (isTRUE(pr$success)) {
          list(status     = "ok",
               run_dir    = pr$run_dir,
               silhouette = compute_silhouette_score(pr$run_dir),
               analysis   = get_current_analysis(samples[[nm]], pr$run_dir))
        } else {
          list(status         = "failed",
               message        = pr$message,
               too_large      = isTRUE(pr$too_large),
               n_snvs         = pr$n_snvs,
               kernel_missing = isTRUE(pr$kernel_missing))
        }
      }
      setProgress(value = 1)
    })

    runs(results)
    ok <- names(Filter(function(r) identical(r$status, "ok"), results))
    active_sample(if (length(ok)) ok[[1]] else names(results)[[1]])

    # One notification per kind of failure, not one per sample.
    failed <- Filter(function(r) !identical(r$status, "ok"), results)
    if (length(failed)) {
      big <- names(Filter(function(r) isTRUE(r$too_large), failed))
      if (length(big)) {
        showNotification(
          ui = HTML(sprintf(
            paste0("<b>Too large for CliPP-on-Web:</b> %s<br>The limit is %s SNVs. ",
                   "Run these with the command-line version of CliPP: ",
                   "<a href='%s' target='_blank' rel='noopener'>%s</a>"),
            paste(big, collapse = ", "), format(MAX_SNVS_WEB, big.mark = ","),
            CLIPP_GITHUB, CLIPP_GITHUB)),
          type = "error", duration = NULL)
      }
      kernel <- Filter(function(r) isTRUE(r$kernel_missing), failed)
      if (length(kernel)) {
        warning(immediate. = TRUE, kernel[[1]]$message)
        showNotification(kernel[[1]]$message, type = "error", duration = NULL)
      }
      other <- Filter(function(r) !isTRUE(r$too_large) && !isTRUE(r$kernel_missing), failed)
      if (length(other)) {
        showNotification(sprintf("%s: %s", names(other)[1], other[[1]]$message),
                         type = "error")
      }
    }

    if (length(ok)) {
      showNotification(sprintf("Finished %d of %d sample%s.", length(ok), length(results),
                               if (length(results) == 1) "" else "s"),
                       type = "message")
    }

    shinyjs::show("clear")
    shinyjs::show("displayCard")
    shinyjs::hide("resultsEmpty")
    plots_ready_clipp(length(ok) > 0)
    tab_states$clipp_web <- length(ok) > 0
  })

  # ---- the tab strip across the top of the results card --------------------
  output$sampleTabs <- renderUI({
    rs <- runs()
    if (length(rs) == 0) return(NULL)
    act <- active_sample()
    div(class = "sample-tabs",
      lapply(names(rs), function(nm) {
        failed <- !identical(rs[[nm]]$status, "ok")
        tags$button(
          type = "button",
          class = paste("sample-tab",
                        if (identical(nm, act)) "active" else "",
                        if (failed) "failed" else ""),
          onclick = sprintf("Shiny.setInputValue('activeSample', %s, {priority: 'event'})",
                            jsonlite::toJSON(nm, auto_unbox = TRUE)),
          nm,
          if (failed) span(class = "sample-tab-flag", title = "This sample did not finish", "!")
        )
      })
    )
  })

  # Each sample has its own ideal smoothing, so the sliders follow the active
  # tab rather than sitting at a fixed default that suits nobody. Move a slider
  # by hand and it stays put until you switch samples.
  observeEvent(active_sample(), {
    run <- active_run()
    if (is.null(run) || !identical(run$status, "ok") || is.null(run$analysis)) return()
    rec <- recommended_smoothing_factor(run$analysis$total_mutations)
    updateSliderInput(session, "smoothingFactor1", value = rec)
    updateSliderInput(session, "smoothingFactor2", value = rec)
  }, ignoreNULL = TRUE)

  # ---- everything below renders whichever tab is active --------------------
  output$silhouetteScore <- renderUI({
    run <- active_run()
    if (is.null(run)) return(NULL)

    if (!identical(run$status, "ok")) {
      return(div(class = "run-failed",
                 tags$b(active_sample()), " did not finish. ", run$message))
    }

    # Low purity or low nrpcc means the clusters are being fitted to noise.
    # Say so above the numbers, where it cannot be missed.
    q <- run$analysis$quality
    quality_note <- if (!is.null(q) && isTRUE(q$flagged)) {
      reasons <- c(
        if (isTRUE(q$low_purity)) sprintf("purity is %.2f, below %.1f", q$purity, MIN_PURITY),
        if (isTRUE(q$low_nrpcc))  sprintf("nrpcc is %.1f, below %d", q$nrpcc, MIN_NRPCC)
      )
      div(class = "quality-warning",
        div(class = "quality-warning-title", "Interpret this sample with caution"),
        div(class = "quality-warning-body",
          sprintf("This sample's %s. ", paste(reasons, collapse = " and ")),
          "There is little clean signal to cluster: subclones may be missed, and ",
          "apparent clusters may be noise. nrpcc is the number of reads per clonal ",
          "copy, mean depth scaled by purity and divided by ploidy."
        )
      )
    } else {
      NULL
    }

    silhouette <- run$silhouette
    stat <- function(label, value) {
      div(class = "stat",
          div(class = "stat-label", label),
          div(class = "stat-value", value))
    }
    fmt_n <- function(x) if (is.null(x) || is.na(x)) "\u2014" else format(x, big.mark = ",")
    score <- switch(silhouette$status,
      ok                = sprintf("%.3f", silhouette$sil_full),
      single_cluster    = "single cluster",
      too_few_mutations = "not calculated",
      "not available"
    )
    note <- switch(silhouette$status,
      single_cluster    = "A silhouette score is not defined for a single cluster.",
      too_few_mutations = "Too few mutations to score this run.",
      "Mean silhouette width of per-mutation cancer cell fraction under CliPP's cluster assignments. Ranges from -1 to 1; higher values indicate better-separated clusters."
    )
    tagList(
      quality_note,
      div(class = "silhouette-stats",
        stat("SILHOUETTE SCORE", score),
        stat("CLUSTERS", fmt_n(silhouette$n_clusters)),
        stat("MUTATIONS", fmt_n(silhouette$n_mutations))
      ),
      p(class = "silhouette-note", note)
    )
  })

  # A failed tab shows its error instead of stale plots from another sample.
  observe({
    run <- active_run()
    ok  <- !is.null(run) && identical(run$status, "ok")
    if (!is.null(run)) shinyjs::show("silhouetteScore")
    for (el in c("plot1", "plot2", "caption1", "caption2", "plotOutputArea")) {
      if (ok) shinyjs::show(el) else shinyjs::hide(el)
    }
    shinyjs::toggle(selector = ".controls-area", condition = ok)
  })

  output$plot1 <- renderPlotly({
    run <- active_run()
    req(run, identical(run$status, "ok"), run$analysis)

    create_vaf_plot(
      merged_df = run$analysis$merged_data,
      sample_name = active_sample(),
      purity = run$analysis$purity,
      read_depth = run$analysis$read_depth,
      smoothing_factor = input$smoothingFactor1 %||% SMOOTHING_DEFAULT,
      color_by = input$colorBy %||% "clonality",
      subclonality_counts = run$analysis$subclonality_counts,
      detailed = isTRUE(input$detailedView)
    )
  })

  output$plot2 <- renderPlotly({
    run <- active_run()
    req(run, identical(run$status, "ok"), run$analysis)

    create_cp_plot(
      merged_df = run$analysis$merged_data,
      sample_name = active_sample(),
      smoothing_factor = input$smoothingFactor2 %||% SMOOTHING_DEFAULT,
      color_by = input$colorBy %||% "clonality",
      subclonality_counts = run$analysis$subclonality_counts,
      df_subc = run$analysis$subclonal_structure,
      detailed = isTRUE(input$detailedView),
      scale = input$cpScale %||% "CP",
      purity = run$analysis$purity
    )
  })

  output$caption1 <- renderUI({
    run <- active_run()
    req(run, identical(run$status, "ok"))
    get_vaf_plot_caption(detailed = isTRUE(input$detailedView))
  })

  output$caption2 <- renderUI({
    run <- active_run()
    req(run, identical(run$status, "ok"))
    get_cp_plot_caption(scale = input$cpScale %||% "CP", detailed = isTRUE(input$detailedView))
  })

  # Smoothing advice follows the active sample's mutation count.
  output$smoothingExplanation1 <- renderUI({
    run <- active_run()
    req(run, identical(run$status, "ok"), run$analysis)
    HTML(paste0("Recommended for this sample: ",
                recommended_smoothing_factor(run$analysis$total_mutations)))
  })

  output$smoothingExplanation2 <- renderUI({
    run <- active_run()
    req(run, identical(run$status, "ok"), run$analysis)
    HTML(paste0("Recommended for this sample: ",
                recommended_smoothing_factor(run$analysis$total_mutations)))
  })

  output$plotOutputArea <- renderUI({
    run <- active_run()
    req(run, identical(run$status, "ok"))

    result_files <- clipp_result_files(run$run_dir)

    div(class = "output-row",
      div(class = "output-label", "Result files"),
      div(class = "output-controls",
        selectInput("selectedFile", NULL, choices = result_files, width = "320px"),
        downloadButton("downloadResult", "Download", class = "btn-download")
      )
    )
  })

  output$downloadResult <- downloadHandler(
    filename = function() {
      selected_file <- input$selectedFile
      nm <- active_sample() %||% "sample"
      paste0(clipp_run_slug(nm), "_",
             if (grepl("[.]txt$", selected_file)) selected_file else paste0(selected_file, ".txt"))
    },
    content = function(file) {
      run <- active_run()
      req(run, identical(run$status, "ok"))
      file_path <- clipp_result_path(input$selectedFile, run$run_dir)

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
      "smoothingExplanation1", "smoothingExplanation2"
    )

    for (control in controls) {
      shinyjs::hide(control)
    }

    if (current_tab == "Run CliPP") {
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
    }
  })


  output$downloadSim4k <- downloadHandler(
    filename = function() "CliPPSim4k_inputs.zip",
    content  = function(file) {
      src <- file.path(global_TCGA_PCAWG, "CliPPSim4k_inputs.zip")
      if (file.exists(src)) file.copy(src, file)
      else writeLines("Simulation input data not available.", file)
    },
    contentType = "application/zip"
  )

  output$browseSim4k <- downloadHandler(
    filename = function() "CliPPSim4k_CliPP1.4_results.zip",
    content  = function(file) {
      src <- file.path(global_TCGA_PCAWG, "CliPPSim4k_CliPP1.4_results.zip")
      if (file.exists(src)) file.copy(src, file)
      else writeLines("Simulation results not available.", file)
    },
    contentType = "application/zip"
  )

  output$browseTCGA <- downloadHandler(
    filename = function() "TCGA_CliPP1.4_results.zip",
    content  = function(file) {
      src <- file.path(global_TCGA_PCAWG, "TCGA_CliPP1.4_results.zip")
      if (file.exists(src)) file.copy(src, file)
      else writeLines("TCGA zip not available.", file)
    },
    contentType = "application/zip"
  )

  output$browsePCAWG <- downloadHandler(
    filename = function() "PCAWG_CliPP1.4_results.zip",
    content  = function(file) {
      src <- file.path(global_TCGA_PCAWG, "PCAWG_CliPP1.4_results.zip")
      if (file.exists(src)) file.copy(src, file)
      else writeLines("PCAWG zip not available.", file)
    },
    contentType = "application/zip"
  )

  # The sample list is the only place the cohort archives' directory names are
  # tied to a cancer type and a purity; it is also inside the zip, but the zip
  # is 43/174 MB and the mapping is 150 KB.
  output$downloadTCGA_samples <- downloadHandler(
    filename = function() "TCGA_samples.tsv",
    content  = function(file) {
      serve_dataSource_file(file.path("TCGA", "samples.tsv"), file, "TCGA sample list")
    },
    contentType = "text/tab-separated-values"
  )

  # The sample list is the only place the cohort archives' directory names are
  # tied to a cancer type and a purity; it is also inside the zip, but the zip
  # is 43/174 MB and the mapping is 150 KB.
  output$downloadPCAWG_samples <- downloadHandler(
    filename = function() "PCAWG_samples.tsv",
    content  = function(file) {
      serve_dataSource_file(file.path("PCAWG", "samples.tsv"), file, "PCAWG sample list")
    },
    contentType = "text/tab-separated-values"
  )

  output$downloadTCGA_cancer <- downloadHandler(
    filename = function() {
      paste0("TCGA_", input$tcga_cancer_type_dl, "_CliPP1.4_results.tsv")
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
      paste0("PCAWG_", input$pcawg_cancer_type_dl, "_CliPP1.4_results.tsv")
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
      "TCGA_driver_mutations_CliPP1.4.csv"
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
      paste0("TCGA_driver_mutations_", input$driver_cancer_type_dl, "_CliPP1.4.csv")
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

  # --- Minimum reproducibility tables (TCGA) --------------------------------
  # The smallest set of columns that recreates the manuscript's driver figures.
  # Served verbatim: these are the analysis outputs, not a reformatted view, so
  # driver_output_cols() is deliberately NOT applied (it would drop columns the
  # figures need and rename the rest).
  serve_dataSource_file <- function(name, file, label) {
    tryCatch({
      src <- file.path(global_TCGA_PCAWG, name)
      if (!file.exists(src)) {
        writeLines(paste0(label, " not found."), file)
        return()
      }
      file.copy(src, file, overwrite = TRUE)
    }, error = function(e) {
      writeLines(paste("Error serving file:", e$message), file)
    })
  }

  output$downloadDriverReproSamples <- downloadHandler(
    filename = function() {
      "TCGA_driver_cohort_table_CliPP1.4.tsv"
    },
    content = function(file) {
      serve_dataSource_file("TCGA_driver_reproducibility_samples.tsv", file,
                            "Cohort sample table")
    },
    contentType = "text/tab-separated-values"
  )

}
