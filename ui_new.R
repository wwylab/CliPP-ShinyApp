library(shiny)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(DT)

# ============================================================================
# UI
# ============================================================================

sidebar_link <- function(value, icon_name, label, badge = NULL, active = FALSE) {
  tags$a(
    href = "#",
    class = paste("sidebar-link", if (active) "active" else ""),
    `data-tab` = value,
    tags$span(class = "sidebar-icon", icon(icon_name)),
    tags$span(class = "sidebar-label", label),
    if (!is.null(badge)) tags$span(class = "sidebar-badge", badge)
  )
}

chapter_item <- function(time_str, time_sec, title, subtitle, active = FALSE) {
  tags$a(
    href = "#",
    class = paste("chapter-row", if (active) "active" else ""),
    `data-time` = time_sec,
    onclick = "seekTutorial(this); return false;",
    span(class = "chapter-time", time_str),
    div(class = "chapter-text",
      div(class = "chapter-title", title),
      div(class = "chapter-sub", subtitle)
    )
  )
}

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#1f2937",
    primary = "#2563eb",
    secondary = "#7c3aed",
    success = "#047857",
    font_scale = 1.0,
    bootswatch = "flatly"
  ),

  useShinyjs(),
  tags$head(
    includeHTML("google-analytics.html"),
    tags$link(rel = "stylesheet", type = "text/css", href = paste0(global_assets, "/css/custom.css")),
    tags$script(src = paste0(global_assets, "/js/custom.js")),
    tags$style(HTML("
      #progress {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 50%;
        z-index: 9999;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        if (window.pageHeaders) {
          window.pageHeaders['Data Exploration'] = ['Data Exploration', 'Kaplan-Meier survival analysis and sMF correlation heatmaps'];  // staging-only
          window.pageHeaders['Phylogenetic Tree'] = ['Phylogenetic Tree', 'Tumor evolutionary history from CliPP subclonal clusters'];
        }
      });
    "))
  ),

  div(class = "app-shell",

    # ============================ Sidebar ============================
    tags$aside(class = "app-sidebar",
      div(class = "sidebar-brand",
        tags$img(src = paste0(global_assets, "/clipp-logo.png"), class = "brand-logo", alt = "CliPP")
      ),
      div(class = "sidebar-section-label", "SECTIONS"),
      tags$nav(class = "sidebar-nav",
        sidebar_link("About",                "circle-info",    "About",                active = TRUE),
        sidebar_link("Tutorial",             "circle-play",    "Tutorial"),
        sidebar_link("CliPP-on-Web",          "layer-group",    "CliPP-on-Web"),
        sidebar_link("CliPP Data Resources", "database",       "CliPP Data Resources"),
        sidebar_link("Driver Mutation",      "dna",            "Driver Mutation"),
        sidebar_link("Data Exploration",     "chart-line",     "Data Exploration"),
        sidebar_link("Phylogenetic Tree",    "diagram-project","Phylogenetic Tree"),
        sidebar_link("Downloads",            "download",       "Downloads")
      )
    ),

    # ============================ Main area ============================
    div(class = "app-main",
      div(class = "app-topbar",
        div(class = "breadcrumb-trail",
          span(class = "breadcrumb-current", "CliPP")
        )
      ),
      div(class = "app-page-area",
        div(class = "page-header-block",
          h1(class = "page-title", "CliPP"),
          p(class = "page-subtitle", "Clonal Structure Identification through Pairwise Penalization")
        ),
        div(class = "app-content",
          tabsetPanel(id = "main_nav", type = "hidden", selected = "About",

            # ----------------------------------------------------------------
            # About
            # ----------------------------------------------------------------
            tabPanel("About",
                     div(class = "about-page fade-in",
                       fluidRow(
                         column(8,
                           div(class = "about-card",
                             div(class = "about-card-header",
                               h4(class = "about-card-title", "Description"),
                               span(class = "about-card-meta", "Last updated · May 7, 2026")
                             ),
                             div(class = "about-card-body",
                               p("Tumor subclonal architecture shapes cancer evolution, yet subclonal reconstruction from bulk sequencing remains difficult to scale due to computational cost and model complexity. We developed ",
                                 tags$b("Clonal Structure Identification through Pairwise Penalization (CliPP)"),
                                 ", a penalized-likelihood framework that jointly estimates cellular prevalence with pairwise fusion penalties, automatically identifying subclones without requiring extensive priors, which enables fast and accurate subclonal reconstruction."),
                               p("Using CliPP on Web, users may upload their own samples for fast subclonal reconstruction using our easy to navigate user interface. We also present the subclonal reconstruction results for 7,279 TCGA samples and 2,778 PCAWG samples.")
                             )
                           )
                         ),
                         column(4,
                           div(class = "about-card contacts-card",
                             div(class = "about-card-header",
                               h4(class = "about-card-title", "Issue contacts")
                             ),
                             div(class = "about-card-body contacts-list",
                               div(class = "contact-item",
                                 div(class = "contact-avatar avatar-blue", "AW"),
                                 div(class = "contact-info",
                                   div(class = "contact-name", "Aaron Wu"),
                                   div(class = "contact-role", "Web application · Engineering"),
                                   a(href = "mailto:pandachessaaron@gmail.com", class = "contact-email", "pandachessaaron@gmail.com")
                                 )
                               ),
                               div(class = "contact-item",
                                 div(class = "contact-avatar avatar-purple", "MM"),
                                 div(class = "contact-info",
                                   div(class = "contact-name", "Matthew Montierth"),
                                   div(class = "contact-role", "Pipeline support · MD Anderson"),
                                   a(href = "mailto:MDMontierth@mdanderson.org", class = "contact-email", "MDMontierth@mdanderson.org")
                                 )
                               )
                             )
                           )
                         )
                       ),
                       div(class = "about-card pipeline-card",
                         div(class = "about-card-header",
                           h4(class = "about-card-title", "How CliPP works"),
                           span(class = "about-card-meta", "Pipeline overview")
                         ),
                         div(class = "about-card-body",
                           div(class = "pipeline-steps",
                             div(class = "pipeline-step",
                               div(class = "step-number", "1"),
                               div(class = "step-title", "Input"),
                               div(class = "step-desc", "WGS / WES variant calls with read depth, allele counts, and copy-number segments.")
                             ),
                             div(class = "pipeline-step",
                               div(class = "step-number", "2"),
                               div(class = "step-title", "Penalized model"),
                               div(class = "step-desc", "Regularized likelihood clusters mutations by penalizing pairwise CCF differences.")
                             ),
                             div(class = "pipeline-step",
                               div(class = "step-number", "3"),
                               div(class = "step-title", "Subclonal reconstruction"),
                               div(class = "step-desc", "Recovers clonal & subclonal populations in minutes per sample.")
                             ),
                             div(class = "pipeline-step",
                               div(class = "step-number", "4"),
                               div(class = "step-title", "Output"),
                               div(class = "step-desc", "Subclonal mutation fraction (sMF), clonal structure, cluster assignments.")
                             )
                           )
                         )
                       ),
                       # ---- Citations ----
                       fluidRow(
                         column(8,
                           div(class = "about-card",
                             div(class = "about-card-header",
                               h4(class = "about-card-title", "Citation")
                             ),
                             div(class = "about-card-body",
                               div(class = "preprint-box",
                                 span(class = "preprint-label", "PRE-PRINT · BIORXIV"),
                                 p(class = "preprint-citation",
                                   "Jiang, Y., Montierth, M.D., Ding, Y., Yu, K., Tran, Q., Wu, A., Li, R., Ji, S., Liu, X., Shin, S.J., Cao, S., Tang, Y., Lesluyes, T., Kimmel, M., Wang, J.R., Tarabichi, M., Zhu, H., Van Loo, P., & Wang, W. ",
                                   tags$em("Scalable subclonal reconstruction of cancer cells in DNA sequencing data using a penalized likelihood model."),
                                   " bioRxiv (2021)."
                                 ),
                                 a(href = "https://www.biorxiv.org/content/10.1101/2021.03.31.437383v2",
                                   class = "preprint-link",
                                   target = "_blank",
                                   "https://www.biorxiv.org/content/10.1101/2021.03.31.437383v2")
                               ),
                               div(class = "format-label", style = "margin-top:1.5rem;", "BIBTEX"),
                               tags$pre(class = "bibtex-block", id = "bibtex-content",
"@article{jiang2021clipp,
  title   = {Scalable subclonal reconstruction of cancer cells in DNA sequencing data using a penalized likelihood model},
  author  = {Jiang, Yujie and Montierth, Matthew D and Ding, Y and Yu, Kaixian and Tran, Quang and Wu, Aaron and Li, Ruonan and Ji, Shuangxi and Liu, Xiaoqian and Shin, Seung Jun and Cao, Shaolong and Tang, Yuxin and Lesluyes, Tom and Kimmel, Marek and Wang, Jennifer R and Tarabichi, Maxime and Zhu, Hongtu and Van Loo, Peter and Wang, Wenyi},
  journal = {bioRxiv},
  year    = {2021},
  doi     = {10.1101/2021.03.31.437383}
}"
                               ),
                               div(class = "cite-actions",
                                 tags$button(type = "button", class = "btn-mini",
                                   onclick = "copyBibtex(); return false;",
                                   "Copy BibTeX"),
                                 tags$button(type = "button", class = "btn-mini",
                                   onclick = "downloadCitation('ris'); return false;",
                                   "Download .ris"),
                                 tags$button(type = "button", class = "btn-mini",
                                   onclick = "downloadCitation('enw'); return false;",
                                   "EndNote (.enw)")
                               )
                             )
                           )
                         ),
                         column(4,
                           div(class = "about-card",
                             div(class = "about-card-header",
                               h4(class = "about-card-title", "Data Usage")
                             ),
                             div(class = "about-card-body",
                               p(class = "data-usage-intro",
                                 "If you use this data in your research, please cite:"),
                               div(class = "citation-numbered-list",
                                 div(class = "numbered-item",
                                   div(class = "num-circle num-blue", "1"),
                                   div(class = "num-text", "CliPP methodology and software publication")
                                 ),
                                 div(class = "numbered-item",
                                   div(class = "num-circle num-purple", "2"),
                                   div(class = "num-text", "PCAWG Consortium publications for PCAWG data")
                                 ),
                                 div(class = "numbered-item",
                                   div(class = "num-circle num-orange", "3"),
                                   div(class = "num-text", "TCGA Research Network publications for TCGA data")
                                 )
                               ),
                               div(class = "data-usage-callout",
                                 div(class = "callout-label", "DATA USAGE"),
                                 p(class = "callout-text",
                                   "All data is provided for research purposes. Please follow the data usage policies of the respective consortiums.")
                               )
                             )
                           )
                         )
                       )
                     )
            ),

            # ----------------------------------------------------------------
            # Tutorial
            # ----------------------------------------------------------------
            tabPanel("Tutorial",
                     div(class = "tutorial-page fade-in",
                       fluidRow(
                         column(8,
                           div(class = "about-card",
                             div(class = "about-card-header",
                               h4(class = "about-card-title", "Video tutorial"),
                               span(class = "about-card-meta", "12:48 · Aaron Wu")
                             ),
                             div(class = "video-frame",
                               div(class = "video-aspect",
                                 tags$iframe(id = "tutorial-iframe",
                                   src = tutorial_embed_url,
                                   frameborder = "0",
                                   allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
                                   allowfullscreen = NA
                                 )
                               )
                             ),
                             div(class = "video-footer",
                               span(class = "video-meta", tutorial_meta),
                               div(class = "video-actions",
                                 tags$a(href = tutorial_watch_url,
                                   target = "_blank", class = "btn-mini",
                                   HTML("Open in new tab &nearr;"))
                               )
                             )
                           )
                         ),
                         column(4,
                           div(class = "about-card chapters-card",
                             div(class = "about-card-header",
                               h4(class = "about-card-title", "Chapters"),
                               span(class = "about-card-meta", "5")
                             ),
                             div(class = "chapters-list",
                               chapter_item("0:00", 0,   "About",                 "Overview and citation",        active = TRUE),
                               chapter_item("0:48", 48,  "CliPP-on-Web",          "Upload and run your own data"),
                               chapter_item("2:47", 167, "CliPP Data Resources",  "TCGA and PCAWG results"),
                               chapter_item("4:10", 250, "Driver Mutation",        "Driver mutation analysis"),
                               chapter_item("5:20", 320, "Downloads",             "Datasets and exports")
                             )
                           )
                         )
                       )
                     )
            ),

            # ----------------------------------------------------------------
            # CliPP-on-Web
            # ----------------------------------------------------------------
            tabPanel("CliPP-on-Web",
                     sidebarLayout(
                         sidebarPanel(
                           tags$div(
                             p("The online tool requires three files as input: SNV File, CNA File and Cancer Purity File.
                               Please check the three links below to download the example files:"),
                             p(a("SNV file", href=paste0(global_TCGA_PCAWG, "/sample.snv.txt"), download="sample.snv.txt"),
                               "TSV file with columns: chromosome_index, position, ref_count, alt_count", br(),
                               a("CNA file", href=paste0(global_TCGA_PCAWG, "/sample.cna.txt"), download="sample.cna.txt"),
                               "TSV file with columns: chromosome_index, start_position, end_position, major_cn, minor_cn, and total_cn", br(),
                               a("Purity file", href=paste0(global_TCGA_PCAWG, "/sample.purity.txt"), download="sample.purity.txt"),
                               "File storing a scalar purity value between 0 and 1"),
                             p("When uploading files, please ensure the file names contain the appropriate suffixes (e.g., snv.txt, cna.txt, purity.txt) to help the application correctly identify and process them.")
                           ),
                           radioButtons("uploadMode", "Choose your upload method:",
                                        choices = list("Upload all files at once" = "all_at_once",
                                                       "Upload files individually" = "individually"),
                                        selected = "all_at_once"),
                           numericInput("numSamples", "Number of samples:", min = 1, max = 10, value = 1),
                           actionButton("setSamples", "Set Samples", class = "btn-primary"),
                           br(),
                           uiOutput("dynamicFileInputs"),
                           hidden(actionButton("uploadSamples", "Upload Samples", class = "btn-primary")),
                           br(),
                           hidden(selectInput("selectedSample", "Select a sample to analyze:", choices = NULL)),
                           hidden(actionButton("submit", "Submit", class = "btn-primary")),
                           hidden(selectInput("colorBy", "Color points by:", choices = list("Clonality" = "clonality", "Read Depth" = "readDepth"), selected = "clonality")),
                           hidden(sliderInput("smoothingFactor1", "Smoothing Factor for VAF Plot:",
                                              min = 0.2, max = 0.7, value = 0.45)),
                           uiOutput("smoothingExplanation1"),
                           hidden(sliderInput("smoothingFactor2", "Smoothing Factor for CP Plot:",
                                              min = 0.2, max = 0.7, value = 0.45)),
                           uiOutput("smoothingExplanation2"),
                           hidden(actionButton("clear", "Clear", class = "btn-danger"))
                         ),
                         mainPanel(
                           plotlyOutput("plot1"),
                           uiOutput("caption1"),
                           plotlyOutput("plot2"),
                           uiOutput("caption2"),
                           uiOutput("plotOutputArea"),
                           hidden(div(id = "plotReadyTextCliPP")),
                           shinyjs::hidden(
                             shinyWidgets::progressBar(
                               id = "progress",
                               value = 0,
                               total = 100,
                               display_pct = TRUE
                             )
                           )
                         )
                       )
            ),

            # ----------------------------------------------------------------
            # CliPP Data Resources
            # ----------------------------------------------------------------
            tabPanel("CliPP Data Resources",
                     tags$div(
                       div(class = "info-panel", style = "margin-bottom: 1.25rem;",
                         p("Explore subclonal reconstruction results from 7,279 TCGA samples and 2,778 PCAWG samples. For each sample, we provide a visualization of subclonal reconstruction results and subclonal mutation fraction (sMF) calculation. Samples included in our data exploration all passed our filtering criteria, having more than 20 and less than 20,000 SNVs, and number of reads per chromosomal copy (nrpcc) value greater than 10. Additional sample filtering was performed based on quality control of copy number calls. For more details please see our ",
                           a(href = "https://www.biorxiv.org/content/10.1101/2021.03.31.437383v2",
                             target = "_blank", "pre-print"), ". To download the full datasets, visit the ",
                           tags$a(href = "#", onclick = "Shiny.setInputValue('main_nav_target','Downloads',{priority:'event'}); return false;", "Downloads page"), ".")
                       ),
                       tabsetPanel(
                         tabPanel("TCGA",
                                  sidebarLayout(
                                    sidebarPanel(
                                      uiOutput("cancer_type_ui_TCGA"),
                                      uiOutput("sample_name_ui_TCGA"),
                                      actionButton("submitTCGA", "Submit", class = "btn-primary"),
                                      hidden(selectInput("colorByTCGA", "Color points by:", choices = list("Clonality" = "clonality", "Read Depth" = "readDepth"), selected = "clonality")),
                                      hidden(sliderInput("smoothingFactor1_TCGA", "Smoothing Factor for VAF Plot:",
                                                         min = 0.2, max = 0.7, value = 0.45)),
                                      uiOutput("smoothingExplanation1_TCGA"),
                                      hidden(sliderInput("smoothingFactor2_TCGA", "Smoothing Factor for CP Plot:",
                                                         min = 0.2, max = 0.7, value = 0.45)),
                                      uiOutput("smoothingExplanation2_TCGA"),
                                      hr(),
                                      h6("Samples by cancer type"),
                                      DT::DTOutput("tcgaCancerCounts", width = "100%")
                                    ),
                                    mainPanel(
                                      plotlyOutput("plot1TCGA"),
                                      shinyjs::hidden(downloadButton("downloadPlot1TCGA",
                                        "Download VAF Plot (PNG)", class = "btn-download-plot")),
                                      uiOutput("caption1TCGA"),
                                      plotlyOutput("plot2TCGA"),
                                      shinyjs::hidden(downloadButton("downloadPlot2TCGA",
                                        "Download CP Plot (PNG)", class = "btn-download-plot")),
                                      uiOutput("caption2TCGA"),
                                      hidden(uiOutput("plotOutputAreaTCGA")),
                                      hidden(div(id = "plotReadyTextTCGA")),
                                      uiOutput("downloadUI_TCGA")
                                    )
                                  )
                         ),
                         tabPanel("PCAWG",
                                  sidebarLayout(
                                    sidebarPanel(
                                      uiOutput("cancer_type_ui_PCAWG"),
                                      uiOutput("sample_name_ui_PCAWG"),
                                      actionButton("submitPCAWG", "Submit", class = "btn-primary"),
                                      hidden(selectInput("colorByPCAWG", "Color points by:", choices = list("Clonality" = "clonality", "Read Depth" = "readDepth"), selected = "clonality")),
                                      hidden(sliderInput("smoothingFactor1_PCAWG", "Smoothing Factor for VAF Plot:",
                                                         min = 0.2, max = 0.7, value = 0.45)),
                                      uiOutput("smoothingExplanation1_PCAWG"),
                                      hidden(sliderInput("smoothingFactor2_PCAWG", "Smoothing Factor for CP Plot:",
                                                         min = 0.2, max = 0.7, value = 0.45)),
                                      uiOutput("smoothingExplanation2_PCAWG"),
                                      hr(),
                                      h6("Samples by cancer type"),
                                      DT::DTOutput("pcawgCancerCounts", width = "100%")
                                    ),
                                    mainPanel(
                                      plotlyOutput("plot1PCAWG"),
                                      shinyjs::hidden(downloadButton("downloadPlot1PCAWG",
                                        "Download VAF Plot (PNG)", class = "btn-download-plot")),
                                      uiOutput("caption1PCAWG"),
                                      plotlyOutput("plot2PCAWG"),
                                      shinyjs::hidden(downloadButton("downloadPlot2PCAWG",
                                        "Download CP Plot (PNG)", class = "btn-download-plot")),
                                      uiOutput("caption2PCAWG"),
                                      hidden(uiOutput("plotOutputAreaPCAWG")),
                                      hidden(div(id = "plotReadyTextPCAWG")),
                                      uiOutput("downloadUI_PCAWG")
                                    )
                                  )
                         )
                       )
                     )
            ),

            # ----------------------------------------------------------------
            # Driver Mutation
            # ----------------------------------------------------------------
            tabPanel("Driver Mutation",
                     div(class = "driver-page fade-in",
                       div(class = "info-panel", style = "margin-bottom: 1.25rem;",
                         p("Explore driver mutation annotation for mutations in 289 genes across 5,440 TCGA samples from 31 cancer types. Driver mutation lists were obtained from ",
                           a(href = "https://www.intogen.org", target = "_blank", "IntOGen"),
                           " (Martínez-Jiménez et al., ", tags$em("Nature Reviews Cancer"), ", 2020).")
                       ),
                       div(class = "about-card driver-card",
                         div(class = "driver-toolbar",
                           uiOutput("cancer_type_ui_driver")
                         ),
                         div(class = "driver-table-wrap",
                           DT::DTOutput("driverMutationTable")
                         )
                       ),
                       shinyjs::hidden(div(id = "driverPlotSection",
                         div(class = "about-card",
                           div(class = "about-card-header",
                             h4(class = "about-card-title", "VAF Distribution")
                           ),
                           div(class = "about-card-body",
                             shinyjs::hidden(sliderInput("smoothingFactor1_driver",
                               "Smoothing factor:", min = 0.2, max = 0.7, value = 0.45)),
                             uiOutput("smoothingExplanation1_driver"),
                             plotlyOutput("driverPlot1", height = "320px"),
                             uiOutput("driverCaption1")
                           )
                         ),
                         div(class = "about-card",
                           div(class = "about-card-header",
                             h4(class = "about-card-title", "CP Distribution")
                           ),
                           div(class = "about-card-body",
                             shinyjs::hidden(sliderInput("smoothingFactor2_driver",
                               "Smoothing factor:", min = 0.2, max = 0.7, value = 0.45)),
                             uiOutput("smoothingExplanation2_driver"),
                             plotlyOutput("driverPlot2", height = "320px"),
                             uiOutput("driverCaption2")
                           )
                         )
                       )),
                       div(class = "driver-footer-row",
                         div(class = "driver-footer-left",
                           shinyjs::hidden(
                             downloadButton("downloadDriverMutation",
                               "Download by Sample",
                               class = "btn-download-driver")
                           ),
                           shinyjs::hidden(
                             downloadButton("downloadDriverMutation_gene",
                               "Download by Gene",
                               class = "btn-download-driver")
                           )
                         ),
                         div(class = "driver-footer-right",
                           div(class = "driver-reference-label", "Reference:"),
                           p(class = "driver-reference",
                             "Martínez-Jiménez, Francisco, Ferran Muiños, Inés Sentís, Jordi Deu-Pons, Iker Reyes-Salazar, ",
                             "Claudia Arnedo-Pac, Loris Mularoni et al. 'A compendium of mutational cancer driver genes.' ",
                             tags$em("Nature Reviews Cancer"), " 20, no. 10 (2020): 555-572.")
                         )
                       )
                     )
            ),

            # ----------------------------------------------------------------
            # Data Exploration (staging-only: KM + Heatmap)
            # ----------------------------------------------------------------
            tabPanel("Data Exploration",
                     tags$div(class = "data-exploration-main fade-in",
                              tabsetPanel(id = "analysis_tabset",
                                          tabPanel("Kaplan–Meier",
                                                   tags$div(
                                                     tags$h3(tags$b("Kaplan-Meier Survival Analysis"), style = "color: #2c3e50; margin-bottom: 10px;"),
                                                     tags$p("Explore survival outcomes based on subclonal mutational fraction (sMF) across different cancer types and subtypes.",
                                                            style = "color: #7f8c8d; font-size: 16px; margin-bottom: 30px;"),
                                                     fluidRow(
                                                       column(3,
                                                              tags$div(class = "legend-panel",
                                                                       radioButtons("survivalType", "Survival Endpoint:",
                                                                                    choices = list(
                                                                                      "Progression-Free Interval (PFI)" = "PFI",
                                                                                      "Overall Survival (OS)" = "OS"
                                                                                    ),
                                                                                    selected = "PFI"),
                                                                       uiOutput("cancerTypeUI_survival"),
                                                                       div(style = "text-align: center; margin: 20px 0;",
                                                                           actionButton("generateKMPlot", "Generate Plot", class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                                                                           downloadButton("downloadSurvivalPlot", "Download Plot", class = "btn-secondary", style = "width: 100%;")
                                                                       )
                                                              ),
                                                              tags$div(class = "legend-panel",
                                                                       tags$h5(tags$b("Legend & Interpretation")),
                                                                       tags$div(class = "legend-item",
                                                                                tags$div(class = "legend-symbol", style = "background: #63A299;"),
                                                                                tags$div(class = "legend-text", "High sMF: Higher subclonal mutation burden")
                                                                       ),
                                                                       tags$div(class = "legend-item",
                                                                                tags$div(class = "legend-symbol", style = "background: #D6ACC0;"),
                                                                                tags$div(class = "legend-text", "Low sMF: Lower subclonal mutation burden")
                                                                       )
                                                              )
                                                       ),
                                                       column(9,
                                                              tags$div(class = "plot-section",
                                                                       plotlyOutput("survivalPlot", height = "450px")
                                                              ),
                                                              conditionalPanel(
                                                                condition = "output.survivalPlotGenerated == true",
                                                                tags$div(class = "analysis-summary",
                                                                         uiOutput("survivalPlotInfo")
                                                                )
                                                              ),
                                                              conditionalPanel(
                                                                condition = "output.survivalPlotGenerated == true",
                                                                tags$div(class = "sample-info-card",
                                                                         uiOutput("survivalSampleInfo")
                                                                )
                                                              )
                                                       )
                                                     )
                                                   )
                                          ),
                                          tabPanel("Heatmap",
                                                   tags$div(
                                                     tags$h3(tags$b("sMF Correlation Heatmap"), style = "color: #2c3e50; margin-bottom: 10px;"),
                                                     tags$p("Explore correlations between subclonal mutational fraction (sMF) and key genomic features across different cancer types and cohorts.",
                                                            style = "color: #7f8c8d; font-size: 16px; margin-bottom: 30px;"),
                                                     fluidRow(
                                                       column(3,
                                                              tags$div(class = "legend-panel",
                                                                       uiOutput("cohort_ui_heatmap"),
                                                                       uiOutput("cancer_types_ui_heatmap"),
                                                                       checkboxInput("show_numbers_heatmap", "Show correlation values", value = TRUE),
                                                                       div(style = "text-align: center; margin: 20px 0;",
                                                                           actionButton("generate_heatmap", "Generate Heatmap", class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                                                                           downloadButton("download_heatmap", "Download Plot", class = "btn-secondary", style = "width: 100%; margin-bottom: 5px;"),
                                                                           downloadButton("download_heatmap_data", "Download Data", class = "btn-secondary", style = "width: 100%;")
                                                                       )
                                                              )
                                                       ),
                                                       column(9,
                                                              tags$div(class = "plot-section",
                                                                       plotOutput("heatmap_plot", height = "600px")
                                                              ),
                                                              conditionalPanel(
                                                                condition = "output.heatmapGenerated == true",
                                                                tags$div(class = "correlation-table-container",
                                                                         tags$h4(tags$b("Summary Statistics"), style = "color: #2c3e50; margin-bottom: 15px;"),
                                                                         DT::DTOutput("heatmap_summary_table")
                                                                )
                                                              ),
                                                              conditionalPanel(
                                                                condition = "output.heatmapGenerated == true",
                                                                tags$div(class = "analysis-summary",
                                                                         uiOutput("heatmap_info")
                                                                )
                                                              )
                                                       )
                                                     )
                                                   )
                                          )
                              )
                     )
            ),

            # ----------------------------------------------------------------
            # Phylogenetic Tree (staging-only)
            # ----------------------------------------------------------------
            tabPanel("Phylogenetic Tree",
                     tags$div(class = "about-page fade-in",
                       div(class = "about-card",
                         div(class = "about-card-header",
                           h4(class = "about-card-title", "Tumor Phylogenetic Tree"),
                           span(class = "about-card-meta", "CONIPHER-inspired CCF nesting")
                         ),
                         div(class = "about-card-body",
                           tags$p("Reconstruct the evolutionary history of a tumor sample from CliPP subclonal cluster data. Select a data source, choose a sample, and click Generate Tree."),
                           div(class = "preprint-box",
                             span(class = "preprint-label", "HOW IT WORKS"),
                             tags$p(class = "preprint-citation",
                               "Each CliPP cluster is placed on the tree by its Cancer Cell Fraction (CCF). The clonal cluster (highest CCF) descends from a germline root node. Subclonal clusters nest below their nearest ancestor. Branch lengths represent CCF differences between parent and child.")
                           ),
                           fluidRow(
                             column(4,
                                    div(class = "legend-panel",
                                        radioButtons("phylo_source", "Data source:",
                                                     choices = list(
                                                       "TCGA"                      = "tcga",
                                                       "PCAWG"                     = "pcawg"
                                                     ),
                                                     selected = "tcga"),
                                        
                                        conditionalPanel(
                                          condition = "input.phylo_source == 'tcga'",
                                          selectInput("phylo_cancer_tcga", "Cancer Type:", choices = cohort_cancer_mapping$TCGA),
                                          selectInput("phylo_sample_tcga", "Sample:", choices = NULL)
                                        ),
                                        
                                        conditionalPanel(
                                          condition = "input.phylo_source == 'pcawg'",
                                          selectInput("phylo_cancer_pcawg", "Cancer Type:", choices = cohort_cancer_mapping$PCAWG),
                                          selectInput("phylo_sample_pcawg", "Sample:", choices = NULL)
                                        ),
                                        
                                        div(style = "text-align: center; margin-top: 20px;",
                                            actionButton("generatePhyloTree", "Generate Tree",
                                                         class = "btn-primary", style = "width: 100%;"),
                                            br(), br(),
                                            downloadButton("downloadPhyloTree", "Download Plot",
                                                           class = "btn-secondary", style = "width: 100%;")
                                        )
                                    ),
                                    div(class = "legend-panel",
                                        tags$h5(tags$b("Legend")),
                                        tags$div(class = "legend-item",
                                                 tags$div(class = "legend-symbol",
                                                          style = "background:#fff; border:2px solid #aaa; border-radius:50%;"),
                                                 tags$div(class = "legend-text", "GL — Germline root (CCF = 1.0)")
                                        ),
                                        tags$div(class = "legend-item",
                                                 tags$div(class = "legend-symbol",
                                                          style = "background:#4E79A7; border-radius:50%;"),
                                                 tags$div(class = "legend-text", "Coloured circles — CliPP clusters")
                                        ),
                                        tags$p("Circle size ∝ mutation count. Branch labels show CCF difference.",
                                               style = "font-size:12px; color:#666; margin-top:10px;")
                                    )
                             ),
                             column(8,
                                    tags$div(class = "plot-section",
                                             plotOutput("phyloTreePlot", height = "auto")
                                    ),
                                    conditionalPanel(
                                      condition = "output.phyloTreeGenerated == true",
                                      tags$div(class = "analysis-summary",
                                               uiOutput("phyloTreeInfo")
                                      )
                                    )
                             )
                           )
                         )
                       )
                     )
             ),

            # ----------------------------------------------------------------
            # Downloads
            # ----------------------------------------------------------------
            tabPanel("Downloads",
                     div(class = "downloads-page fade-in",
                       fluidRow(
                         column(8,
                            div(id = "dl-simulation", class = "dataset-card",
                             div(class = "dataset-head",
                               div(class = "ds-icon ds-icon-blue", icon("globe")),
                               div(class = "dataset-head-text",
                                 div(class = "dataset-title", "Simulation dataset · CliPPSim4k"),
                                 div(class = "dataset-sub", "Simulated cancer genomic data for testing and validation")
                               ),
                             ),
                             div(class = "dataset-stats",
                               div(class = "stat", div(class = "stat-label", "SAMPLE COUNT"), div(class = "stat-value", "4,050")),
                               div(class = "stat", div(class = "stat-label", "FILE FORMAT"), div(class = "stat-value", "TSV · TXT")),
                               div(class = "stat", div(class = "stat-label", "GROUND TRUTH"), div(class = "stat-value", "Yes")),
                               div(class = "stat", div(class = "stat-label", "LAST UPDATED"), div(class = "stat-value", "2025-09-12"))
                             ),
                             tags$table(class = "dataset-table",
                               tags$thead(tags$tr(
                                 tags$th("DATASET"), tags$th("DESCRIPTION"),
                                 tags$th("TYPE"), tags$th("SIZE"), tags$th(class = "th-action", "ACTION")
                               )),
                               tags$tbody(
                                 tags$tr(
                                   tags$td(class = "td-name", "CliPPSim4k Summary"),
                                   tags$td(class = "td-desc", "Comprehensive simulation results for 4,050 samples with known ground truth"),
                                   tags$td(span(class = "type-badge", "TSV")),
                                   tags$td(class = "td-size", "~2 MB"),
                                   tags$td(class = "td-action",
                                     downloadButton("downloadSim4k", "Download", class = "btn-download"))
                                 ),
                                 tags$tr(
                                   tags$td(class = "td-name", "Individual simulation data"),
                                   tags$td(class = "td-desc", "Per-sample simulation files with ground-truth clonal structure"),
                                   tags$td(span(class = "type-badge", "TXT")),
                                   tags$td(class = "td-size", "212 MB"),
                                   tags$td(class = "td-action",
                                     downloadButton("browseSim4k", "Download ZIP", class = "btn-browse"))
                                 )
                               )
                             )
                           ),

                           div(id = "dl-tcga", class = "dataset-card",
                             div(class = "dataset-head",
                               div(class = "ds-icon ds-icon-orange", icon("layer-group")),
                               div(class = "dataset-head-text",
                                 div(class = "dataset-title", "TCGA · The Cancer Genome Atlas"),
                                 div(class = "dataset-sub", "Multi-platform genomic data processed through the CliPP pipeline")
                               )
                             ),
                             div(class = "dataset-stats",
                               div(class = "stat", div(class = "stat-label", "TUMORS"), div(class = "stat-value", dl_n_tcga)),
                               div(class = "stat", div(class = "stat-label", "CANCER TYPES"), div(class = "stat-value", dl_types_tcga)),
                               div(class = "stat", div(class = "stat-label", "FORMAT"), div(class = "stat-value", "TSV")),
                               div(class = "stat", div(class = "stat-label", "LAST UPDATED"), div(class = "stat-value", "2026-02-08"))
                             ),
                             tags$table(class = "dataset-table",
                               tags$thead(tags$tr(
                                 tags$th("DATASET"), tags$th("DESCRIPTION"),
                                 tags$th("TYPE"), tags$th("SIZE"), tags$th(class = "th-action", "ACTION")
                               )),
                               tags$tbody(
                                 tags$tr(
                                   tags$td(class = "td-name", "Per-sample CliPP results"),
                                   tags$td(class = "td-desc", "Individual clonal structure outputs - zip archive"),
                                   tags$td(span(class = "type-badge", "ZIP")),
                                   tags$td(class = "td-size", "372 MB"),
                                   tags$td(class = "td-action",
                                     downloadButton("browseTCGA", "Download ZIP", class = "btn-browse"))
                                 )
                               )
                             ),
                             div(class = "cancer-dl-row",
                               span(class = "cancer-dl-label", "Download by cancer type"),
                               div(class = "cancer-dl-controls",
                                 selectInput("tcga_cancer_type_dl", NULL,
                                   choices = if (!is.null(dl_tcga_counts)) sort(dl_tcga_counts[["Cancer Type"]]) else sort(cohort_cancer_mapping$TCGA),
                                   width = "160px"),
                                 downloadButton("downloadTCGA_cancer", "Raw assignments", class = "btn-download")
                               )
                             )
                           ),

                           div(id = "dl-pcawg", class = "dataset-card",
                             div(class = "dataset-head",
                               div(class = "ds-icon ds-icon-purple", icon("cubes-stacked")),
                               div(class = "dataset-head-text",
                                 div(class = "dataset-title", "PCAWG · Pan-Cancer Analysis of Whole Genomes"),
                                 div(class = "dataset-sub", "Whole-genome sequencing data with full subclonal reconstruction")
                               ),
                             ),
                             div(class = "dataset-stats",
                               div(class = "stat", div(class = "stat-label", "TUMORS"), div(class = "stat-value", dl_n_pcawg)),
                               div(class = "stat", div(class = "stat-label", "CANCER TYPES"), div(class = "stat-value", dl_types_pcawg)),
                               div(class = "stat", div(class = "stat-label", "FORMAT"), div(class = "stat-value", "TSV · VCF")),
                               div(class = "stat", div(class = "stat-label", "LAST UPDATED"), div(class = "stat-value", "2025-12-04"))
                             ),
                             tags$table(class = "dataset-table",
                               tags$thead(tags$tr(
                                 tags$th("DATASET"), tags$th("DESCRIPTION"),
                                 tags$th("TYPE"), tags$th("SIZE"), tags$th(class = "th-action", "ACTION")
                               )),
                               tags$tbody(
                                 tags$tr(
                                   tags$td(class = "td-name", "Per-sample CliPP outputs"),
                                   tags$td(class = "td-desc", "Cluster assignments and CCF estimates for every PCAWG tumor"),
                                   tags$td(span(class = "type-badge", "ZIP")),
                                   tags$td(class = "td-size", "2.8 GB"),
                                   tags$td(class = "td-action",
                                     downloadButton("browsePCAWG", "Download ZIP", class = "btn-browse"))
                                 )
                               )
                             ),
                             div(class = "cancer-dl-row",
                               span(class = "cancer-dl-label", "Download by cancer type"),
                               div(class = "cancer-dl-controls",
                                 selectInput("pcawg_cancer_type_dl", NULL,
                                   choices = if (!is.null(dl_pcawg_counts)) sort(dl_pcawg_counts[["Cancer Type"]]) else character(0),
                                   width = "160px"),
                                 downloadButton("downloadPCAWG_cancer", "Raw assignments", class = "btn-download")
                               )
                             )
                           ),

                           div(id = "dl-driver", class = "dataset-card",
                             div(class = "dataset-head",
                               div(class = "ds-icon ds-icon-green", icon("dna")),
                               div(class = "dataset-head-text",
                                 div(class = "dataset-title", "Driver Mutations · TCGA"),
                                 div(class = "dataset-sub", "Candidate driver mutations annotated with clonality status across TCGA tumors")
                               ),
                             ),
                             div(class = "dataset-stats",
                               div(class = "stat", div(class = "stat-label", "MUTATIONS"), div(class = "stat-value", "16,913")),
                               div(class = "stat", div(class = "stat-label", "SAMPLES"),   div(class = "stat-value", "5,440")),
                               div(class = "stat", div(class = "stat-label", "CANCER TYPES"), div(class = "stat-value", "31")),
                               div(class = "stat", div(class = "stat-label", "FORMAT"),    div(class = "stat-value", "CSV"))
                             ),
                             tags$table(class = "dataset-table",
                               tags$thead(tags$tr(
                                 tags$th("DATASET"), tags$th("DESCRIPTION"),
                                 tags$th("TYPE"), tags$th("SIZE"), tags$th(class = "th-action", "ACTION")
                               )),
                               tags$tbody(
                                 tags$tr(
                                   tags$td(class = "td-name", "Driver mutation data"),
                                   tags$td(class = "td-desc", "Driver mutations with VAF, CP, clonality, and clinical annotations for all TCGA samples"),
                                   tags$td(span(class = "type-badge", "CSV")),
                                   tags$td(class = "td-size", "~12 MB"),
                                   tags$td(class = "td-action",
                                     downloadButton("downloadDriverData", "Download", class = "btn-download"))
                                 )
                               )
                             ),
                             div(class = "cancer-dl-row",
                               span(class = "cancer-dl-label", "Download by cancer type"),
                               div(class = "cancer-dl-controls",
                                 selectInput("driver_cancer_type_dl", NULL,
                                   choices = driver_cancer_types,
                                   width = "160px"),
                                 downloadButton("downloadDriverData_cancer", "Download", class = "btn-download")
                               )
                             )
                           )
                         ),

                         column(4,
                           div(class = "about-card",
                             div(class = "about-card-header",
                               h4(class = "about-card-title", "On this page")
                             ),
                             div(class = "about-card-body sidebar-toc",
                               tags$a(href = "#dl-simulation", class = "toc-link active", "Simulation data"),
                               tags$a(href = "#dl-tcga",       class = "toc-link", "TCGA data"),
                               tags$a(href = "#dl-pcawg",      class = "toc-link", "PCAWG data"),
                               tags$a(href = "#dl-driver",     class = "toc-link", "Driver mutations")
                             )
                           ),
                           div(class = "about-card",
                             div(class = "about-card-header",
                               h4(class = "about-card-title", "Access issues?")
                             ),
                             div(class = "about-card-body",
                               p(class = "access-issues-text",
                                 "If you encounter problems with downloads, contact ",
                                 a(href = "mailto:pandachessaaron@gmail.com", "pandachessaaron@gmail.com"),
                                 " or ",
                                 a(href = "mailto:MDMontierth@mdanderson.org", "MDMontierth@mdanderson.org"),
                                 ".")
                             )
                           )
                         )
                       )
                     )
            ),

            # ----------------------------------------------------------------
          ) # end tabsetPanel
        )   # end app-content
      ),    # end app-page-area
      tags$footer(class = "app-footer",
        div(class = "footer-left",
          HTML("&copy; 2024 The University of Texas MD Anderson Cancer Center."),
          span(class = "footer-meta", "All rights reserved."),
          tags$a(href = "#", class = "footer-link", "Terms"),
          tags$a(href = "#", class = "footer-link", "Privacy"),
          tags$a(href = "#", class = "footer-link", "Acknowledgements")
        ),
        div(class = "footer-right", "CliPP-on-Web · build 2.4.1")
      )
    )     # end app-main
  )   # end app-shell
)
