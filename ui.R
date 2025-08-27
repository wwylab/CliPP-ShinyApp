library(shiny)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(DT)

# UI for the Shiny application
ui <- fluidPage(
  tags$head(includeHTML("google-analytics.html")),
  shinyjs::useShinyjs(),
  
  # CSS for styling
  tags$head(
    tags$style(HTML("
      #progress {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        width: 50%;
        z-index: 9999;
      }
      .plot-container {
        position: relative;
        width: 100%;
        margin-bottom: 40px;
      }
      .plot-with-controls {
        display: flex;
        flex-direction: column;
        gap: 10px;
        margin-bottom: 20px;
      }
      .plot-area {
        width: 100%;
      }
      .controls-area {
        position: absolute;
        top: 28%;
        left: 86%;
        width: 20%;
        min-width: 100px;
        max-width: 200px;
        background: #f5f5f5;
        padding: 15px;
        border-radius: 8px;
        z-index: 100;
        transition: top 0.3s ease; 
      }
      .controls-area-depth {
        top: -10%; 
      }
      .caption-area {
        width: 100%;
        margin-bottom: 30px;
      }
      .recommended-smoothing {
        font-size: 0.9em;
        color: #666;
        margin-top: 10px;
      }
    ")),
    
    tags$script(HTML("
      $(document).ready(function() {
        function adjustControlPosition(colorBySelector, controlAreaSelectors) {
          $(document).on('change', colorBySelector, function() {
            var colorMode = $(this).val();
            
            controlAreaSelectors.forEach(function(selector) {
              if (colorMode === 'readDepth' || colorMode === 'depth') {
                $(selector).addClass('controls-area-depth');
              } else {
                $(selector).removeClass('controls-area-depth');
              }
            });
          });
        }
        
        adjustControlPosition('#colorBy', ['.plot-container .controls-area']);
        
        adjustControlPosition('#colorByTCGA', ['.plot-container .controls-area']);
        
        adjustControlPosition('#colorByPCAWG', ['.plot-container .controls-area']);
        
        adjustControlPosition('#colorByDriver', ['.plot-container .controls-area']);
      });
    "))
  ),
  
  # Title and attribution for research purposes
  titlePanel("CliPP-on-Web: Scalable Subclonal Reconstruction for Cancer Research"),
  div(p("Copyright ", HTML("&copy;"), " 2025 The University of Texas MD Anderson Cancer Center. All rights reserved.")),
  
  # Multi-page navigation structure
  navbarPage("",
             # Page 1: About section with application description and contact info
             tabPanel("About",
                      tags$div(
                        #tags$h4(tags$b(tags$span("Description"))),
                        #p("CliPP-on-Web: Scalable Subclonal Reconstruction for Cancer Research"),
                        p(tags$b("Advancing Tumor Evolution Studies with Efficient Analysis")),
                        p("CliPP (Clonal structure identification through Pairwise Penalization) is an advanced computational framework for identifying clonal and subclonal mutations using a regularized likelihood model. Designed for researchers investigating tumor heterogeneity, CliPP-on-Web provides rapid and accurate subclonal reconstruction without requiring programming expertise."),
                        p(tags$b("Addressing Computational Challenges in Subclonal Analysis")),
                        p("Intra-tumor heterogeneity plays a critical role in tumor evolution and therapeutic resistance. However, large-scale subclonal reconstruction has traditionally been constrained by computational limitations. CliPP-on-Web overcomes these challenges by enabling high-throughput, efficient analysis of sequencing data at scale."),
                        p(tags$b("Efficient, Large-Scale Analysis")),
                        p("Leveraging an optimized likelihood-based clustering model, CliPP has successfully processed whole-genome and whole-exome sequencing data from over 10,000 tumor samples across TCGA and PCAWG. These datasets are accessible for further exploration in the Data Resources tab."),
                        p(tags$b("Key Features")),
                        tags$ul(
                          tags$li("Analyze Your Own Data: Upload tumor sequencing data and obtain rapid subclonal reconstruction results."),
                          tags$li("Access Curated Datasets: Explore pre-processed subclonal analyses from PCAWG and TCGA."),
                          tags$li("Driver Mutation Subclonality: Investigate the clonal architecture of key driver mutations in TCGA.")
                        ),
                        p("For detailed methodology, refer to our BioRxiv preprint: ",
                          a(href = "https://www.biorxiv.org/content/10.1101/2024.07.03.601939v1",
                            "https://www.biorxiv.org/content/10.1101/2024.07.03.601939v1"), "."),
                        tags$br(),
                        p(tags$b(tags$span(style = "color: #D22B2B;", "For issues with the app, please contact:"))),
                        p("Aaron Wu - ", a(href = "mailto:aw80@rice.edu", "aw80@rice.edu")),
                        p("Quang Tran - ", a(href = "mailto:qmtran@mdanderson.org", "qmtran@mdanderson.org"))
                      )
             ),
             
             # Page 2: Tutorial section with an embedded video
             tabPanel("Tutorial",
                      tags$div(
                        tags$h4(tags$b("Video Tutorial")),
                        tags$p("Watch the video below to learn how to use the CliPP-on-Web application:"),
                        tags$iframe(src="https://www.youtube.com/embed/NTJX33csjwA",
                                    style="width:80%; height:700px; border:none;"),
                        
                        tags$br(),
                        tags$hr(),
                        
                        # Additional tutorial examples section
                        tags$h4(tags$b("Tutorial Examples")),
                        tags$p("Explore these step-by-step examples to master different aspects of CliPP analysis:"),
                        
                        # Example placeholders
                        tags$div(
                          style = "margin: 20px 0;",
                          tags$h5(tags$b("Example 1: Basic Data Upload and Analysis")),
                          tags$p(tags$em("Coming Soon:"), " Learn how to upload your SNV, CNA, and purity files for a complete CliPP analysis."),
                          tags$ul(
                            tags$li("File format requirements and validation"),
                            tags$li("Understanding output visualizations"),
                            tags$li("Interpreting clonal vs. subclonal mutations")
                          )
                        ),
                        
                        tags$div(
                          style = "margin: 20px 0;",
                          tags$h5(tags$b("Example 2: TCGA Data Exploration")),
                          tags$p(tags$em("Coming Soon:"), " Navigate the extensive TCGA dataset to analyze cancer-specific patterns."),
                          tags$ul(
                            tags$li("Selecting appropriate cancer types and samples"),
                            tags$li("Comparing clonal architecture across samples"),
                            tags$li("Downloading results for further analysis")
                          )
                        ),
                        
                        tags$div(
                          style = "margin: 20px 0;",
                          tags$h5(tags$b("Example 3: PCAWG Whole Genome Analysis")),
                          tags$p(tags$em("Coming Soon:"), " Dive deep into whole genome sequencing data from the PCAWG consortium."),
                          tags$ul(
                            tags$li("Understanding whole genome vs. exome differences"),
                            tags$li("Lambda parameter selection and optimization"),
                            tags$li("Advanced visualization techniques")
                          )
                        ),
                        
                        tags$div(
                          style = "margin: 20px 0;",
                          tags$h5(tags$b("Example 4: Driver Mutation Subclonality")),
                          tags$p(tags$em("Coming Soon:"), " Investigate the clonal status of key driver mutations across cancer types."),
                          tags$ul(
                            tags$li("Identifying clinically relevant driver mutations"),
                            tags$li("Assessing subclonal vs. clonal driver events"),
                            tags$li("Correlating with treatment outcomes")
                          )
                        ),
                        
                        tags$div(
                          style = "margin: 20px 0;",
                          tags$h5(tags$b("Example 5: Survival Analysis Integration")),
                          tags$p(tags$em("Coming Soon:"), " Connect subclonal mutation burden with patient survival outcomes."),
                          tags$ul(
                            tags$li("Calculating subclonal mutational fraction (sMF)"),
                            tags$li("Kaplan-Meier survival curve interpretation"),
                            tags$li("Statistical significance testing")
                          )
                        ),
                        
                        tags$div(
                          style = "margin: 20px 0;",
                          tags$h5(tags$b("Example 6: Advanced Parameter Tuning")),
                          tags$p(tags$em("Coming Soon:"), " Master advanced CliPP parameters for specialized analyses."),
                          tags$ul(
                            tags$li("Smoothing factor optimization"),
                            tags$li("Handling low-purity samples"),
                            tags$li("Multi-sample comparative analysis")
                          )
                        ),
                        
                        tags$br(),
                        tags$div(
                          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #007bff;",
                          tags$p(tags$b("Note:"), " These detailed tutorial examples are currently in development. 
                                 For immediate assistance, please watch the video tutorial above or contact our support team.")
                        )
                      )
             ),
             
             # Page 3: Main functionality for uploading and analyzing data with CliPP-on-Web
             tabPanel("CliPP-on-Web",
                      tags$div(
                        tags$h4(tags$b("Upload Files")),
                        sidebarLayout(
                          sidebarPanel(
                            tags$div(
                              p("The online tool requires three files as input: SNV File, CNA File and Cancer Purity File.
                              Please check the three links below to download the example files:"),
                              p(a("SNV file", href="sample.snv.txt", download="sample.snv.txt"),
                                "TSV file with columns: chromosome_index, position, ref_count, alt_count", br(),
                                a("CNA file", href="sample.cna.txt", download="sample.cna.txt"),
                                "TSV file with columns: chromosome_index, start_position, end_position, major_cn, minor_cn, and total_cn", br(),
                                a("Purity file", href="sample.purity.txt", download="sample.purity.txt"),
                                "File storing a scalar purity value between 0 and 1"
                              )
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
                            hidden(selectInput("colorBy", "Color points by:",
                                               choices = list("Clonality" = "clonality", "Read Depth" = "readDepth"),
                                               selected = "clonality")),
                            hidden(actionButton("clear", "Clear", class = "btn-danger"))
                          ),
                          
                          # Main panel to display plots and analysis output
                          mainPanel(
                            # First plot - VAF plot
                            div(class = "plot-container",
                                div(class = "plot-area",
                                    plotlyOutput("plot1")
                                ),
                                conditionalPanel(
                                  condition = "output.plotsReadyCliPP == true",
                                  div(class = "controls-area",
                                      sliderInput("smoothingFactor1", "Smoothing Factor:",
                                                  min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                                      div(class = "recommended-smoothing",
                                          uiOutput("smoothingExplanation1")
                                      )
                                  )
                                ),
                                div(class = "caption-area",
                                    uiOutput("caption1")
                                )
                            ),
                            
                            # Second plot - CP plot
                            div(class = "plot-container",
                                div(class = "plot-area",
                                    plotlyOutput("plot2")
                                ),
                                conditionalPanel(
                                  condition = "output.plotsReadyCliPP == true",
                                  div(class = "controls-area",
                                      sliderInput("smoothingFactor2", "Smoothing Factor:",
                                                  min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                                      div(class = "recommended-smoothing",
                                          uiOutput("smoothingExplanation2")
                                      )
                                  )
                                ),
                                div(class = "caption-area",
                                    uiOutput("caption2")
                                )
                            ),
                            
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
                      )
             ),
             
             # Page 4: CliPP Data Resources with automatic data loading and visualization
             tabPanel("CliPP Data Resources",
                      tags$div(
                        tabsetPanel(id = "tabset",
                                    # Subsection for TCGA data selection and analysis
                                    tabPanel("TCGA",
                                             tags$div(
                                               tags$h4(tags$b("TCGA Select")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   uiOutput("cohort_ui_TCGA"),
                                                   checkboxInput("disable_select_TCGA", "Deactivate Cancer Type Selection", value = FALSE),
                                                   uiOutput("cancer_type_ui_TCGA"),
                                                   uiOutput("sample_name_ui_TCGA"),
                                                   actionButton("submitTCGA", "Submit", class = "btn-primary"),
                                                   hidden(actionButton("clearTCGA", "Clear", class = "btn-danger")),
                                                   hidden(selectInput("colorByTCGA", "Color points by:", 
                                                                      choices = list("Clonality" = "clonality", "Read Depth" = "readDepth"), 
                                                                      selected = "clonality"))
                                                 ),
                                                 mainPanel(
                                                   # First TCGA plot
                                                   div(class = "plot-container",
                                                       div(class = "plot-area",
                                                           plotlyOutput("plot1TCGA") 
                                                       ),
                                                       conditionalPanel(
                                                         condition = "output.plotsReadyTCGA == true",
                                                         div(class = "controls-area",
                                                             sliderInput("smoothingFactor1_TCGA", "Smoothing Factor:",
                                                                         min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                                                             div(class = "recommended-smoothing",
                                                                 uiOutput("smoothingExplanation1_TCGA")
                                                             )
                                                         )
                                                       ),
                                                       div(class = "caption-area",
                                                           uiOutput("caption1TCGA")
                                                       )
                                                   ),
                                                   
                                                   # Second TCGA plot
                                                   div(class = "plot-container",
                                                       div(class = "plot-area",
                                                           plotlyOutput("plot2TCGA") 
                                                       ),
                                                       conditionalPanel(
                                                         condition = "output.plotsReadyTCGA == true",
                                                         div(class = "controls-area",
                                                             sliderInput("smoothingFactor2_TCGA", "Smoothing Factor:",
                                                                         min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                                                             div(class = "recommended-smoothing",
                                                                 uiOutput("smoothingExplanation2_TCGA")
                                                             )
                                                         )
                                                       ),
                                                       div(class = "caption-area",
                                                           uiOutput("caption2TCGA")
                                                       )
                                                   ),
                                                   
                                                   hidden(uiOutput("plotOutputAreaTCGA")),
                                                   hidden(div(id = "plotReadyTextTCGA")),
                                                   uiOutput("downloadUI_TCGA")
                                                 )
                                               )
                                             )
                                    ),
                                    
                                    # Subsection for PCAWG data selection and analysis
                                    tabPanel("PCAWG",
                                             tags$div(
                                               tags$h4(tags$b("PCAWG Select")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   uiOutput("cohort_ui_PCAWG"),
                                                   checkboxInput("disable_select_PCAWG", "Deactivate Cancer Type Selection", value = FALSE),
                                                   uiOutput("cancer_type_ui_PCAWG"),
                                                   uiOutput("sample_name_ui_PCAWG"),
                                                   actionButton("submitPCAWG", "Submit", class = "btn-primary"),
                                                   hidden(actionButton("clearPCAWG", "Clear", class = "btn-danger")),
                                                   hidden(selectInput("colorByPCAWG", "Color points by:", 
                                                                      choices = list("Clonality" = "clonality", "Read Depth" = "readDepth"), 
                                                                      selected = "clonality"))
                                                 ),
                                                 mainPanel(
                                                   # First PCAWG plot
                                                   div(class = "plot-container",
                                                       div(class = "plot-area",
                                                           plotlyOutput("plot1PCAWG")
                                                       ),
                                                       conditionalPanel(
                                                         condition = "output.plotsReadyPCAWG == true",
                                                         div(class = "controls-area",
                                                             sliderInput("smoothingFactor1_PCAWG", "Smoothing Factor:",
                                                                         min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                                                             div(class = "recommended-smoothing",
                                                                 uiOutput("smoothingExplanation1_PCAWG")
                                                             )
                                                         )
                                                       ),
                                                       div(class = "caption-area",
                                                           uiOutput("caption1PCAWG")
                                                       )
                                                   ),
                                                   
                                                   # Second PCAWG plot
                                                   div(class = "plot-container",
                                                       div(class = "plot-area",
                                                           plotlyOutput("plot2PCAWG") 
                                                       ),
                                                       conditionalPanel(
                                                         condition = "output.plotsReadyPCAWG == true",
                                                         div(class = "controls-area",
                                                             sliderInput("smoothingFactor2_PCAWG", "Smoothing Factor:",
                                                                         min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                                                             div(class = "recommended-smoothing",
                                                                 uiOutput("smoothingExplanation2_PCAWG")
                                                             )
                                                         )
                                                       ),
                                                       div(class = "caption-area",
                                                           uiOutput("caption2PCAWG")
                                                       )
                                                   ),
                                                   
                                                   hidden(uiOutput("plotOutputAreaPCAWG")),
                                                   hidden(div(id = "plotReadyTextPCAWG")),
                                                   uiOutput("downloadUI_PCAWG")
                                                 )
                                               )
                                             )
                                    )
                        )
                      )
             ),
             
             # Page 5: Driver Mutation analysis tool
             tabPanel("Driver Mutation",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput("cancer_type_ui_driver"),
                          tags$div(
                            class = "alert alert-info",
                            tags$b("How to use:"),
                            tags$div(
                              tags$p("To explore driver mutations, first select a cancer type from the dropdown menu. You can then view mutations in two modes:"),
                              tags$p("Consolidated by Sample: Displays aggregated mutation data for each sample, helping you identify prevalent mutations across different cases."),
                              tags$p("Individual Mutations: Provides a detailed view of each mutation within a sample, allowing for in-depth analysis of specific genetic alterations."),
                              tags$p("Click on any entry in the mutation table to see detailed visualization of the genetic changes.")
                            )
                          ),
                          radioButtons("view_mode", "View Mode:",
                                       choices = list(
                                         "Consolidated by Sample" = "consolidated",
                                         "Individual Mutations" = "individual"
                                       ),
                                       selected = "consolidated")
                        ),
                        mainPanel(
                          DT::DTOutput("driverMutationTable"),
                          uiOutput("selectedMutationInfo"),
                          downloadButton("downloadDriverMutation", "Download Data"),
                          tags$h4(tags$b(tags$span("Reference:"))),
                          p("Martínez-Jiménez, Francisco, et al. 'A compendium of mutational cancer driver genes.' Nature Reviews Cancer 20.10 (2020): 555-572."),
                          
                          # Driver plot 1
                          div(class = "plot-container",
                              div(class = "plot-area",
                                  plotlyOutput("driverPlot1") 
                              ),
                              conditionalPanel(
                                condition = "output.driverRowSelected == true",
                                div(class = "controls-area",
                                    sliderInput("smoothingFactor1_driver", "Smoothing Factor:",
                                                min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                                    div(class = "recommended-smoothing",
                                        uiOutput("smoothingExplanation1_driver")
                                    )
                                )
                              ),
                              div(class = "caption-area",
                                  hidden(uiOutput("driverCaption1"))
                              )
                          ),
                          
                          # Driver plot 2
                          div(class = "plot-container",
                              div(class = "plot-area",
                                  plotlyOutput("driverPlot2") 
                              ),
                              conditionalPanel(
                                condition = "output.driverRowSelected == true",
                                div(class = "controls-area",
                                    sliderInput("smoothingFactor2_driver", "Smoothing Factor:",
                                                min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                                    div(class = "recommended-smoothing",
                                        uiOutput("smoothingExplanation2_driver")
                                    )
                                )
                              ),
                              div(class = "caption-area",
                                  hidden(uiOutput("driverCaption2"))
                              )
                          )
                        )
                      )
             ),
             
             # Page 6: KM Plots & Scatter Analysis
             tabPanel("KM Plots & Scatter Analysis",
                      tags$div(
                        tabsetPanel(id = "analysis_tabset",
                                    # Sub-tab for Kaplan-Meier survival plots
                                    tabPanel("Kaplan-Meier Survival",
                                             tags$div(
                                               tags$h4(tags$b("Kaplan-Meier Survival Analysis")),
                                               p("Explore survival outcomes based on subclonal mutational fraction (sMF) across different cancer types and subtypes."),
                                               
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   tags$div(
                                                     class = "alert alert-info",
                                                     tags$b("How to use:"),
                                                     tags$div(
                                                       tags$p("1. Select the survival endpoint (PFI or OS)"),
                                                       tags$p("2. Choose a cancer type from the available options"),
                                                       tags$p("3. View the Kaplan-Meier survival curve"),
                                                       tags$p("The algorithm automatically determines the optimal cutoff for subclonal mutational fraction (sMF) using decision tree analysis.")
                                                     )
                                                   ),
                                                   
                                                   radioButtons("survivalType", "Survival Endpoint:",
                                                                choices = list(
                                                                  "Progression-Free Interval (PFI)" = "PFI",
                                                                  "Overall Survival (OS)" = "OS"
                                                                ),
                                                                selected = "PFI"),
                                                   
                                                   uiOutput("cancerTypeUI_survival"),
                                                   
                                                   actionButton("generateKMPlot", "Generate Survival Plot", class = "btn-primary"),
                                                   
                                                   tags$br(), tags$br(),
                                                   
                                                   downloadButton("downloadSurvivalPlot", "Download Plot", class = "btn-secondary"),
                                                   
                                                   tags$hr(),
                                                   
                                                   tags$div(
                                                     tags$h5(tags$b("Legend:")),
                                                     tags$p("• sMF: Subclonal Mutational Fraction"),
                                                     tags$p("• High sMF (≥ cutoff): Higher subclonal burden"),
                                                     tags$p("• Low sMF (< cutoff): Lower subclonal burden"),
                                                     tags$p("• P-value: Log-rank test significance")
                                                   )
                                                 ),
                                                 
                                                 mainPanel(
                                                   # Survival plot output
                                                   div(class = "plot-container",
                                                       div(class = "plot-area",
                                                           plotlyOutput("survivalPlot", height = "600px")
                                                       )
                                                   ),
                                                   
                                                   # Additional information
                                                   uiOutput("survivalPlotInfo"),
                                                   
                                                   # Sample size information
                                                   uiOutput("survivalSampleInfo")
                                                 )
                                               )
                                             )
                                    ),
                                    
                                    # Sub-tab for TMB vs sMF scatter plots
                                    tabPanel("TMB vs sMF Scatter",
                                             tags$div(
                                               tags$h4(tags$b("Tumor Mutational Burden vs Subclonal Mutational Fraction")),
                                               p("Explore the relationship between TMB and sMF across different cohorts and cancer types."),
                                               
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   tags$div(
                                                     class = "alert alert-info",
                                                     tags$b("How to use:"),
                                                     tags$div(
                                                       tags$p("1. Select a cohort to analyze"),
                                                       tags$p("2. Choose specific cancer types (optional)"),
                                                       tags$p("3. Toggle log scale for TMB axis"),
                                                       tags$p("4. View correlation coefficients and scatter plots")
                                                     )
                                                   ),
                                                   
                                                   selectInput("selected_cohort_tmb", "Select Cohort:",
                                                               choices = list(
                                                                 "TCGA" = "TCGA",
                                                                 "PCAWG" = "PCAWG", 
                                                                 "OCCAMS" = "OCCAMS",
                                                                 "NCT02113657 (IPI)" = "NCT02113657 (IPI)",
                                                                 "NCT02703623 (ARPi+IPI)" = "NCT02703623 (ARPi+IPI)"
                                                               ),
                                                               selected = "TCGA"),
                                                   
                                                   uiOutput("cancer_types_ui_tmb"),
                                                   
                                                   checkboxInput("log_scale_tmb", "Use log scale for TMB", value = TRUE),
                                                   
                                                   actionButton("generate_tmb_plot", "Generate Plot", class = "btn-primary"),
                                                   
                                                   tags$br(), tags$br(),
                                                   
                                                   downloadButton("download_tmb_plot", "Download Plot", class = "btn-secondary"),
                                                   downloadButton("download_correlation_table", "Download Correlations", class = "btn-secondary"),
                                                   
                                                   tags$hr(),
                                                   
                                                   tags$div(
                                                     tags$h5(tags$b("Legend:")),
                                                     tags$p("• TMB: Tumor Mutational Burden (mutations per Mb)"),
                                                     tags$p("• sMF: Subclonal Mutational Fraction"),
                                                     tags$p("• R: Spearman correlation coefficient"),
                                                     tags$p("• Blue line: Linear regression with confidence interval")
                                                   )
                                                 ),
                                                 
                                                 mainPanel(
                                                   # TMB vs sMF plot output
                                                   div(class = "plot-container",
                                                       div(class = "plot-area",
                                                           plotlyOutput("tmb_smf_plot", height = "800px")
                                                       )
                                                   ),
                                                   
                                                   # Correlation summary table
                                                   tags$h4("Correlation Summary"),
                                                   DT::DTOutput("correlation_table"),
                                                   
                                                   # Additional information
                                                   uiOutput("tmb_smf_info")
                                                 )
                                               )
                                             )
                                    )
                        )
                      )
             ),
             
             # Page 8: Data Downloads
             tabPanel("Downloads",
                      tags$div(
                        tags$head(
                          tags$style(HTML("
                            .download-container {
                              max-width: 1200px;
                              margin: 0 auto;
                            }
                            
                            .download-intro {
                              background-color: #f8f9fa;
                              padding: 20px;
                              border-radius: 5px;
                              margin-bottom: 30px;
                              border-left: 4px solid #3498db;
                            }
                            
                            .data-section {
                              margin-bottom: 40px;
                            }
                            
                            .download-table {
                              width: 100%;
                              border-collapse: collapse;
                              margin: 20px 0;
                              background-color: white;
                              box-shadow: 0 1px 3px rgba(0,0,0,0.2);
                            }
                            
                            .download-table th {
                              background-color: #34495e;
                              color: white;
                              padding: 12px 15px;
                              text-align: left;
                              font-weight: 600;
                            }
                            
                            .download-table td {
                              padding: 10px 15px;
                              border-bottom: 1px solid #ddd;
                            }
                            
                            .download-table tr:hover {
                              background-color: #f5f5f5;
                            }
                            
                            .download-link {
                              color: #3498db;
                              text-decoration: none;
                              font-weight: 500;
                            }
                            
                            .download-link:hover {
                              color: #2980b9;
                              text-decoration: underline;
                            }
                            
                            .file-size {
                              color: #7f8c8d;
                              font-size: 0.9em;
                            }
                            
                            .description {
                              color: #555;
                              font-style: italic;
                            }
                            
                            .cancer-type {
                              font-weight: 600;
                              color: #2c3e50;
                            }
                            
                            .metadata {
                              background-color: #ecf0f1;
                              padding: 15px;
                              border-radius: 5px;
                              margin: 15px 0;
                            }
                            
                            .download-nav {
                              background-color: #34495e;
                              padding: 15px;
                              border-radius: 5px;
                              margin-bottom: 20px;
                            }
                            
                            .download-nav ul {
                              list-style: none;
                              margin: 0;
                              padding: 0;
                              display: flex;
                              flex-wrap: wrap;
                            }
                            
                            .download-nav li {
                              margin-right: 30px;
                              margin-bottom: 5px;
                            }
                            
                            .download-nav a {
                              color: white;
                              text-decoration: none;
                              font-weight: 500;
                            }
                            
                            .download-nav a:hover {
                              color: #3498db;
                            }
                          "))
                        ),
                        
                        div(class = "download-container",
                            tags$h3(tags$b("CliPP Data Downloads")),
                            
                            # Quick navigation
                            div(class = "download-nav",
                                tags$ul(
                                  tags$li(tags$a(href = "#overview", "Overview")),
                                  tags$li(tags$a(href = "#simulation", "Simulation Data")),
                                  tags$li(tags$a(href = "#tcga", "TCGA Data")),
                                  tags$li(tags$a(href = "#pcawg", "PCAWG Data")),
                                  tags$li(tags$a(href = "#tools", "Analysis Tools"))
                                )
                            ),
                            
                            # Overview section
                            div(id = "overview", class = "download-intro",
                                p(tags$b("CliPP (Clonal structure Identification through Penalizing Pairwise differences)"), 
                                  " provides comprehensive datasets for cancer genomic analysis. Download processed data from major cancer genomic consortiums and simulation data:"),
                                tags$ul(
                                  tags$li(tags$b("Simulation (CliPPSim4k)"), " - Simulated cancer genomic data for testing and validation"),
                                  tags$li(tags$b("TCGA (The Cancer Genome Atlas)"), " - Multi-platform genomic data"),
                                  tags$li(tags$b("PCAWG (Pan-Cancer Analysis of Whole Genomes)"), " - Whole genome sequencing data")
                                ),
                                p("All datasets include clonal structure analysis results, mutation assignments, and sample metadata.")
                            ),
                            
                            # Simulation section
                            div(id = "simulation", class = "data-section",
                                tags$h4(tags$b("Simulation Dataset Downloads (CliPPSim4k)")),
                                
                                div(class = "metadata",
                                    tags$b("Dataset Information:"),
                                    tags$ul(
                                      tags$li("Sample count: 4,000 simulated cancer genomes"),
                                      tags$li("Simulation types: Various clonal architectures and mutation patterns"),
                                      tags$li("Data processing: CliPP clonal structure analysis"),
                                      tags$li("File formats: TSV, TXT")
                                    )
                                ),
                                
                                tags$table(class = "download-table",
                                           tags$thead(
                                             tags$tr(
                                               tags$th("Dataset"),
                                               tags$th("Description"),
                                               tags$th("File Type"),
                                               tags$th("Download")
                                             )
                                           ),
                                           tags$tbody(
                                             tags$tr(
                                               tags$td(class = "cancer-type", "CliPPSim4k Summary"),
                                               tags$td(class = "description", "Comprehensive simulation results for 4,000 samples with known ground truth"),
                                               tags$td("TSV"),
                                               tags$td(a(href = "dataSource/CliPPSim4k.tsv", class = "download-link", download = "CliPPSim4k.tsv", "Download"), 
                                                       tags$span(class = "file-size", " (~2MB)"))
                                             ),
                                             tags$tr(
                                               tags$td(class = "cancer-type", "Individual Simulation Data"),
                                               tags$td(class = "description", "Per-sample simulation files with ground truth clonal structure"),
                                               tags$td("Multiple TXT files"),
                                               tags$td(a(href = "dataSource/Simulation/", class = "download-link", "Browse Folder"))
                                             )
                                           )
                                )
                            ),
                            
                            # TCGA section
                            div(id = "tcga", class = "data-section",
                                tags$h4(tags$b("TCGA Dataset Downloads")),
                                
                                div(class = "metadata",
                                    tags$b("Dataset Information:"),
                                    tags$ul(
                                      tags$li("Sample count: ~10,000+ cancer samples"),
                                      tags$li("Cancer types: 33 cancer types"),
                                      tags$li("Data types: Multi-platform genomic data with CliPP analysis"),
                                      tags$li("File formats: TSV, TXT")
                                    )
                                ),
                                
                                tags$table(class = "download-table",
                                           tags$thead(
                                             tags$tr(
                                               tags$th("Dataset"),
                                               tags$th("Description"),
                                               tags$th("File Type"),
                                               tags$th("Download")
                                             )
                                           ),
                                           tags$tbody(
                                             tags$tr(
                                               tags$td(class = "cancer-type", "CliPP TCGA Summary"),
                                               tags$td(class = "description", "Comprehensive CliPP analysis results for all TCGA samples"),
                                               tags$td("TSV"),
                                               tags$td(a(href = "dataSource/CliPP_TCGA.tsv", class = "download-link", download = "CliPP_TCGA.tsv", "Download"), 
                                                       tags$span(class = "file-size", " (~5MB)"))
                                             ),
                                             tags$tr(
                                               tags$td(class = "cancer-type", "Individual Sample Data"),
                                               tags$td(class = "description", "Per-sample mutation assignments and clonal structure"),
                                               tags$td("Multiple TXT files"),
                                               tags$td(a(href = "dataSource/TCGA/", class = "download-link", "Browse Folder"))
                                             )
                                           )
                                )
                            ),
                            
                            # PCAWG section
                            div(id = "pcawg", class = "data-section",
                                tags$h4(tags$b("PCAWG Dataset Downloads")),
                                
                                div(class = "metadata",
                                    tags$b("Dataset Information:"),
                                    tags$ul(
                                      tags$li("Sample count: ~2,600 cancer whole genomes"),
                                      tags$li("Cancer types: 38 tumor types across multiple sites"),
                                      tags$li("Data processing: CliPP clonal structure analysis with multiple λ parameters"),
                                      tags$li("File formats: TSV, TXT")
                                    )
                                ),
                                
                                tags$table(class = "download-table",
                                           tags$thead(
                                             tags$tr(
                                               tags$th("Dataset"),
                                               tags$th("Description"),
                                               tags$th("File Type"),
                                               tags$th("Download")
                                             )
                                           ),
                                           tags$tbody(
                                             tags$tr(
                                               tags$td(class = "cancer-type", "CliPP PCAWG Summary"),
                                               tags$td(class = "description", "Comprehensive CliPP analysis results for all PCAWG samples"),
                                               tags$td("TSV"),
                                               tags$td(a(href = "dataSource/CliPP_PCAWG.tsv", class = "download-link", download = "CliPP_PCAWG.tsv", "Download"), 
                                                       tags$span(class = "file-size", " (~2.5MB)"))
                                             ),
                                             tags$tr(
                                               tags$td(class = "cancer-type", "Individual Sample Data"),
                                               tags$td(class = "description", "Per-sample mutation assignments and clonal structure"),
                                               tags$td("Multiple TXT files"),
                                               tags$td(a(href = "dataSource/PCAWG/", class = "download-link", "Browse Folder"))
                                             )
                                           )
                                ),
                                
                            ),
                            
                            # Analysis Tools section
                            div(id = "tools", class = "data-section",
                                tags$h4(tags$b("Analysis Tools & Software")),
                                
                                tags$table(class = "download-table",
                                           tags$thead(
                                             tags$tr(
                                               tags$th("Tool"),
                                               tags$th("Description"),
                                               tags$th("Platform"),
                                               tags$th("Access")
                                             )
                                           ),
                                           tags$tbody(
                                             tags$tr(
                                               tags$td(class = "cancer-type", "CliPP Python Package"),
                                               tags$td(class = "description", "Command-line interface for CliPP analysis"),
                                               tags$td("Python"),
                                               tags$td(a(href = "forCliPP/CliPP/", class = "download-link", "Browse Source"))
                                             ),
                                             tags$tr(
                                               tags$td(class = "cancer-type", "Github Repository"),
                                               tags$td(class = "description", "Source code and documentation"),
                                               tags$td("Git"),
                                               tags$td(a(href = "https://github.com/your-repo/CliPP", class = "download-link", "View on Github"))
                                             ),
                                             tags$tr(
                                               tags$td(class = "cancer-type", "CliPP Docker"),
                                               tags$td(class = "description", "Containerized CliPP environment"),
                                               tags$td("Docker"),
                                               tags$td(a(href = "forCliPP/CliPP/Dockerfile", class = "download-link", "View Dockerfile"))
                                             )
                                           )
                                )
                            ),
                            
                            # Usage and Citation
                            div(class = "metadata",
                                tags$h5(tags$b("Data Usage & Citation")),
                                p(tags$b("If you use this data in your research, please cite:")),
                                tags$ul(
                                  tags$li("CliPP methodology and software publication"),
                                  tags$li("PCAWG Consortium publications for PCAWG data"),
                                  tags$li("TCGA Research Network publications for TCGA data")
                                ),
                                p(tags$b("Data Usage:"), " All data is provided for research purposes. Please follow the data usage policies of the respective consortiums."),
                                p(tags$b("Last Updated:"), " August 2025"),
                                p(tags$b("Contact:"), " For questions about the datasets, please contact the development team via the About page.")
                            )
                        )
                      )
             ),
             
             # Page 8: Citations for related studies and resources
             tabPanel("Citations",
                      tags$div(
                        tags$h4(tags$b(tags$span("Manuscript under preparation:"))),
                        p("Yujie Jiang, Matthew D Montierth, Kaixian Yu, Shuangxi Ji,
                        Quang Tran, Xiaoqian Liu, Jessica C Lal, Shuai Guo, Aaron Wu,
                        Seung Jun Shin, Shaolong Cao, Ruonan Li, Yuxin Tang, Tom Lesluyes,
                        Scott Kopetz, Pavlos Msaouel, Anil K. Sood, Christopher Jones, Jaffer Ajani,
                        Sumit K Subudhi, Ana Aparicio, Padmanee Sharma, John Paul Shen, Marek Kimmel,
                        Jennifer R. Wang, Maxime Tarabichi, Rebecca Fitzgerald, Peter Van Loo, Hongtu Zhu,
                        Wenyi Wang. Subclonal mutational load predicts survival and response to immunotherapy
                        in cancers with low to moderate TMB.",
                          a(href = "https://www.biorxiv.org/content/10.1101/2024.07.03.601939v1",
                            "https://www.biorxiv.org/content/10.1101/2024.07.03.601939v1"),"."),
                        
                        tags$br(),
                        tags$h4(tags$b(tags$span("TMB vs sMF Analysis:"))),
                        p("The TMB vs sMF analysis module in this application reproduces the analysis shown in Supplementary Figure 12 of the above manuscript. 
                        This feature enables researchers to explore the relationship between tumor mutational burden (TMB) and subclonal mutational fraction (sMF) 
                        across different cohorts including TCGA, PCAWG, OCCAMS, NCT02113657 (IPI), and NCT02703623 (ARPi+IPI). 
                        The analysis includes Spearman correlation coefficients, statistical significance testing, and interactive visualization capabilities.")
                      )
             )
  ))
