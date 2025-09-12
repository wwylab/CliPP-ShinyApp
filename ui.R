library(shiny)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(DT)

# UI for the Shiny application
ui <- fluidPage(
  tags$head(includeHTML("google-analytics.html")),
  shinyjs::useShinyjs(),
  
  # Basic CSS styling - simplified for functionality
  tags$head(
    tags$style(HTML("
      /* Keep existing controls positioning */
      .controls-area {
        position: absolute;
        top: 28%;
        left: 86%;
        width: 20%;
        min-width: 120px;
        max-width: 220px;
        background: #f8f9fa;
        padding: 20px;
        border-radius: 5px;
        z-index: 100;
        border: 1px solid #ddd;
      }
      
      .controls-area-depth {
        top: -10%;
      }
      
      /* Downloads Button - inline with navbar */
      .navbar-downloads-btn {
        background: #5bc0de;
        color: white;
        border: none;
        border-radius: 4px;
        padding: 8px 16px;
        font-size: 14px;
        cursor: pointer;
        margin: 8px 15px;
        float: right;
      }
      
      .navbar-downloads-btn:hover {
        background: #46b8da;
        color: white;
      }
      
      /* Adjust navbar to accommodate inline button */
      .navbar .navbar-nav {
        max-width: calc(100% - 140px); /* Reduce navbar tabs width */
      }
      
      .navbar .navbar-header {
        float: left;
      }
      
      .navbar .navbar-collapse {
        padding-right: 0;
      }
      
      /* Downloads Modal - keep functional */
      .downloads-modal {
        display: none;
        position: fixed;
        z-index: 1001;
        left: 0;
        top: 0;
        width: 100%;
        height: 100%;
        background: rgba(0, 0, 0, 0.5);
        overflow-y: auto;
      }
      
      .downloads-modal-content {
        background: white;
        margin: 5% auto;
        padding: 0;
        border-radius: 5px;
        width: 90%;
        max-width: 1200px;
        max-height: 80vh;
        overflow-y: auto;
      }
      
      .downloads-modal-header {
        background: #f5f5f5;
        padding: 15px 20px;
        border-bottom: 1px solid #ddd;
      }
      
      .downloads-modal-body {
        padding: 20px;
      }
      
      .downloads-close {
        float: right;
        font-size: 24px;
        cursor: pointer;
        color: #999;
      }
      
      .downloads-close:hover {
        color: #333;
      }
      
      /* Data Exploration Page Styling */
      .info-panel {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-left: 4px solid #3498db;
        border-radius: 8px;
        padding: 20px;
        margin: 15px 0;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
      
      .legend-panel {
        background: #fff;
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 15px;
        margin: 15px 0;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      
      .legend-panel h5 {
        color: #2c3e50;
        font-weight: 600;
        margin-bottom: 12px;
        padding-bottom: 8px;
        border-bottom: 2px solid #3498db;
      }
      
      .legend-item {
        display: flex;
        align-items: center;
        margin: 8px 0;
        padding: 5px 0;
      }
      
      .legend-symbol {
        width: 20px;
        height: 15px;
        margin-right: 10px;
        border-radius: 3px;
        flex-shrink: 0;
      }
      
      .legend-text {
        color: #555;
        font-size: 14px;
        line-height: 1.4;
      }
      
      .analysis-summary {
        background: linear-gradient(135deg, rgba(52, 152, 219, 0.05) 0%, rgba(155, 89, 182, 0.05) 100%);
        border: 1px solid rgba(52, 152, 219, 0.2);
        border-radius: 8px;
        padding: 20px;
        margin: 20px 0;
      }
      
      .sample-info-card {
        background: #fff;
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 15px;
        margin: 15px 0;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      
      .data-exploration-main {
        background: #fff;
        border-radius: 12px;
        padding: 30px;
        margin: 20px 0;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        min-height: 600px;
      }
      
      .plot-section {
        background: #fafbfc;
        border: 1px solid #e1e4e8;
        border-radius: 10px;
        padding: 25px;
        margin: 20px 0;
      }
      
      .correlation-table-container {
        background: #fff;
        border-radius: 8px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #e0e0e0;
        box-shadow: 0 2px 6px rgba(0,0,0,0.05);
      }
      
      .instructions-card {
        background: linear-gradient(135deg, #e8f4f8 0%, #d6eaf8 100%);
        border-left: 4px solid #3498db;
        border-radius: 8px;
        padding: 15px 20px;
        margin: 15px 0;
      }
      
      .instructions-card .fa {
        color: #3498db;
        margin-right: 8px;
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
        
        // Downloads modal functionality - use event delegation for dynamically added button
        $(document).on('click', '#floating-downloads-btn', function() {
          $('#downloads-modal').show();
        });
        
        $('.downloads-close').click(function() {
          $('#downloads-modal').hide();
        });
        
        $(window).click(function(event) {
          if (event.target.id === 'downloads-modal') {
            $('#downloads-modal').hide();
          }
        });
      });
    "))
  ),
  
  # Title and attribution for research purposes
  titlePanel("CliPP-on-Web: Scalable Subclonal Reconstruction for Cancer Research"),
  div(p("Copyright ", HTML("&copy;"), " 2025 The University of Texas MD Anderson Cancer Center. All rights reserved.")),
  
  # Downloads modal
  tags$div(id = "downloads-modal", class = "downloads-modal",
           tags$div(class = "downloads-modal-content",
                    tags$div(class = "downloads-modal-header",
                             tags$span(class = "downloads-close", HTML("&times;")),
                             tags$h3("CliPP Data Downloads")
                    ),
                    tags$div(class = "downloads-modal-body",
                             # Downloads content will be inserted here
                             div(class = "download-container",
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
           )
  ),
  
  # Multi-page navigation structure
  navbarPage("",
             header = tags$script(HTML("
               $(document).ready(function() {
                 $('.navbar .container-fluid').append('<button id=\"floating-downloads-btn\" class=\"navbar-downloads-btn\">Downloads</button>');
               });
             ")),
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
             
             # Page 2: Feature-Based Video Tutorials
             tabPanel("Tutorial",
                      tags$div(
                        tags$h4(tags$b("CliPP-on-Web Video Tutorials")),
                        tags$p("Learn CliPP-on-Web through focused, feature-specific video tutorials. Each video covers a specific aspect of the application."),
                        
                        # Overall Introduction
                        tags$div(
                          class = "tutorial-section",
                          style = "border: 2px solid #3498db; border-radius: 8px; padding: 15px; margin: 15px 0; background-color: #f8f9fa;",
                          tags$h5(tags$b("🎬 Complete Overview"), style = "color: #2c3e50;"),
                          tags$p("Start here for a comprehensive introduction to CliPP-on-Web:"),
                          tags$iframe(src="https://www.youtube.com/embed/NTJX33csjwA",
                                      style="width:100%; height:400px; border:none; border-radius: 5px;"),
                          tags$p(tags$em("Duration: ~15 minutes"), style = "color: #7f8c8d; margin-top: 10px;")
                        ),
                        
                        tags$hr(),
                        
                        # Feature-specific tutorials (hidden for now)
                        hidden(
                          tags$h4(tags$b("Feature-Specific Tutorials")),
                          tags$p("Deep dive into individual features with these focused tutorials:"),
                          
                          # Row 1: Data Upload and Resources
                          tags$div(
                            class = "tutorial-row", 
                            style = "display: flex; gap: 20px; margin: 20px 0; flex-wrap: wrap;",
                            
                            # Tutorial 1: Data Upload
                            tags$div(
                              class = "tutorial-card",
                              style = "flex: 1; min-width: 300px; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                              tags$h6(tags$b("1️⃣ Data Upload & File Formats"), style = "color: #e74c3c;"),
                              tags$div(
                                style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; height: 120px; display: flex; align-items: center; justify-content: center; border: 2px dashed #ccc;",
                                tags$span("📹 Video Placeholder", style = "color: #999; font-size: 18px;")
                              ),
                              tags$p("Learn to upload and validate your data files:", style = "font-size: 14px;"),
                              tags$ul(
                                style = "font-size: 13px; color: #555;",
                                tags$li("SNV, CNA, and purity file requirements"),
                                tags$li("File validation and error handling"),
                                tags$li("Multiple sample upload modes")
                              ),
                              tags$p(tags$em("Target: 5-7 minutes"), style = "color: #7f8c8d; font-size: 12px;")
                            ),
                            
                            # Tutorial 2: TCGA Resources
                            tags$div(
                              class = "tutorial-card",
                              style = "flex: 1; min-width: 300px; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                              tags$h6(tags$b("2️⃣ TCGA Data Exploration"), style = "color: #3498db;"),
                              tags$div(
                                style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; height: 120px; display: flex; align-items: center; justify-content: center; border: 2px dashed #ccc;",
                                tags$span("📹 Video Placeholder", style = "color: #999; font-size: 18px;")
                              ),
                              tags$p("Navigate TCGA datasets effectively:", style = "font-size: 14px;"),
                              tags$ul(
                                style = "font-size: 13px; color: #555;",
                                tags$li("Cancer type and cohort selection"),
                                tags$li("Sample filtering and comparison"),
                                tags$li("Visualization customization")
                              ),
                              tags$p(tags$em("Target: 6-8 minutes"), style = "color: #7f8c8d; font-size: 12px;")
                            )
                          ),
                          
                          # Row 2: PCAWG and Driver Mutations
                          tags$div(
                            class = "tutorial-row", 
                            style = "display: flex; gap: 20px; margin: 20px 0; flex-wrap: wrap;",
                            
                            # Tutorial 3: PCAWG
                            tags$div(
                              class = "tutorial-card",
                              style = "flex: 1; min-width: 300px; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                              tags$h6(tags$b("3️⃣ PCAWG Whole Genome Analysis"), style = "color: #9b59b6;"),
                              tags$div(
                                style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; height: 120px; display: flex; align-items: center; justify-content: center; border: 2px dashed #ccc;",
                                tags$span("📹 Video Placeholder", style = "color: #999; font-size: 18px;")
                              ),
                              tags$p("Master PCAWG whole genome data:", style = "font-size: 14px;"),
                              tags$ul(
                                style = "font-size: 13px; color: #555;",
                                tags$li("Whole genome vs exome differences"),
                                tags$li("Lambda parameter optimization"),
                                tags$li("Large-scale genomic patterns")
                              ),
                              tags$p(tags$em("Target: 7-9 minutes"), style = "color: #7f8c8d; font-size: 12px;")
                            ),
                            
                            # Tutorial 4: Driver Mutations
                            tags$div(
                              class = "tutorial-card",
                              style = "flex: 1; min-width: 300px; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                              tags$h6(tags$b("4️⃣ Driver Mutation Analysis"), style = "color: #f39c12;"),
                              tags$div(
                                style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; height: 120px; display: flex; align-items: center; justify-content: center; border: 2px dashed #ccc;",
                                tags$span("📹 Video Placeholder", style = "color: #999; font-size: 18px;")
                              ),
                              tags$p("Analyze driver mutation subclonality:", style = "font-size: 14px;"),
                              tags$ul(
                                style = "font-size: 13px; color: #555;",
                                tags$li("Identifying key driver mutations"),
                                tags$li("Clonal vs subclonal classification"),
                                tags$li("VAF and cellular prevalence plots")
                              ),
                              tags$p(tags$em("Target: 6-8 minutes"), style = "color: #7f8c8d; font-size: 12px;")
                            )
                          ),
                          
                          # Row 3: Survival Analysis and Scatter Plots
                          tags$div(
                            class = "tutorial-row", 
                            style = "display: flex; gap: 20px; margin: 20px 0; flex-wrap: wrap;",
                            
                            # Tutorial 5: Kaplan-Meier Survival
                            tags$div(
                              class = "tutorial-card",
                              style = "flex: 1; min-width: 300px; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                              tags$h6(tags$b("5️⃣ Kaplan-Meier Survival Analysis"), style = "color: #27ae60;"),
                              tags$div(
                                style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; height: 120px; display: flex; align-items: center; justify-content: center; border: 2px dashed #ccc;",
                                tags$span("📹 Video Placeholder", style = "color: #999; font-size: 18px;")
                              ),
                              tags$p("Connect mutations to patient outcomes:", style = "font-size: 14px;"),
                              tags$ul(
                                style = "font-size: 13px; color: #555;",
                                tags$li("Subclonal mutational fraction (sMF)"),
                                tags$li("Survival curve interpretation"),
                                tags$li("Statistical significance testing")
                              ),
                              tags$p(tags$em("Target: 8-10 minutes"), style = "color: #7f8c8d; font-size: 12px;")
                            ),
                            
                            # Tutorial 6: Scatter Plot Analysis
                            tags$div(
                              class = "tutorial-card",
                              style = "flex: 1; min-width: 300px; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                              tags$h6(tags$b("6️⃣ TMB vs sMF Scatter Analysis"), style = "color: #e67e22;"),
                              tags$div(
                                style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; height: 120px; display: flex; align-items: center; justify-content: center; border: 2px dashed #ccc;",
                                tags$span("📹 Video Placeholder", style = "color: #999; font-size: 18px;")
                              ),
                              tags$p("Explore TMB and subclonality relationships:", style = "font-size: 14px;"),
                              tags$ul(
                                style = "font-size: 13px; color: #555;",
                                tags$li("Tumor mutational burden patterns"),
                                tags$li("Correlation analysis across cohorts"),
                                tags$li("2x2 grid visualization optimization")
                              ),
                              tags$p(tags$em("Target: 5-7 minutes"), style = "color: #7f8c8d; font-size: 12px;")
                            )
                          ),
                          
                          # Row 4: Advanced Features
                          tags$div(
                            class = "tutorial-row", 
                            style = "display: flex; gap: 20px; margin: 20px 0; flex-wrap: wrap;",
                            
                            # Tutorial 7: Parameter Tuning
                            tags$div(
                              class = "tutorial-card",
                              style = "flex: 1; min-width: 300px; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                              tags$h6(tags$b("7️⃣ Advanced Parameter Tuning"), style = "color: #8e44ad;"),
                              tags$div(
                                style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; height: 120px; display: flex; align-items: center; justify-content: center; border: 2px dashed #ccc;",
                                tags$span("📹 Video Placeholder", style = "color: #999; font-size: 18px;")
                              ),
                              tags$p("Optimize CliPP analysis parameters:", style = "font-size: 14px;"),
                              tags$ul(
                                style = "font-size: 13px; color: #555;",
                                tags$li("Smoothing factor optimization"),
                                tags$li("Handling low-purity samples"),
                                tags$li("Multi-lambda parameter selection")
                              ),
                              tags$p(tags$em("Target: 9-12 minutes"), style = "color: #7f8c8d; font-size: 12px;")
                            ),
                            
                            # Tutorial 8: Data Export
                            tags$div(
                              class = "tutorial-card",
                              style = "flex: 1; min-width: 300px; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                              tags$h6(tags$b("8️⃣ Data Export & Downloads"), style = "color: #16a085;"),
                              tags$div(
                                style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; height: 120px; display: flex; align-items: center; justify-content: center; border: 2px dashed #ccc;",
                                tags$span("📹 Video Placeholder", style = "color: #999; font-size: 18px;")
                              ),
                              tags$p("Export results for further analysis:", style = "font-size: 14px;"),
                              tags$ul(
                                style = "font-size: 13px; color: #555;",
                                tags$li("Plot downloads and customization"),
                                tags$li("Data table exports"),
                                tags$li("Accessing pre-computed datasets")
                              ),
                              tags$p(tags$em("Target: 4-6 minutes"), style = "color: #7f8c8d; font-size: 12px;")
                            )
                          ),
                          
                          tags$br()
                        ), # End of hidden feature-specific tutorials
                        
                        # Tutorial Notes and Future Plans
                        tags$div(
                          style = "background-color: #e8f4f8; padding: 20px; border-radius: 8px; border-left: 4px solid #3498db; margin: 20px 0;",
                          tags$h5(tags$b("📋 Tutorial Development Status"), style = "color: #2c3e50; margin-bottom: 15px;"),
                          tags$p(tags$b("Current Status:"), " Feature-based tutorial videos are being developed to provide focused, bite-sized learning experiences."),
                          tags$p(tags$b("Coming Soon:"), " Each tutorial will be 4-12 minutes long, focusing on specific functionality to help you master CliPP-on-Web efficiently."),
                          tags$p(tags$b("For Now:"), " Watch the complete overview video above for comprehensive guidance, or explore the application directly using the sample data provided."),
                          tags$hr(style = "border-color: #3498db; margin: 15px 0;"),
                          tags$p(tags$b("Questions?"), " Contact our support team through the About page for assistance with specific features.")
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
             
             # Page 6: Data Exploration
             tabPanel("Data Exploration",
                      tags$div(class = "data-exploration-main",
                               tabsetPanel(id = "analysis_tabset",
                                           # Sub-tab for Kaplan-Meier survival plots
                                           tabPanel("Kaplan–Meier",
                                                    tags$div(
                                                      tags$h3(tags$b("Kaplan-Meier Survival Analysis"), style = "color: #2c3e50; margin-bottom: 10px;"),
                                                      tags$p("Explore survival outcomes based on subclonal mutational fraction (sMF) across different cancer types and subtypes.", 
                                                             style = "color: #7f8c8d; font-size: 16px; margin-bottom: 30px;"),
                                                      
                                                      # Instructions panel
                                                      tags$div(
                                                        class = "instructions-card",
                                                        tags$h5(tags$b("Analysis Instructions"), style = "color: #2c3e50; margin-bottom: 15px;"),
                                                        tags$div(
                                                          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-top: 10px;",
                                                          tags$div(
                                                            tags$p("1. Select survival endpoint (PFI or OS)", style = "margin: 5px 0;"),
                                                            tags$p("2. Choose a cancer type to analyze", style = "margin: 5px 0;")
                                                          ),
                                                          tags$div(
                                                            tags$p("3. Generate the survival plot", style = "margin: 5px 0;"),
                                                            tags$p("4. Interpret results using legend below", style = "margin: 5px 0;")
                                                          )
                                                        ),
                                                        tags$p(tags$em("Note: The algorithm automatically determines optimal sMF cutoffs using decision tree analysis."), 
                                                               style = "margin-top: 15px; color: #666; font-size: 14px;")
                                                      ),
                                                      
                                                      # Controls and Plot Layout
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
                                                               
                                                               # Legend Panel
                                                               tags$div(class = "legend-panel",
                                                                        tags$h5(tags$b("Legend & Interpretation")),
                                                                        tags$div(class = "legend-item",
                                                                                 tags$div(class = "legend-symbol", style = "background: #63A299;"),
                                                                                 tags$div(class = "legend-text", "High sMF: Higher subclonal mutation burden")
                                                                        ),
                                                                        tags$div(class = "legend-item",
                                                                                 tags$div(class = "legend-symbol", style = "background: #D6ACC0;"),
                                                                                 tags$div(class = "legend-text", "Low sMF: Lower subclonal mutation burden")
                                                                        ),
                                                                        tags$div(class = "legend-item",
                                                                                 tags$div(class = "legend-symbol", style = "background: transparent; border: 1px solid #666;"),
                                                                                 tags$div(class = "legend-text", "sMF: Subclonal Mutational Fraction")
                                                                        ),
                                                                        tags$div(class = "legend-item",
                                                                                 tags$div(class = "legend-symbol", style = "background: #f39c12;"),
                                                                                 tags$div(class = "legend-text", "P-value: Log-rank test significance")
                                                                        )
                                                               )
                                                        ),
                                                        
                                                        column(9,
                                                               # Plot Section
                                                               tags$div(class = "plot-section",
                                                                        plotlyOutput("survivalPlot", height = "600px")
                                                               ),
                                                               
                                                               # Analysis Results
                                                               tags$div(class = "analysis-summary",
                                                                        uiOutput("survivalPlotInfo")
                                                               ),
                                                               
                                                               # Sample Information
                                                               tags$div(class = "sample-info-card",
                                                                        uiOutput("survivalSampleInfo")
                                                               )
                                                        )
                                                      )
                                                    )
                                           ),
                                           
                                           # Sub-tab for TMB vs sMF scatter plots
                                           tabPanel("Scatter Plot",
                                                    tags$div(
                                                      tags$h3(tags$b("TMB vs Subclonal Mutational Fraction"), style = "color: #2c3e50; margin-bottom: 10px;"),
                                                      tags$p("Explore relationships between tumor mutational burden and subclonal mutations across different cohorts and cancer types.", 
                                                             style = "color: #7f8c8d; font-size: 16px; margin-bottom: 30px;"),
                                                      
                                                      # Instructions panel
                                                      tags$div(
                                                        class = "instructions-card",
                                                        tags$h5(tags$b("Analysis Instructions"), style = "color: #2c3e50; margin-bottom: 15px;"),
                                                        tags$div(
                                                          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-top: 10px;",
                                                          tags$div(
                                                            tags$p("1. Select a cohort to analyze", style = "margin: 5px 0;"),
                                                            tags$p("2. Choose cancer types (optional)", style = "margin: 5px 0;")
                                                          ),
                                                          tags$div(
                                                            tags$p("3. Toggle log scale if needed", style = "margin: 5px 0;"),
                                                            tags$p("4. Generate plots and view correlations", style = "margin: 5px 0;")
                                                          )
                                                        )
                                                      ),
                                                      
                                                      # Controls and Plot Layout
                                                      fluidRow(
                                                        column(3,
                                                               tags$div(class = "legend-panel",
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
                                                                        
                                                                        div(style = "text-align: center; margin: 20px 0;",
                                                                            actionButton("generate_tmb_plot", "Generate Plot", class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                                                                            downloadButton("download_tmb_plot", "Download Plot", class = "btn-secondary", style = "width: 100%; margin-bottom: 5px;"),
                                                                            downloadButton("download_correlation_table", "Download Data", class = "btn-secondary", style = "width: 100%;")
                                                                        )
                                                               ),
                                                               
                                                               # Legend Panel
                                                               tags$div(class = "legend-panel",
                                                                        tags$h5(tags$b("Legend & Metrics")),
                                                                        tags$div(class = "legend-item",
                                                                                 tags$div(class = "legend-symbol", style = "background: #3498db;"),
                                                                                 tags$div(class = "legend-text", "TMB: Tumor Mutational Burden (mutations/Mb)")
                                                                        ),
                                                                        tags$div(class = "legend-item",
                                                                                 tags$div(class = "legend-symbol", style = "background: #e74c3c;"),
                                                                                 tags$div(class = "legend-text", "sMF: Subclonal Mutational Fraction")
                                                                        ),
                                                                        tags$div(class = "legend-item",
                                                                                 tags$div(class = "legend-symbol", style = "background: #27ae60;"),
                                                                                 tags$div(class = "legend-text", "R: Spearman correlation coefficient")
                                                                        ),
                                                                        tags$div(class = "legend-item",
                                                                                 tags$div(class = "legend-symbol", style = "background: linear-gradient(45deg, #3498db, transparent); border: 1px solid #3498db;"),
                                                                                 tags$div(class = "legend-text", "Blue line: Regression with confidence interval")
                                                                        )
                                                               )
                                                        ),
                                                        
                                                        column(9,
                                                               # Plot Section
                                                               tags$div(class = "plot-section",
                                                                        plotlyOutput("tmb_smf_plot", height = "800px")
                                                               ),
                                                               
                                                               # Correlation Summary Table
                                                               tags$div(class = "correlation-table-container",
                                                                        tags$h4(tags$b("Correlation Summary"), style = "color: #2c3e50; margin-bottom: 15px;"),
                                                                        DT::DTOutput("correlation_table")
                                                               ),
                                                               
                                                               # Additional Analysis Information
                                                               tags$div(class = "analysis-summary",
                                                                        uiOutput("tmb_smf_info")
                                                               )
                                                        )
                                                      )
                                                    )
                                           )
                               )
                      )
             ),
             
             # Page 7: Citations for related studies and resources
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
