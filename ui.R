library(shiny)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(DT)

ui <- fluidPage(
  tags$head(includeHTML("google-analytics.html")),
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
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

      .downloads-tab-container {
        position: relative;
        min-height: 100vh;
        padding-left: 20px;
        padding-right: 20px;
      }

      .vertical-nav-floating {
        position: fixed;
        top: 50%;
        left: 20px;
        transform: translateY(-50%);
        width: 200px;
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        z-index: 1000;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
        transition: all 0.3s ease;
      }

      .vertical-nav-floating:hover {
        box-shadow: 0 6px 18px rgba(0, 0, 0, 0.15);
        transform: translateY(-50%) translateX(2px);
      }

      .vertical-nav-floating .nav-list {
        list-style: none;
        padding: 0;
        margin: 0;
      }

      .vertical-nav-floating .nav-list li {
        margin-bottom: 12px;
      }

      .vertical-nav-floating h5 {
        color: #495057;
        margin-bottom: 15px;
        font-weight: 600;
      }

      .vertical-nav-floating .nav-link {
        display: block;
        color: #495057;
        text-decoration: none;
        padding: 12px 16px;
        border-radius: 6px;
        transition: all 0.3s ease;
        font-weight: 500;
        font-size: 14px;
        border-left: 3px solid transparent;
      }

      .vertical-nav-floating .nav-link:hover {
        background: #e9ecef;
        border-left: 3px solid #6c757d;
        transform: translateX(4px);
        color: #212529;
        text-decoration: none;
      }

      .vertical-nav-floating .nav-link:active,
      .vertical-nav-floating .nav-link.active {
        background: #dee2e6;
        border-left: 3px solid #495057;
        color: #212529;
      }

      .downloads-main-content {
        margin-left: 260px;
        padding: 30px;
        max-width: calc(100% - 300px);
      }

      .download-section {
        margin-bottom: 40px;
        padding: 30px;
        background: #ffffff;
        border-radius: 8px;
        border: 1px solid #e9ecef;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.05);
        transition: all 0.3s ease;
      }

      .download-section:hover {
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08);
        transform: translateY(-1px);
      }

      .download-section h3 {
        margin-bottom: 25px;
        font-size: 24px;
        font-weight: 600;
      }

      .metadata-card {
        background: #f8f9fa;
        border-left: 4px solid #6c757d;
        border-radius: 6px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #e9ecef;
      }

      .download-table-styled {
        width: 100%;
        border-collapse: collapse;
        margin: 25px 0;
        font-size: 16px;
        border-radius: 12px;
        overflow: hidden;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
      }

      .download-table-styled thead tr {
        background: #495057;
        color: white;
        text-align: left;
        font-weight: 600;
      }

      .download-table-styled th,
      .download-table-styled td {
        padding: 15px 20px;
        border: none;
      }

      .download-table-styled tbody tr {
        border-bottom: 1px solid #e0e0e0;
        transition: all 0.3s ease;
      }

      .download-table-styled tbody tr:nth-of-type(even) {
        background: #f8f9fa;
      }

      .download-table-styled tbody tr:hover {
        background: #e9ecef;
        transform: scale(1.01);
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.1);
      }

      .download-table-styled tbody tr:last-of-type {
        border-bottom: none;
      }

      .dataset-name {
        font-weight: 600;
        color: #2c3e50;
      }

      .file-type-badge {
        display: inline-block;
        background: #6c757d;
        color: white;
        padding: 6px 12px;
        border-radius: 12px;
        font-size: 12px;
        font-weight: 500;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }

      .download-btn, .browse-btn {
        display: inline-block;
        padding: 10px 20px;
        border-radius: 25px;
        text-decoration: none;
        font-weight: 600;
        font-size: 14px;
        transition: all 0.3s ease;
        border: none;
        cursor: pointer;
        text-align: center;
      }

      .download-btn {
        background: #28a745;
        color: white;
        border: 1px solid #28a745;
      }

      .download-btn:hover {
        background: #218838;
        border-color: #1e7e34;
        transform: translateY(-1px);
        color: white;
        text-decoration: none;
      }

      .browse-btn {
        background: #6c757d;
        color: white;
        border: 1px solid #6c757d;
      }

      .browse-btn:hover {
        background: #545b62;
        border-color: #4e555b;
        transform: translateY(-1px);
        color: white;
        text-decoration: none;
      }

      .file-size {
        font-size: 12px;
        color: #7f8c8d;
        font-style: italic;
        margin-left: 10px;
      }

      .citation-section {
        background: #f8f9fa;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        padding: 30px;
        margin-top: 40px;
        border-left: 4px solid #6c757d;
      }

      @media (max-width: 1200px) {
        .vertical-nav-floating {
          width: 180px;
          left: 15px;
        }

        .downloads-main-content {
          margin-left: 220px;
          max-width: calc(100% - 250px);
        }
      }

      @media (max-width: 992px) {
        .vertical-nav-floating {
          position: relative;
          top: auto;
          left: auto;
          transform: none;
          width: 100%;
          margin-bottom: 30px;
        }

        .downloads-main-content {
          margin-left: 0;
          max-width: 100%;
        }
      }

      @media (max-width: 768px) {
        .download-table-styled {
          font-size: 14px;
        }

        .download-table-styled th,
        .download-table-styled td {
          padding: 12px 15px;
        }

        .download-btn, .browse-btn {
          padding: 8px 16px;
          font-size: 12px;
        }
      }

      .sample-count-info {
        background: linear-gradient(135deg, #e8f5e8, #d4edda);
        border: 1px solid #c3e6cb;
        border-radius: 8px;
        padding: 15px;
        margin: 15px 0;
        text-align: center;
        transition: all 0.3s ease;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
      }

      .sample-count-info:hover {
        background: linear-gradient(135deg, #d4edda, #c3e6cb);
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15);
      }

      .sample-count-number {
        font-size: 18px;
        font-weight: 600;
        color: #155724;
        margin-bottom: 5px;
        display: block;
      }

      .sample-count-label {
        font-size: 14px;
        color: #155724;
        font-weight: 500;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }

      .sample-count-details {
        font-size: 12px;
        color: #6c757d;
        margin-top: 8px;
        font-style: italic;
      }

      .sample-count-hidden {
        display: none;
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

        // Floating vertical navigation smooth scrolling
        $(document).on('click', '.vertical-nav-floating .nav-link', function(e) {
          e.preventDefault();
          var href = $(this).attr('href');
          var target = $(href);

          if (target.length) {
            $('.vertical-nav-floating .nav-link').removeClass('active');
            $(this).addClass('active');

            $('html, body').animate({
              scrollTop: target.offset().top - 120
            }, 800);
          }
        });

        // Highlight active section in navigation based on scroll position
        $(window).scroll(function() {
          if ($('.downloads-tab-container').length > 0 && $('.downloads-tab-container').is(':visible')) {
            var scrollPos = $(window).scrollTop() + 150;

            $('.download-section').each(function() {
              var section = $(this);
              if (section.offset()) {
                var sectionTop = section.offset().top;
                var sectionBottom = sectionTop + section.outerHeight();
                var sectionId = section.attr('id');

                if (scrollPos >= sectionTop && scrollPos < sectionBottom) {
                  $('.vertical-nav-floating .nav-link').removeClass('active');
                  $('.vertical-nav-floating .nav-link[href=\"#' + sectionId + '\"]').addClass('active');
                }
              }
            });
          }
        });
      });
    "))
  ),
  
  titlePanel("CliPP-on-Web: Scalable Subclonal Reconstruction for Cancer Research"),
  div(p("Copyright ", HTML("&copy;"), " 2025 The University of Texas MD Anderson Cancer Center. All rights reserved.")),
  
  
  navbarPage("",
             tabPanel("About",
                      tags$div(
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
                          a(href = "https://www.biorxiv.org/content/10.1101/2024.07.03.601939v2",
                            "https://www.biorxiv.org/content/10.1101/2024.07.03.601939v2"), "."),
                        tags$br(),
                        p(tags$b(tags$span(style = "color: #D22B2B;", "For issues with the app, please contact:"))),
                        p("Aaron Wu - ", a(href = "mailto:aw80@rice.edu", "aw80@rice.edu")),
                        p("Quang Tran - ", a(href = "mailto:qmtran@mdanderson.org", "qmtran@mdanderson.org"))
                      )
             ),
             
             tabPanel("Tutorial",
                      tags$div(
                        tags$h4(tags$b("CliPP-on-Web Video Tutorials")),
                        tags$p("Learn CliPP-on-Web through focused, feature-specific video tutorials. Each video covers a specific aspect of the application."),
                        
                        # Overall Introduction
                        hidden(
                          tags$div(
                            class = "tutorial-section",
                            style = "border: 2px solid #3498db; border-radius: 8px; padding: 15px; margin: 15px 0; background-color: #f8f9fa;",
                            tags$h5(tags$b("🎬 Complete Overview"), style = "color: #2c3e50;"),
                            tags$p("Start here for a comprehensive introduction to CliPP-on-Web:"),
                            tags$iframe(src="https://www.youtube.com/embed/NTJX33csjwA",
                                        style="width:100%; height:400px; border:none; border-radius: 5px;"),
                            tags$p(tags$em("Duration: ~15 minutes"), style = "color: #7f8c8d; margin-top: 10px;")
                          )
                        ),
                        
                      
                        # Row 1: Data Upload and Resources
                        tags$div(
                          class = "tutorial-row", 
                          style = "display: flex; gap: 20px; margin: 20px 0; flex-wrap: wrap;",
                          
                          # Tutorial 1: Data Upload
                          tags$div(
                            class = "tutorial-card",
                            style = "flex: 1; min-width: 300px; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                            tags$h6(tags$b("1️⃣ Data Upload & File Formats"), style = "color: #e74c3c;"),
                            tags$iframe(
                              src = "https://www.youtube.com/embed/J6wOEK8iznY",
                              style = "width: 100%; height: 180px; border: none; border-radius: 5px;",
                              frameborder = "0",
                              allowfullscreen = TRUE
                            ),
                            tags$p("Learn to upload and validate your data files:", style = "font-size: 14px;"),
                            tags$ul(
                              style = "font-size: 13px; color: #555;",
                              tags$li("SNV, CNA, and purity file requirements"),
                              tags$li("File validation and error handling"),
                              tags$li("Multiple sample upload modes")
                            )
                          ),
                          
                          # Tutorial 2: CliPP Data Resources (TCGA + PCAWG)
                          tags$div(
                            class = "tutorial-card",
                            style = "flex: 1; min-width: 300px; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                            tags$h6(tags$b("2️⃣ CliPP Data Resources"), style = "color: #3498db;"),
                            tags$iframe(
                              src = "https://www.youtube.com/embed/eOPzoNAjae8",
                              style = "width: 100%; height: 180px; border: none; border-radius: 5px;",
                              frameborder = "0",
                              allowfullscreen = TRUE
                            ),
                            tags$p("Navigate TCGA and PCAWG datasets:", style = "font-size: 14px;"),
                            tags$ul(
                              style = "font-size: 13px; color: #555;",
                              tags$li("TCGA multi-platform genomic data"),
                              tags$li("PCAWG whole-genome datasets"),
                              tags$li("Cancer type and cohort selection")
                            )
                          )
                        ),
                        
                        # Row 2: Driver Mutations and Data Exploration
                        tags$div(
                          class = "tutorial-row",
                          style = "display: flex; gap: 20px; margin: 20px 0; flex-wrap: wrap;",
                          
                          # Tutorial 3: Driver Mutations
                          tags$div(
                            class = "tutorial-card",
                            style = "flex: 1; min-width: 300px; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                            tags$h6(tags$b("3️⃣ Driver Mutation Analysis"), style = "color: #f39c12;"),
                            tags$iframe(
                              src = "https://www.youtube.com/embed/ZmFQxehEKL4",
                              style = "width: 100%; height: 180px; border: none; border-radius: 5px;",
                              frameborder = "0",
                              allowfullscreen = TRUE
                            ),
                            tags$p("Analyze driver mutation subclonality:", style = "font-size: 14px;"),
                            tags$ul(
                              style = "font-size: 13px; color: #555;",
                              tags$li("Identifying key driver mutations"),
                              tags$li("Clonal vs subclonal classification"),
                              tags$li("VAF and cellular prevalence plots")
                            )
                          ),
                          
                          # Tutorial 4: Data Exploration (KM + Heatmap)
                          tags$div(
                            class = "tutorial-card",
                            style = "flex: 1; min-width: 300px; border: 1px solid #ddd; border-radius: 8px; padding: 15px; background-color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                            tags$h6(tags$b("4️⃣ Data Exploration"), style = "color: #27ae60;"),
                            tags$iframe(
                              src = "https://www.youtube.com/embed/_AtFySOoC2g",
                              style = "width: 100%; height: 180px; border: none; border-radius: 5px;",
                              frameborder = "0",
                              allowfullscreen = TRUE
                            ),
                            tags$p("Advanced analysis and visualization:", style = "font-size: 14px;"),
                            tags$ul(
                              style = "font-size: 13px; color: #555;",
                              tags$li("Kaplan-Meier survival analysis"),
                              tags$li("sMF correlation heatmaps"),
                              tags$li("Multi-cohort comparison tools")
                            )
                          )
                        ),
                        
                      )
             ),
             
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

                            # Smoothing factor controls for CliPP-on-Web
                            conditionalPanel(
                              condition = "output.plotsReadyCliPP == true",
                              sliderInput("smoothingFactor1", "Smoothing Factor (VAF Plot):",
                                          min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                              div(class = "recommended-smoothing",
                                  uiOutput("smoothingExplanation1")
                              ),
                              sliderInput("smoothingFactor2", "Smoothing Factor (CP Plot):",
                                          min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                              div(class = "recommended-smoothing",
                                  uiOutput("smoothingExplanation2")
                              )
                            ),

                            hidden(actionButton("clear", "Clear", class = "btn-danger"))
                          ),
                          
                          mainPanel(
                            div(class = "plot-container",
                                div(class = "plot-area",
                                    plotlyOutput("plot1")
                                ),
                                div(class = "caption-area",
                                    uiOutput("caption1")
                                )
                            ),
                            
                            div(class = "plot-container",
                                div(class = "plot-area",
                                    plotlyOutput("plot2")
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
             
             tabPanel("CliPP Data Resources",
                      tags$div(
                        tabsetPanel(id = "tabset",
                                    tabPanel("TCGA",
                                             tags$div(
                                               tags$h4(tags$b("TCGA Select")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   uiOutput("cancer_type_ui_TCGA"),
                                                   
                                                   div(id = "sample_count_display_TCGA", class = "sample-count-info",
                                                       uiOutput("sample_count_TCGA")
                                                   ),
                                                   
                                                   uiOutput("sample_name_ui_TCGA"),
                                                   actionButton("submitTCGA", "Submit", class = "btn-primary"),
                                                   hidden(actionButton("clearTCGA", "Clear", class = "btn-danger")),
                                                   
                                                   # Smoothing factor controls for TCGA
                                                   conditionalPanel(
                                                     condition = "output.plotsReadyTCGA == true",
                                                     sliderInput("smoothingFactor1_TCGA", "Smoothing Factor (VAF Plot):",
                                                                 min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                                                     div(class = "recommended-smoothing",
                                                         uiOutput("smoothingExplanation1_TCGA")
                                                     ),
                                                     sliderInput("smoothingFactor2_TCGA", "Smoothing Factor (CP Plot):",
                                                                 min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                                                     div(class = "recommended-smoothing",
                                                         uiOutput("smoothingExplanation2_TCGA")
                                                     )
                                                   ),
                                                   
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
                                                       div(class = "caption-area",
                                                           uiOutput("caption1TCGA")
                                                       )
                                                   ),
                                                   
                                                   # Second TCGA plot
                                                   div(class = "plot-container",
                                                       div(class = "plot-area",
                                                           plotlyOutput("plot2TCGA")
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
                                    
                                    tabPanel("PCAWG",
                                             tags$div(
                                               tags$h4(tags$b("PCAWG Select")),
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   uiOutput("cancer_type_ui_PCAWG"),
                                                   
                                                   # Sample count display
                                                   div(id = "sample_count_display_PCAWG", class = "sample-count-info",
                                                       uiOutput("sample_count_PCAWG")
                                                   ),
                                                   
                                                   uiOutput("sample_name_ui_PCAWG"),
                                                   actionButton("submitPCAWG", "Submit", class = "btn-primary"),
                                                   hidden(actionButton("clearPCAWG", "Clear", class = "btn-danger")),
                                                   
                                                   # Smoothing factor controls for PCAWG
                                                   conditionalPanel(
                                                     condition = "output.plotsReadyPCAWG == true",
                                                     sliderInput("smoothingFactor1_PCAWG", "Smoothing Factor (VAF Plot):",
                                                                 min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                                                     div(class = "recommended-smoothing",
                                                         uiOutput("smoothingExplanation1_PCAWG")
                                                     ),
                                                     sliderInput("smoothingFactor2_PCAWG", "Smoothing Factor (CP Plot):",
                                                                 min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                                                     div(class = "recommended-smoothing",
                                                         uiOutput("smoothingExplanation2_PCAWG")
                                                     )
                                                   ),
                                                   
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
                                                       div(class = "caption-area",
                                                           uiOutput("caption1PCAWG")
                                                       )
                                                   ),
                                                   
                                                   # Second PCAWG plot
                                                   div(class = "plot-container",
                                                       div(class = "plot-area",
                                                           plotlyOutput("plot2PCAWG")
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
                                       selected = "consolidated"),

                          # Smoothing factor controls for Driver Mutation
                          conditionalPanel(
                            condition = "output.driverRowSelected == true",
                            sliderInput("smoothingFactor1_driver", "Smoothing Factor (VAF Plot):",
                                        min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                            div(class = "recommended-smoothing",
                                uiOutput("smoothingExplanation1_driver")
                            ),
                            sliderInput("smoothingFactor2_driver", "Smoothing Factor (CP Plot):",
                                        min = 0.2, max = 0.7, value = 0.45, width = "100%"),
                            div(class = "recommended-smoothing",
                                uiOutput("smoothingExplanation2_driver")
                            )
                          )
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
                              div(class = "caption-area",
                                  hidden(uiOutput("driverCaption1"))
                              )
                          ),
                          
                          # Driver plot 2
                          div(class = "plot-container",
                              div(class = "plot-area",
                                  plotlyOutput("driverPlot2")
                              ),
                              div(class = "caption-area",
                                  hidden(uiOutput("driverCaption2"))
                              )
                          )
                        )
                      )
             ),
             
             tabPanel("Data Exploration",
                      tags$div(class = "data-exploration-main",
                               tabsetPanel(id = "analysis_tabset",
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
                                                               
                                                               # Analysis Results (only show when plot exists)
                                                               conditionalPanel(
                                                                 condition = "output.survivalPlotGenerated == true",
                                                                 tags$div(class = "analysis-summary",
                                                                          uiOutput("survivalPlotInfo")
                                                                 )
                                                               ),
                                                               
                                                               # Sample Information (only show when plot exists)
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
                                                      
                                                      # Instructions panel
                                                      tags$div(
                                                        class = "instructions-card",
                                                        tags$h5(tags$b("Analysis Instructions"), style = "color: #2c3e50; margin-bottom: 15px;"),
                                                        tags$div(
                                                          style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-top: 10px;",
                                                          tags$div(
                                                            tags$p("1. All cohorts (TCGA, OCCAMS, mCRPC) are included", style = "margin: 5px 0;"),
                                                            tags$p("2. Filter TCGA cancer types (optional)", style = "margin: 5px 0;")
                                                          ),
                                                          tags$div(
                                                            tags$p("3. Toggle correlation values display", style = "margin: 5px 0;"),
                                                            tags$p("4. View correlation patterns and summary", style = "margin: 5px 0;")
                                                          )
                                                        )
                                                      ),
                                                      
                                                      # Controls and Plot Layout
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
                                                               ),
                                                               
                                                               # Legend Panel
                                                               tags$div(class = "legend-panel",
                                                                        tags$h5(tags$b("Legend & Interpretation")),
                                                                        tags$div(class = "legend-item",
                                                                                 tags$div(class = "legend-symbol", style = "background: linear-gradient(90deg, blue, white, red);"),
                                                                                 tags$div(class = "legend-text", "Correlation scale: -1 (blue) to +1 (red)")
                                                                        ),
                                                                        tags$div(class = "legend-item",
                                                                                 tags$div(class = "legend-symbol", style = "background: #3498db;"),
                                                                                 tags$div(class = "legend-text", "TMB: Tumor Mutational Burden")
                                                                        ),
                                                                        tags$div(class = "legend-item",
                                                                                 tags$div(class = "legend-symbol", style = "background: #e74c3c;"),
                                                                                 tags$div(class = "legend-text", "NRPCC: Non-reference ploidy corrected copy number")
                                                                        ),
                                                                        tags$div(class = "legend-item",
                                                                                 tags$div(class = "legend-symbol", style = "background: #27ae60;"),
                                                                                 tags$div(class = "legend-text", "Purity: Tumor purity estimates")
                                                                        )
                                                               )
                                                        ),
                                                        
                                                        column(9,
                                                               # Plot Section
                                                               tags$div(class = "plot-section",
                                                                        plotOutput("heatmap_plot", height = "600px")
                                                               ),
                                                               
                                                               # Summary Statistics Table (only show when heatmap exists)
                                                               conditionalPanel(
                                                                 condition = "output.heatmapGenerated == true",
                                                                 tags$div(class = "correlation-table-container",
                                                                          tags$h4(tags$b("Summary Statistics"), style = "color: #2c3e50; margin-bottom: 15px;"),
                                                                          DT::DTOutput("heatmap_summary_table")
                                                                 )
                                                               ),
                                                               
                                                               # Additional Analysis Information (only show when heatmap exists)
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
             
             tabPanel("Downloads",
                      tags$div(class = "downloads-tab-container",
                               tags$div(id = "downloads-vertical-nav", class = "vertical-nav-floating",
                                        tags$h5("Navigate", style = "margin-bottom: 15px; font-weight: 600;"),
                                        tags$ul(class = "nav-list",
                                                tags$li(tags$a(href = "#overview-section", class = "nav-link", "Overview")),
                                                tags$li(tags$a(href = "#simulation-section", class = "nav-link", "Simulation Data")),
                                                tags$li(tags$a(href = "#tcga-section", class = "nav-link", "TCGA Data")),
                                                tags$li(tags$a(href = "#pcawg-section", class = "nav-link", "PCAWG Data"))
                                        )
                               ),
                               
                               tags$div(class = "downloads-main-content",
                                        div(id = "overview-section", class = "download-section",
                                            tags$h3(tags$b("Overview"), style = "color: #3498db; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                                            p(tags$b("CliPP (Clonal structure Identification through Penalizing Pairwise differences)"),
                                              " provides comprehensive datasets for cancer genomic analysis. Download processed data from major cancer genomic consortiums and simulation data:"),
                                            tags$ul(
                                              tags$li(tags$b("Simulation (CliPPSim4k)"), " - Simulated cancer genomic data for testing and validation"),
                                              tags$li(tags$b("TCGA (The Cancer Genome Atlas)"), " - Multi-platform genomic data"),
                                              tags$li(tags$b("PCAWG (Pan-Cancer Analysis of Whole Genomes)"), " - Whole genome sequencing data")
                                            ),
                                            p("All datasets include clonal structure analysis results, mutation assignments, and sample metadata."),

                                            div(class = "metadata-card", style = "background: #e1f5fe; border-left: 4px solid #0288d1;",
                                                tags$b("🔐 Authentication Required:"),
                                                p("These downloads require password authentication. When you click a download button:"),
                                                tags$ul(
                                                  tags$li("The link will open in your default browser"),
                                                  tags$li("A notification with the password will appear in this app"),
                                                  tags$li("Enter the password when prompted by the website")
                                                ),
                                                p(tags$em("Note: Password is only provided when you attempt to download."))
                                            ),

                                            div(class = "metadata-card", style = "background: #fff3cd; border-left: 4px solid #ffc107;",
                                                tags$b("Access Issues?"),
                                                p("If you encounter problems, contact ",
                                                  a(href = "mailto:aw80@rice.edu", "aw80@rice.edu"),
                                                  " or ",
                                                  a(href = "mailto:qmtran@mdanderson.org", "qmtran@mdanderson.org"),
                                                  " for assistance.")
                                            )
                                        ),
                                        
                                        # Simulation section
                                        div(id = "simulation-section", class = "download-section",
                                            tags$h3(tags$b("Simulation Dataset Downloads (CliPPSim4k)"), style = "color: #e74c3c; border-bottom: 2px solid #e74c3c; padding-bottom: 10px;"),
                                            
                                            div(class = "metadata-card",
                                                tags$b("Dataset Information:"),
                                                tags$ul(
                                                  tags$li("Sample count: 4,000 simulated cancer genomes"),
                                                  tags$li("Simulation types: Various clonal architectures and mutation patterns"),
                                                  tags$li("Data processing: CliPP clonal structure analysis"),
                                                  tags$li("File formats: TSV, TXT")
                                                )
                                            ),
                                            
                                            tags$table(class = "download-table-styled",
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
                                                           tags$td(class = "dataset-name", "CliPPSim4k Summary"),
                                                           tags$td("Comprehensive simulation results for 4,000 samples with known ground truth"),
                                                           tags$td(tags$span(class = "file-type-badge", "TSV")),
                                                           tags$td(
                                                             actionButton("downloadSim4k", "Download", class = "download-btn"),
                                                             tags$span(class = "file-size", " (~2MB)")
                                                           )
                                                         ),
                                                         tags$tr(
                                                           tags$td(class = "dataset-name", "Individual Simulation Data"),
                                                           tags$td("Per-sample simulation files with ground truth clonal structure"),
                                                           tags$td(tags$span(class = "file-type-badge", "TXT Files")),
                                                           tags$td(actionButton("browseSim4k", "Browse Data", class = "browse-btn"))
                                                         )
                                                       )
                                            )
                                        ),
                                        
                                        # TCGA section
                                        div(id = "tcga-section", class = "download-section",
                                            tags$h3(tags$b("TCGA Dataset Downloads"), style = "color: #9b59b6; border-bottom: 2px solid #9b59b6; padding-bottom: 10px;"),
                                            
                                            div(class = "metadata-card",
                                                tags$b("Dataset Information:"),
                                                tags$ul(
                                                  tags$li("Sample count: ~10,000+ cancer samples"),
                                                  tags$li("Cancer types: 33 cancer types"),
                                                  tags$li("Data types: Multi-platform genomic data with CliPP analysis"),
                                                  tags$li("File formats: TSV, TXT")
                                                )
                                            ),
                                            
                                            tags$table(class = "download-table-styled",
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
                                                           tags$td(class = "dataset-name", "CliPP TCGA Summary"),
                                                           tags$td("Comprehensive CliPP analysis results for all TCGA samples"),
                                                           tags$td(tags$span(class = "file-type-badge", "TSV")),
                                                           tags$td(
                                                             actionButton("downloadTCGA", "Download", class = "download-btn"),
                                                             tags$span(class = "file-size", " (~5MB)")
                                                           )
                                                         ),
                                                         tags$tr(
                                                           tags$td(class = "dataset-name", "Individual Sample Data"),
                                                           tags$td("Per-sample mutation assignments and clonal structure"),
                                                           tags$td(tags$span(class = "file-type-badge", "TXT Files")),
                                                           tags$td(actionButton("browseTCGA", "Browse Data", class = "browse-btn"))
                                                         )
                                                       )
                                            )
                                        ),
                                        
                                        # PCAWG section
                                        div(id = "pcawg-section", class = "download-section",
                                            tags$h3(tags$b("PCAWG Dataset Downloads"), style = "color: #27ae60; border-bottom: 2px solid #27ae60; padding-bottom: 10px;"),
                                            
                                            div(class = "metadata-card",
                                                tags$b("Dataset Information:"),
                                                tags$ul(
                                                  tags$li("Sample count: ~2,600 cancer whole genomes"),
                                                  tags$li("Cancer types: 38 tumor types across multiple sites"),
                                                  tags$li("Data processing: CliPP clonal structure analysis with multiple λ parameters"),
                                                  tags$li("File formats: TSV, TXT")
                                                )
                                            ),
                                            
                                            tags$table(class = "download-table-styled",
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
                                                           tags$td(class = "dataset-name", "CliPP PCAWG Summary"),
                                                           tags$td("Comprehensive CliPP analysis results for all PCAWG samples"),
                                                           tags$td(tags$span(class = "file-type-badge", "TSV")),
                                                           tags$td(
                                                             actionButton("downloadPCAWG", "Download", class = "download-btn"),
                                                             tags$span(class = "file-size", " (~2.5MB)")
                                                           )
                                                         ),
                                                         tags$tr(
                                                           tags$td(class = "dataset-name", "Individual Sample Data"),
                                                           tags$td("Per-sample mutation assignments and clonal structure"),
                                                           tags$td(tags$span(class = "file-type-badge", "TXT Files")),
                                                           tags$td(actionButton("browsePCAWG", "Browse Data", class = "browse-btn"))
                                                         )
                                                       )
                                            )
                                        ),
                                        
                                        # Usage and Citation
                                        div(class = "citation-section",
                                            tags$h4(tags$b("Data Usage & Citation"), style = "color: #2c3e50; margin-top: 40px;"),
                                            p(tags$b("If you use this data in your research, please cite:")),
                                            tags$ul(
                                              tags$li("CliPP methodology and software publication"),
                                              tags$li("PCAWG Consortium publications for PCAWG data"),
                                              tags$li("TCGA Research Network publications for TCGA data")
                                            ),
                                            p(tags$b("Data Usage:"), " All data is provided for research purposes. Please follow the data usage policies of the respective consortiums."),
                                            p(tags$b("Last Updated:"), " September 2025"),
                                            p(tags$b("Contact:"), " For questions about the datasets, please contact the development team via the About page.")
                                        )
                               )
                      )
             ),
             
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
                        
                      )
             )
  ))
