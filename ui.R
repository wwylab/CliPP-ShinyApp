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
                        tags$iframe(src="https://www.youtube.com/embed/ZPX_hC-uiYA",
                                    style="width:80%; height:700px; border:none;")
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
                                # Using conditionalPanel for smoothing controls
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
                                # Using conditionalPanel for smoothing controls
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
                            
                            # Plot output area and progress bar
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
                                                       # Using conditionalPanel for smoothing controls
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
                                                       # Using conditionalPanel for smoothing controls
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
                                                       # Using conditionalPanel for smoothing controls
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
                                                       # Using conditionalPanel for smoothing controls
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
                              # Using conditionalPanel for smoothing controls
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
                              # Using conditionalPanel for smoothing controls
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
             
             # Page 6: Citations for related studies and resources
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
                            "https://www.biorxiv.org/content/10.1101/2024.07.03.601939v1"),".")
                      )
             )
  ))
