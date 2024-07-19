library(shiny)
library(plotly)
library(shinyjs)

ui <- fluidPage(
  shinyjs::useShinyjs(), 
  
  # Title and copyright page important to display for research purposes
  titlePanel("CliPP-on-Web: Clonal structure identification through penalizing pairwise differences"),
  div(p("Copyright ", HTML("&copy;"), " 2024 Wang Lab at MD Anderson. All rights reserved.")),
  
  # New formatting splits into 4 pages: about section which has all of the descriptions of the process Wenyi pioneered, CliPP-on-Web used for clinicians to determine subclonal information
  # CliPP Data Resources that loads PCAWG and TCGA directly from a folder for automatic information, and a citations for all of the users on the web
  
  # Page 1: About
  navbarPage("",
             tabPanel("About",
                      tags$div(
                        tags$h4(tags$b(tags$span("Description"))),
                        p("Subpopulations of tumor cells characterized by mutation profiles may confer differential fitness 
                          and consequently influence prognosis of cancers. Understanding subclonal architecture has the potential 
                          to provide biological insight in tumor evolution and advance precision cancer treatment. Recent methods 
                          comprehensively integrate single nucleotide variants (SNVs) and copy number aberrations (CNAs) to 
                          reconstruct subclonal architecture using whole-genome or whole-exome sequencing (WGS, WES) data 
                          from bulk tumor samples. However, the commonly used Bayesian methods require a large amount of 
                          computational resources, a prior knowledge of the number of subclones, and extensive post-processing. 
                          Regularized likelihood modeling approach, never explored for subclonal reconstruction, can inherently 
                          address these drawbacks. We therefore propose a model-based method, Clonal structure identification 
                          through Pair-wise Penalization, or CliPP, for clustering subclonal mutations without prior knowledge 
                          or post-processing. The CliPP model is applicable to genomic regions with or without CNAs. CliPP 
                          demonstrates high accuracy in subclonal reconstruction through extensive simulation studies. 
                          A penalized likelihood framework for subclonal reconstruction will help address intrinsic drawbacks 
                          of existing methods and expand the scope of computational analysis for cancer evolution in large 
                          cancer genomic studies. Also see our paper:",
                          a(href = "https://www.biorxiv.org/content/10.1101/2021.03.31.437383v1",
                            "https://www.biorxiv.org/content/10.1101/2021.03.31.437383v1"),"."),
                        tags$br(),
                        p(tags$b(tags$span(style = "color: #D22B2B;", "For issues with the app, please contact:"))),
                        p("Aaron Wu - ", a(href = "mailto:aw80@rice.edu", "aw80@rice.edu")),
                        p("Quang Tran - ", a(href = "mailto:qmtran@mdanderson.org", "qmtran@mdanderson.org"))
                      )
             ),
             
             # Page 2: CliPP-on-Web
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
                                               min = 0.2, max = 0.7, value = 0.5, step = 0.1)),
                            hidden(sliderInput("smoothingFactor2", "Smoothing Factor for CP Plot:",
                                               min = 0.2, max = 0.7, value = 0.5, step = 0.1)),
                            hidden(actionButton("clear", "Clear", class = "btn-danger"))
                          ),
                          
                          # This controls the production of plots and captions
                          mainPanel(
                            plotlyOutput("plot1"),
                            uiOutput("caption1"),
                            plotlyOutput("plot2"),
                            uiOutput("caption2"),
                            uiOutput("plotOutputArea"),
                            hidden(div(id = "plotReadyTextCliPP"))
                          )
                        )
                      )
             ),
             
             # Page 3: CliPP Data Resources
             # Automatically runs the analysis on the files in the folder and produces the plots
             tabPanel("CliPP Data Resources",
                      tags$div(
                        tabsetPanel(
                          # Type 1: TCGA
                          tabPanel("TCGA",
                                   tags$div(
                                     tags$h4(tags$b("TCGA Select")),
                                     sidebarLayout(
                                       sidebarPanel(
                                         uiOutput("cancer_type_ui_TCGA"),
                                         uiOutput("sample_name_ui_TCGA"),
                                         actionButton("submitTCGA", "Submit", class = "btn-primary"),
                                         hidden(selectInput("colorByTCGA", "Color points by:", choices = list("Clonality" = "clonality", "Read Depth" = "readDepth"), selected = "clonality")),
                                         hidden(sliderInput("smoothingFactor1_TCGA", "Smoothing Factor for VAF Plot:",
                                                            min = 0.2, max = 0.7, value = 0.5, step = 0.1)),
                                         hidden(sliderInput("smoothingFactor2_TCGA", "Smoothing Factor for CP Plot:",
                                                            min = 0.2, max = 0.7, value = 0.5, step = 0.1))
                                       ),
                                       mainPanel(
                                         plotlyOutput("plot1TCGA"),
                                         uiOutput("caption1TCGA"),
                                         plotlyOutput("plot2TCGA"),
                                         uiOutput("caption2TCGA"),
                                         hidden(uiOutput("plotOutputAreaTCGA")),
                                         hidden(div(id = "plotReadyTextTCGA"))
                                       )
                                     )
                                   )
                          ),
                          # Type 2: PCAWG
                          tabPanel("PCAWG",
                                   tags$div(
                                     tags$h4(tags$b("PCAWG Select")),
                                     sidebarLayout(
                                       sidebarPanel(
                                         uiOutput("cancer_type_ui_PCAWG"),
                                         uiOutput("sample_name_ui_PCAWG"),
                                         actionButton("submitPCAWG", "Submit", class = "btn-primary"),
                                         hidden(selectInput("colorByPCAWG", "Color points by:", choices = list("Clonality" = "clonality", "Read Depth" = "readDepth"), selected = "clonality")),
                                         hidden(sliderInput("smoothingFactor1_PCAWG", "Smoothing Factor for VAF Plot:",
                                                            min = 0.2, max = 0.7, value = 0.5, step = 0.1)),
                                         hidden(sliderInput("smoothingFactor2_PCAWG", "Smoothing Factor for CP Plot:",
                                                            min = 0.2, max = 0.7, value = 0.5, step = 0.1))
                                       ),
                                       mainPanel(
                                         plotlyOutput("plot1PCAWG"),
                                         uiOutput("caption1PCAWG"),
                                         plotlyOutput("plot2PCAWG"),
                                         uiOutput("caption2PCAWG"),
                                         hidden(uiOutput("plotOutputAreaPCAWG")),
                                         hidden(div(id = "plotReadyTextPCAWG"))
                                       )
                                     )
                                   )
                          )
                        )
                      )
             ),
             
             # Page 4: Driver Mutation
             tabPanel("Driver Mutation",
                      tags$div(
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput("cancer_type_ui_driver"),
                            uiOutput("sample_name_ui_driver"),
                            actionButton("submitDriver", "Submit", class = "btn-primary")
                          ),
                          mainPanel(
                            tableOutput("driverMutationTable")
                          )
                        )
                      )
             ),
             
             # Page 5: Citations
             tabPanel("Citations",
                      tags$div(
                        tags$h4(tags$b(tags$span("Manuscript under preparation:"))),
                        p("Yujie Jiang, Matthew D Montierth, Kaixian Yu, Shuangxi Ji, Shuai Guo, Quang Tran, Seung Jun Shin, Shaolong Cao, 
             Ruonan Li, Yuxin Tang, Tom Lesluyes, Scott Kopetz, Jaffer Ajani, Pavlos Msaouel, Sumit K Subudhi, Ana Aparicio, 
             Padmanee Sharma, John Paul Shen, Anil K. Sood, Maxime Tarabichi, Jennifer R. Wang, Marek Kimmel, Peter Van Loo, 
             Hongtu Zhu, Wenyi Wang. Pan-cancer subclonal mutation analysis of 7,827 tumors predicts clinical outcome."),
                        tags$h4(tags$b(tags$span("References:"))),
                        p("Martínez-Jiménez, Francisco, Ferran Muiños, Inés Sentís, Jordi Deu-Pons, Iker Reyes-Salazar, Claudia Arnedo-Pac, Loris Mularoni et al. 
             'A compendium of mutational cancer driver genes.' Nature Reviews Cancer 20, no. 10 (2020): 555-572.")
                      )
             )
  )
)
