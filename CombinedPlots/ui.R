library("shiny")
require("ggplot2")
require("DT")

shinyUI(navbarPage("Ratliff scRNA-Seq Prostate Data",
                   
                   ##### TAB FOR VIOLIN PLOTS #####
                   
                   tabPanel("Violin Plots",
                            sidebarLayout(
                              
                              sidebarPanel(
                                h2("Violin Plots"),
                                helpText("Violin plots depict expression of a given gene in Control vs. Knockdown cells. Note that the horizontal range of the points across the x-axis is simply a graphical jitter; it is meant for visual clarity and has no further meaning."),
                                textInput("geneID", label = h5("Enter Ensembl ID for gene of interest"), value = "e.g. ENSG..."),
                                checkboxInput("log", "Plot gene expression on log scale", 
                                              value = FALSE),
                                submitButton("Submit")
                              ),
                              
                              mainPanel(
                                # Show a plot of the generated distribution
                                plotOutput("violinPlot"),
                                # Report percentage of zeros in either group
                                strong("Percentage of zero counts"),
                                br(),
                                htmlOutput("zeros"),
                                br(),
                                strong("Differential expression results"),
                                br(),
                                htmlOutput("DEresults")
                              )
                            )
                            ),
                   
                   ##### TAB FOR CORRELATION PLOTS #####
                   
                   tabPanel("Correlation Plots",
                            sidebarLayout(
                              
                              sidebarPanel(
                                h2("Correlation Plots"),
                                helpText("These plots depict the correlation between the expression values of two given genes."),
                                textInput("XaxisGeneID", label = h5("Enter Ensembl ID for gene to be plotted on x-axis"), value = "e.g. ENSG..."),
                                textInput("yaxisGeneID", label = h5("Enter Ensembl ID for gene to be plotted on y-axis"), value = "e.g. ENSG..."),
                                checkboxInput("log2", "Plot gene expression on log scale", 
                                              value = FALSE),
                                submitButton("Submit")
                              ),
                              
                              mainPanel(
                                # Show a plot of the generated distribution
                                plotOutput("correlationPlot"),
                                # Report Correlation between gene expression values of the two selected genes
                                strong("Correlation of Gene Expression Values"),
                                textOutput("correlations")
                              )
                            )
                            ),
                   
                   tabPanel("DE Results",
                            sidebarLayout(
                              sidebarPanel(
                                h2("Differential Expression Results"), 
                                helpText("Results for the differential expression analysis of 10,854 genes between Control vs. Knockdown cells, as tested via the R/BioConductor package edgeR. Genes with average counts < 5 across all replicates were not tested for differential expression, and do not appear in the table."),
                                strong("Filter gene results by:"),
                                selectInput("filterDEtable", 
                                            label = "",
                                            choices = list("All genes" = 1, 
                                                           "Differentially expressed" = 2,
                                                           "Significantly upregulated in KD" = 3,
                                                           "Significantly downregulated in KD" = 4,
                                                           "Not differentially expressed" = 5), 
                                            selected = 2),
                                submitButton("Submit")
                              ),
                              mainPanel(
                                br(),
                                tags$ul(
                                  tags$li(strong("LogFC: "),"Log fold-change between knockdown and treatment groups"), 
                                  tags$li(strong("LogCPM: "), "Log counts-per-million"), 
                                  tags$li(strong("LR: "), "Likelihood ratio statistic for the differential expression test"),
                                  tags$li(strong("PValue: "), "Unadjusted p-value"),
                                  tags$li(strong("FDR: "), "P-value adjusted using the Benjamini-Hochberg procedure for false discovery rate control")
                                ),
                                br(),
                                br(),
                                dataTableOutput('mytable')
                              )
                            )
                            )
                   
                   ))