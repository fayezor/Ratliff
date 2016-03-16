if(!require(shiny)){
  install.packages("shiny")
}
if(!require(ggplot2)){
  install.packages("ggplot2")
}
if(!require(DT)){
  install.packages("DT")
}
require(shiny)
require(ggplot2)
require(DT)

shinyUI(navbarPage("Ratliff scRNA-Seq Prostate Data",
                   
                   ##### ----------------------------------------------- 
                   ##### DE RESULTS TAB, CONTROL V. KNOCKDOWN
                   ##### -----------------------------------------------
                   
                   tabPanel("DE Results",
                            sidebarLayout(
                              sidebarPanel(
                                h2("Differential Expression Results"), 
                                helpText("Results for the differential expression analysis of 10,854 genes between Control vs. Knockdown cells."),
                                br(),
                                helpText("Tested via the R/BioConductor package edgeR. Genes with average counts < 5 across all replicates were not tested for differential expression, and do not appear in the table."),
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
                   ),
                   
                   ##### ----------------------------------------------- 
                   ##### DE RESULTS TAB, ZERO VS. NONZERO
                   ##### -----------------------------------------------
                   
                   tabPanel("Zero v. Nonzero",
                            sidebarLayout(
                              sidebarPanel(
                                h2("Differential Expression Results"), 
                                helpText("Results for the differential expression analysis of 10,854 genes between groups of cells, defined by Zero vs. Nonzero SULT2B1b expression. There were 352 cells in the Zero group and 47 in the Nonzero group."),
                                br(),
                                helpText("Tested via the R/BioConductor package edgeR. Genes with average counts < 5 across all replicates were not tested for differential expression, and do not appear in the table."),
                                strong("Filter gene results by:"),
                                selectInput("filterDEtable_zerovnonzero", 
                                            label = "",
                                            choices = list("All genes" = 1, 
                                                           "Differentially expressed" = 2,
                                                           "Significantly upregulated in Zero" = 3,
                                                           "Significantly downregulated in Nonzero" = 4,
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
                                dataTableOutput('mytable_zerovnonzero')
                              )
                            )
                   ),
                   
                   ##### ----------------------------------------------- 
                   ##### VENN DIAGRAM TAB
                   ##### -----------------------------------------------
                   
                   tabPanel("Venn Diagram",
                            sidebarLayout(
                              sidebarPanel(
                                br(),
                                helpText("The Venn diagram to the right shows the overlap in genes found to be differentially expressed in the two comparisons, control vs. knockdown and zero vs. nonzero. Use the pull-down menu below to view the list of genes in each area of the diagram."),
                                strong("Filter gene list by:"),
                                selectInput("filterVenn", 
                                            label = "",
                                            choices = list("C-KD only"=1,
                                                           "Zero-Nonzero only"=2,
                                                           "C-KD and Zero-Nonzero"=3),
                                            selected=1),
                                submitButton("Submit")
                              ),
                            mainPanel(
                              br(),
                              br(),
                              img(src='venn.jpeg', align = "center"),
                              br(),
                              br(),
                              dataTableOutput('vennTable')
                            )
                            )),
                   
                   ##### ----------------------------------------------- 
                   ##### VIOLIN PLOTS TAB
                   ##### -----------------------------------------------
                   
                   tabPanel("Violin Plots",
                            sidebarLayout(
                              
                              sidebarPanel(
                                h2("Violin Plots"),
                                helpText("Violin plots depict expression of a given gene in Control vs. Knockdown cells. Note that the horizontal range of the points across the x-axis is simply a graphical jitter; it is meant for visual clarity and has no further meaning."),
                                textInput("geneID", label = h5("Enter Ensembl ID for gene of interest"), value = "e.g. ENSG..."),
                                checkboxInput("log", "Plot gene expression on log scale", 
                                              value = FALSE),
                                submitButton("Submit"),
                                br(),
                                helpText("Don't remember the gene ID? Check out the 'DE Results' tab to look up the associated Ensembl ID(s) for a given gene name.")
                                # selectInput("genename", label=NULL, c("Gene name"="", mart.data$GeneName[order(mart.data$GeneName)]), selectize=TRUE),                                     verbatimTextOutput('genenameout')
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
                   
                   ##### ----------------------------------------------- 
                   ##### CORRELATION PLOTS TAB
                   ##### -----------------------------------------------
                   
                   tabPanel("Correlation Plots",
                            sidebarLayout(
                              
                              sidebarPanel(
                                h2("Correlation Plots"),
                                helpText("These plots depict the correlation between the expression values of two given genes."),
                                textInput("XaxisGeneID", label = h5("Enter Ensembl ID for gene to be plotted on x-axis"), value = "e.g. ENSG..."),
                                textInput("yaxisGeneID", label = h5("Enter Ensembl ID for gene to be plotted on y-axis"), value = "e.g. ENSG..."),
                                checkboxInput("log2", "Plot gene expression on log scale", 
                                              value = FALSE),
                                submitButton("Submit"),
                                br(),
                                helpText("Don't remember the gene ID? Check out the 'DE Results' tab to look up the associated Ensembl ID(s) for a given gene name.")
                              ),
                              
                              mainPanel(
                                # Show a plot of the generated distribution
                                plotOutput("correlationPlot"),
                                # Report Correlation between gene expression values of the two selected genes
                                strong("Correlation of Gene Expression Values"),
                                textOutput("correlations")
                              )
                            )
                            )
                   ))

#dataTableOutput('marttable')
