library("shiny")

shinyUI(navbarPage("Plots for Ratliff scRNA-Seq Prostate Data",
                   
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
                            )
                   ))