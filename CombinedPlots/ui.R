library("shiny")

# Define UI for application that draws a violin plot
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Combined Plots"),
  
  # Sidebar with a text box input for selecting genes
  sidebarLayout(
    
    sidebarPanel(
      h2("Violin Plots"),
      helpText("Violin plots depict expression of a given gene in Control vs. Knockdown cells. Note that the horizontal range of the points across the x-axis is simply a graphical jitter; it is meant for visual clarity and has no further meaning."),
      textInput("geneID", label = h5("Enter Ensembl ID for gene of interest"), value = "e.g. ENSG..."),
      checkboxInput("log", "Plot gene expression on log scale", 
                    value = FALSE),
      submitButton("Submit"),
      br(),
      h2("Correlation Plots"), 
      helpText("These plots depict the correlation between the expression vlues of two given genes."),
      textInput("XaxisGeneID", label = h5("Enter Ensembl ID for gene you would like on the x-axis"), value = "e.g. ENSG..."),
      textInput("yaxisGeneID", label = h5("Enter Ensembl ID for gene you would like on the y-axis"), value = "e.g. ENSG..."),
      checkboxInput("log", "Plot gene expression on log scale", 
                    value = FALSE),
      submitButton("Submit")
    ),
    
    mainPanel(
      
      # Show a plot of the generated distribution
      plotOutput("violinPlot"),
      br(),
      br(),
      
      # Report percentage of zeros in either group
      h4("Percentage of zero counts"),
      textOutput("controlzeros"),
      textOutput("kdzeros"),
      
      # Show a plot of the generated distribution
      plotOutput("correlationPlot"),
      br(),
      br(),
      
      # Report Correlation between gene expression values of the two selected genes
      h4("Correlation of Gene Expression Values"),
      textOutput("correlations")
    )
  )
))