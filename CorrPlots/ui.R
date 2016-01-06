library("shiny")

# Define UI for application that draws a violin plot
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Correlation Gene Expression Plots"),
  
  # Sidebar with a text box input for selecting genes
  sidebarLayout(
    
    sidebarPanel(
      helpText("These plots depict the correlation between the expression vlues of two given genes."),
      textInput("XaxisGeneID", label = h5("Enter Ensembl ID for gene you would like on the x-axis"), value = "e.g. ENSG..."),
      textInput("yaxisGeneID", label = h5("Enter Ensembl ID for gene you would like on the y-axis"), value = "e.g. ENSG..."),
      checkboxInput("log", "Plot gene expression on log scale", 
                    value = FALSE),
      submitButton("Submit")
    ),
    
    mainPanel(
      
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